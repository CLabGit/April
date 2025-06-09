import os
import re
import torch
import torch.nn.functional as F
from collections import OrderedDict
from quantization.autoquant_utils import quantize_sequential, quantize_model
from quantization.base_quantized_classes import QuantizedActivation, FP32Acts
from quantization.base_quantized_model import QuantizedModel

from transformers.models.vit.modeling_vit import *
from transformers import ViTImageProcessor, ViTForImageClassification
from PIL import Image
import requests
import timm
from torchvision.models import vit_b_16

from typing import Dict, List, Optional, Set, Tuple, Union


class VisionTransformerForImageClassification(ViTForImageClassification):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        
    def forward(
        self,
        pixel_values: Optional[torch.Tensor] = None,
        head_mask: Optional[torch.Tensor] = None,
        labels: Optional[torch.Tensor] = None,
        output_attentions: Optional[bool] = None,
        output_hidden_states: Optional[bool] = None,
        interpolate_pos_encoding: Optional[bool] = None,
        return_dict: Optional[bool] = None,
    ):
        r"""
        labels (`torch.LongTensor` of shape `(batch_size,)`, *optional*):
            Labels for computing the image classification/regression loss. Indices should be in `[0, ...,
            config.num_labels - 1]`. If `config.num_labels == 1` a regression loss is computed (Mean-Square loss), If
            `config.num_labels > 1` a classification loss is computed (Cross-Entropy).
        """
        return_dict = return_dict if return_dict is not None else self.config.use_return_dict

        outputs = self.vit(
            pixel_values,
            head_mask=head_mask,
            output_attentions=output_attentions,
            output_hidden_states=output_hidden_states,
            interpolate_pos_encoding=interpolate_pos_encoding,
            return_dict=return_dict,
        )

        sequence_output = outputs[0]

        logits = self.classifier(sequence_output[:, 0, :])

        return logits

class QuantizedVitPatchEmbeddings(QuantizedActivation):
    def __init__(self, vit_patch_emb_orig, **quant_params):
        super().__init__(**quant_params)
        specials = {}
        
        self.projection = quantize_model(
            vit_patch_emb_orig.projection,
            specials=specials,
            **quant_params,
        )
        
        self.num_channels = vit_patch_emb_orig.num_channels
        self.image_size = vit_patch_emb_orig.image_size
        self.patch_size = vit_patch_emb_orig.patch_size
        self.num_patches = vit_patch_emb_orig.num_patches
        
    def forward(self, pixel_values: torch.Tensor, interpolate_pos_encoding: bool = False) -> torch.Tensor:
        batch_size, num_channels, height, width = pixel_values.shape
        if num_channels != self.num_channels:
            raise ValueError(
                "Make sure that the channel dimension of the pixel values match with the one set in the configuration."
                f" Expected {self.num_channels} but got {num_channels}."
            )
        if not interpolate_pos_encoding:
            if height != self.image_size[0] or width != self.image_size[1]:
                raise ValueError(
                    f"Input image size ({height}*{width}) doesn't match model"
                    f" ({self.image_size[0]}*{self.image_size[1]})."
                )
        embeddings = self.projection(pixel_values).flatten(2).transpose(1, 2)
        return self.quantize_activations(embeddings)

class QuantizedVitEmbeddings(QuantizedActivation):
    def __init__(self, vit_emb_orig, **quant_params):
        super().__init__(**quant_params)
        specials = {ViTPatchEmbeddings: QuantizedVitPatchEmbeddings}
        
        self.patch_embeddings = quantize_model(
            vit_emb_orig.patch_embeddings,
            specials=specials,
            **quant_params,
        )
        
        self.cls_token = vit_emb_orig.cls_token
        self.position_embeddings = vit_emb_orig.position_embeddings
        self.dropout = vit_emb_orig.dropout
        
    def forward(self, x):
        batch_size, num_channels, height, width = x.shape
        embeddings = self.patch_embeddings(x)

        # add the [CLS] token to the embedded patch tokens
        cls_tokens = self.cls_token.expand(batch_size, -1, -1)
        embeddings = torch.cat((cls_tokens, embeddings), dim=1)

        # add positional encoding to each token
        embeddings = embeddings + self.position_embeddings

        embeddings = self.dropout(embeddings)

        return self.quantize_activations(embeddings)

class QuantizedViTImmediate(QuantizedActivation):
    def __init__(self, vit_int_orig, **quant_params):
        super().__init__(**quant_params)
        specials = {}
        
        self.dense = quantize_model(
            vit_int_orig.dense,
            specials=specials,
            **quant_params,
        )
        
        self.intermediate_act_fn = vit_int_orig.intermediate_act_fn
    
    def forward(self, hidden_states: torch.Tensor) -> torch.Tensor:
        hidden_states = self.dense(hidden_states)
        hidden_states = self.intermediate_act_fn(hidden_states)

        return self.quantize_activations(hidden_states)

class QuantizedViTOutput(QuantizedActivation):
    def __init__(self, vit_out_orig, **quant_params):
        super().__init__(**quant_params)
        specials = {}
        
        self.dense = quantize_model(
            vit_out_orig.dense,
            specials=specials,
            **quant_params,
        )
        
        self.dropout = vit_out_orig.dropout
    
    def forward(self, hidden_states: torch.Tensor, input_tensor: torch.Tensor) -> torch.Tensor:
        hidden_states = self.dense(hidden_states)
        hidden_states = self.dropout(hidden_states)

        hidden_states = hidden_states + input_tensor

        return self.quantize_activations(hidden_states)   


class QuantizedViTSelfAttention(QuantizedActivation):
    def __init__(self, vit_self_attn_orig, **quant_params):
        super().__init__(**quant_params)
        specials = {}
        
        self.num_attention_heads = vit_self_attn_orig.num_attention_heads
        self.attention_head_size = vit_self_attn_orig.attention_head_size
        self.all_head_size = vit_self_attn_orig.all_head_size
        
        self.query = quantize_model(vit_self_attn_orig.query, **quant_params)
        self.key = quantize_model(vit_self_attn_orig.key, **quant_params)
        self.value = quantize_model(vit_self_attn_orig.value, **quant_params)
        self.dropout = vit_self_attn_orig.dropout
        
        self.training = vit_self_attn_orig.training
        self.attention_probs_dropout_prob = vit_self_attn_orig.attention_probs_dropout_prob

    def transpose_for_scores(self, x: torch.Tensor) -> torch.Tensor:
        new_x_shape = x.size()[:-1] + (self.num_attention_heads, self.attention_head_size)
        x = x.view(new_x_shape)
        return x.permute(0, 2, 1, 3)
    
    def forward(self, x):
        mixed_query_layer = self.query(x)
        
        key_layer = self.transpose_for_scores(self.key(x))
        value_layer = self.transpose_for_scores(self.value(x))
        query_layer = self.transpose_for_scores(mixed_query_layer)
        
        # context_layer = torch.nn.functional.scaled_dot_product_attention(
        #     query=query_layer, 
        #     key=key_layer,
        #     value=value_layer,
        #     attn_mask=None,
        #     dropout_p=self.attention_probs_dropout_prob if self.training else 0.0,
        #     is_causal=False,
        #     scale=None,
        # )
        
        scores = torch.matmul(query_layer, key_layer.transpose(-2, -1))
        head_dim = query_layer.size(-1)
        scaled_scores = scores / torch.sqrt(torch.tensor(head_dim, dtype=scores.dtype))
        attention_weights = F.softmax(scaled_scores, dim=-1)
        if self.training:
            attention_weights = F.dropout(attention_weights, p=self.attention_probs_dropout_prob)
        context_layer = torch.matmul(attention_weights, value_layer)
        
        context_layer = context_layer.permute(0, 2, 1, 3).contiguous()
        new_context_layer_shape = context_layer.size()[:-2] + (self.all_head_size,)
        context_layer = context_layer.view(new_context_layer_shape)
        return self.quantize_activations(context_layer)
            

class QuantizedViTSelfOutput(QuantizedActivation):
    def __init__(self, vit_self_out_orig, **quant_params):
        super().__init__(**quant_params)
        specials = {}
        
        self.dense = quantize_model(
            vit_self_out_orig.dense,
            specials=specials,
            **quant_params,
        )
        self.dropout = vit_self_out_orig.dropout
        
    def forward(self, x):
        x = self.dense(x)
        x = self.dropout(x)

        return x

class QuantizedViTSdpaAttention(QuantizedActivation):
    def __init__(self, vit_sdpa_attn_orig, **quant_params):
        super().__init__(**quant_params)
        specials = {ViTSdpaSelfAttention: QuantizedViTSelfAttention, ViTSelfOutput: QuantizedViTSelfOutput}
        
        self.attention = quantize_model(
            vit_sdpa_attn_orig.attention,
            specials=specials,
            **quant_params,
        )
        self.output = quantize_model(
            vit_sdpa_attn_orig.output,
            specials=specials,  
            **quant_params,
        )
        # self.output = vit_sdpa_attn_orig.output
        
    def forward(self, x):
        self_output = self.attention(x)
        attention_output = self.output(self_output)
        return attention_output

class QuantizedViTLayer(QuantizedActivation):
    def __init__(self, vit_layer_orig, **quant_params):
        super().__init__(**quant_params)
        specials = {ViTIntermediate: QuantizedViTImmediate, ViTOutput: QuantizedViTOutput, ViTSdpaAttention: QuantizedViTSdpaAttention}
        
        self.intermediate = quantize_model(
            vit_layer_orig.intermediate,
            specials=specials,
            **quant_params,
        )
        self.attention = quantize_model(
            vit_layer_orig.attention,
            specials=specials,
            **quant_params,
        )
        self.output = quantize_model(
            vit_layer_orig.output,
            specials=specials,
            **quant_params,
        )
        self.layernorm_before = quantize_model(vit_layer_orig.layernorm_before, **quant_params)
        self.layernorm_after = quantize_model(vit_layer_orig.layernorm_after, **quant_params)
        # self.attention = vit_layer_orig.attention

    def forward(
        self,
        hidden_states: torch.Tensor,
        head_mask: Optional[torch.Tensor] = None,
        output_attentions: bool = False,
    ) -> Union[Tuple[torch.Tensor, torch.Tensor], Tuple[torch.Tensor]]:
        
        attention_output = self.attention(
            self.layernorm_before(hidden_states),  # in ViT, layernorm is applied before self-attention
        )

        # first residual connection
        hidden_states = attention_output + hidden_states
        hidden_states = self.quantize_activations(hidden_states)

        # in ViT, layernorm is also applied after self-attention
        layer_output = self.layernorm_after(hidden_states)
        layer_output = self.intermediate(layer_output)

        # second residual connection is done here
        layer_output = self.output(layer_output, hidden_states) # alread quantized in self.output

        return layer_output

class QuantizedViTEncoder(QuantizedActivation):
    def __init__(self, vit_enc_orig, **quant_params):
        super().__init__(**quant_params)
        specials = {ViTLayer: QuantizedViTLayer}
        
        self.layer = quantize_model(
            vit_enc_orig.layer,
            specials=specials,
            **quant_params,
        )
        
        self.gradient_checkpointing = vit_enc_orig.gradient_checkpointing
        
    def forward(
        self,
        hidden_states: torch.Tensor,
    ):
        for i, layer_module in enumerate(self.layer):
            hidden_states = layer_module(hidden_states)

        return self.quantize_activations(hidden_states)

        
class QuantizedViTPooler(QuantizedActivation):
    def __init__(self, vit_pol_orig, **quant_params):
        super().__init__(**quant_params)
        specials = {}
        
        self.dense = quantize_model(
            vit_pol_orig.dense,
            specials=specials,
            **quant_params,
        )
        
        
    def forward(self, hidden_states):
        # We "pool" the model by simply taking the hidden state corresponding
        # to the first token.
        first_token_tensor = hidden_states[:, 0]
        pooled_output = self.dense(first_token_tensor)
        pooled_output = self.activation(pooled_output)
        return self.quantize_activations(pooled_output)

class QuantizedViTModel(QuantizedActivation):
    def __init__(self, vit_mod_orig, **quant_params):
        super().__init__(**quant_params)
        specials = {ViTEmbeddings: QuantizedVitEmbeddings, ViTEncoder: QuantizedViTEncoder}

        self.embeddings = quantize_model(
            vit_mod_orig.embeddings,
            specials=specials, 
            **quant_params,
        )
        self.encoder = quantize_model(
            vit_mod_orig.encoder,
            specials=specials,
            **quant_params,
        )
        self.layernorm = quantize_model(vit_mod_orig.layernorm, **quant_params)
        # if self.pooler is not None:
        #     self.pooler = quantize_model(
        #         vit_mod_orig.pooler,
        #         specials=specials,
        #         **quant_params,
        #     ) 
        # else:
        #     self.pooler = None
            
    def forward(self, x):
        embedding_output = self.embeddings(x)
        encoder_outputs = self.encoder(embedding_output)
        sequence_output = encoder_outputs
        sequence_output = self.layernorm(sequence_output)
        # pooled_output = self.pooler(sequence_output) if self.pooler is not None else None
        head_outputs = sequence_output
        
        # return self.quantize_activations(head_outputs)
        return head_outputs
        

class QuantizedVisionTransformerForImageClassification(QuantizedModel):
    def __init__(self, model_fp, input_size=(1, 3, 224, 224), quant_setup=None, **quant_params):
        super().__init__(input_size)
        specials = {ViTModel: QuantizedViTModel}
        # quantize and copy parts from original model
        quantize_input = quant_setup and quant_setup == "LSQ_paper"
        self.vit = quantize_model(
            model_fp.vit, 
            tie_activation_quantizers=not quantize_input,
            specials=specials, 
            **quant_params,
        )
        
        self.classifier = quantize_model(model_fp.classifier, **quant_params)
        
        
    def forward(self, x):
        outputs = self.vit(x)
        sequence_output = outputs
        logits = self.classifier(sequence_output[:, 0, :])
        
        return logits


def vit_quantized(pretrained=True, model_dir=None, load_type="fp32", **qparams):
    fp_model = VisionTransformerForImageClassification.from_pretrained('google/vit-base-patch16-224')
    quant_model = QuantizedVisionTransformerForImageClassification(fp_model, **qparams)
    # fp_model = timm.create_model('vit_base_patch16_224', pretrained=True)
    # fp_model = vit_b_16(pretrained=True)
    # if pretrained and load_type == "fp32":
    return quant_model
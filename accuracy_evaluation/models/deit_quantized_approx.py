import os
import re
import torch
import torch.nn.functional as F
from collections import OrderedDict
from approx.replace_operations_with_approx_ops import quantize_sequential, Flattener, quantize_model, BNQConv
from quantization.base_quantized_classes import QuantizedActivation, FP32Acts
from quantization.base_quantized_model import QuantizedModel
from quantization.quantization_manager import QuantizationManager

from transformers.models.vit.modeling_vit import *
from transformers import ViTImageProcessor, ViTForImageClassification
# from PIL import Image
# import requests
# import timm
# from torchvision.models import vit_b_16

from typing import Dict, List, Optional, Set, Tuple, Union

from approx.approx_matmul_whole_v9 import *
# from approx.approx_matmul_whole_v11 import *


class DistilledVisionTransformerForImageClassification(ViTForImageClassification):
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
        # self.attention_probs_dropout_prob = vit_self_attn_orig.attention_probs_dropout_prob

        self.q_quantizer = QuantizationManager(
            qmethod=self.act_method,
            init=self.act_range_method,
            qparams=self.act_qparams,
            range_estim_params=self.act_range_options,
        )
        self.k_quantizer = QuantizationManager(
            qmethod=self.act_method,
            init=self.act_range_method,
            qparams=self.act_qparams,
            range_estim_params=self.act_range_options,
        )
        self.v_quantizer = QuantizationManager(
            qmethod=self.act_method,
            init=self.act_range_method,
            qparams=self.act_qparams,
            range_estim_params=self.act_range_options,
        )
        self.scores_quantizer = QuantizationManager(
            qmethod=self.act_method,
            init=self.act_range_method,
            qparams=self.act_qparams,
            range_estim_params=self.act_range_options,
        )
        self.attention_weights_quantizer = QuantizationManager(
            qmethod=self.act_method,
            init=self.act_range_method,
            qparams=self.act_qparams,
            range_estim_params=self.act_range_options,
        )
        self.context_layer_quantizer = QuantizationManager(
            qmethod=self.act_method,
            init=self.act_range_method,
            qparams=self.act_qparams,
            range_estim_params=self.act_range_options,
        )
        
    def transpose_for_scores(self, x: torch.Tensor) -> torch.Tensor:
        new_x_shape = x.size()[:-1] + (self.num_attention_heads, self.attention_head_size)
        x = x.view(new_x_shape)
        return x.permute(0, 2, 1, 3)
    
    def approx_multiply(self, x, y, x_bias, y_bias, res_bias):
        if self.approx_flag:
            expo_width = self.custom_approx_params['expo_width']
            mant_width = self.custom_approx_params['mant_width']
            dnsmp_factor = self.custom_approx_params['dnsmp_factor']
            withComp = self.custom_approx_params['withComp']
            # v9
            with_approx = self.custom_approx_params['with_approx']
            with_s2nn2s_opt = self.custom_approx_params['with_s2nn2s_opt']
            sim_hw_add_OFUF = self.custom_approx_params['sim_hw_add_OFUF']
            with_OF_opt = self.custom_approx_params['with_OF_opt']
            with_UF_opt = self.custom_approx_params['with_UF_opt']
            golden_clip_OF = self.custom_approx_params['golden_clip_OF']
            quant_btw_mult_accu = self.custom_approx_params['quant_btw_mult_accu']
            debug_mode = self.custom_approx_params['debug_mode']
            self_check_mode = self.custom_approx_params['self_check_mode']
            # v11
            test_golden = self.custom_approx_params['test_golden']
            test_golden_quant_btw_mult_accu = self.custom_approx_params['test_golden_quant_btw_mult_accu']
            test_golden_quant_btw_mult_accu_use_flexbias = self.custom_approx_params['test_golden_quant_btw_mult_accu_use_flexbias']
            test_baseline = self.custom_approx_params['test_baseline']
            test_best_allnorm = self.custom_approx_params['test_best_allnorm']
            test_best_s2nn2s = self.custom_approx_params['test_best_s2nn2s']
            test_casestudy = self.custom_approx_params['test_casestudy']
            with_compensation = self.custom_approx_params['with_compensation']
            with_view_all_as_norm = self.custom_approx_params['with_view_all_as_norm']
            with_flexbias = self.custom_approx_params['with_flexbias']
            # with_s2nn2s_opt = self.custom_approx_params['with_s2nn2s_opt']
            # debug_mode = self.custom_approx_params['debug_mode']
            # self_check_mode = self.custom_approx_params['self_check_mode']
            
            # comp_table_NN = get_comp_table_NN(expo_width, mant_width, withComp=withComp, dnsmp_factor=dnsmp_factor)
            comp_table_NN = get_error_table_NN(expo_width, mant_width, withComp=withComp, dnsmp_factor=dnsmp_factor)
            
            x_reshaped = x.reshape(-1, x.size(2), x.size(3))
            y_reshaped = y.reshape(-1, y.size(2), y.size(3))
            result_list = []
            for i in range(x_reshaped.size(0)):
                # v9
                result = custom_matmul_vectorize(x_reshaped[i], y_reshaped[i], expo_width, mant_width,
                                                    x_bias.item(), y_bias.item(), res_bias.item(), 
                                                    comp_table_NN,
                                                    with_approx=with_approx,
                                                    with_s2nn2s_opt=with_s2nn2s_opt,
                                                    sim_hw_add_OFUF=sim_hw_add_OFUF, with_OF_opt=with_OF_opt, with_UF_opt=with_UF_opt, golden_clip_OF=golden_clip_OF,
                                                    quant_btw_mult_accu=quant_btw_mult_accu,
                                                    debug_mode=debug_mode, self_check_mode=self_check_mode)
                # # v11
                # result = custom_matmul_vectorize(x_reshaped[i], y_reshaped[i], expo_width, mant_width,
                #                                     x_bias.item(), y_bias.item(), res_bias.item(), 
                #                                     comp_table_NN,
                #                                     test_golden=test_golden,
                #                                     test_golden_quant_btw_mult_accu=test_golden_quant_btw_mult_accu,
                #                                     test_golden_quant_btw_mult_accu_use_flexbias=test_golden_quant_btw_mult_accu_use_flexbias,
                #                                     test_baseline=test_baseline,
                #                                     test_best_allnorm=test_best_allnorm,
                #                                     test_best_s2nn2s=test_best_s2nn2s,
                #                                     test_casestudy=test_casestudy,
                #                                     with_compensation=with_compensation,
                #                                     with_view_all_as_norm=with_view_all_as_norm,
                #                                     with_flexbias=with_flexbias,
                #                                     with_s2nn2s_opt=with_s2nn2s_opt,
                #                                     debug_mode=debug_mode, self_check_mode=self_check_mode)
                result_list.append(result)
            result = torch.stack(result_list)
            result = result.reshape(x.size(0), x.size(1), x.size(2), y.size(3))
        else:
            result = torch.matmul(x, y)
        return result
    
    def run_forward_fix_ranges(self, query_layer, key_layer, value_layer):
        expo_width = self.custom_approx_params['expo_width']
        
        q_bias = self.q_quantizer.get_fp_bias()
        k_bias = self.k_quantizer.get_fp_bias()
        v_bias = self.v_quantizer.get_fp_bias()
        scores_bias = torch.tensor(2**(expo_width-1)) if self.scores_quantizer.get_fp_bias() is None else self.scores_quantizer.get_fp_bias()
        attention_weights_bias = torch.tensor(2**(expo_width-1)) if self.attention_weights_quantizer.get_fp_bias() is None else self.attention_weights_quantizer.get_fp_bias()
        context_layer_bias = torch.tensor(2**(expo_width-1)) if self.context_layer_quantizer.get_fp_bias() is None else self.context_layer_quantizer.get_fp_bias()
        
        scores = self.approx_multiply(query_layer, key_layer, q_bias, k_bias, scores_bias)
        
        head_dim = query_layer.size(-1)
        scaled_scores = scores / torch.sqrt(torch.tensor(head_dim, dtype=scores.dtype))
        attention_weights = F.softmax(scaled_scores, dim=-1)
        
        context_layer = self.approx_multiply(attention_weights, value_layer, attention_weights_bias, v_bias, context_layer_bias)
        
        return context_layer
        
    def run_forward(self, query_layer, key_layer, value_layer):
        expo_width = self.custom_approx_params['expo_width']
        
        q_bias = self.q_quantizer.get_fp_bias()
        k_bias = self.k_quantizer.get_fp_bias()
        v_bias = self.v_quantizer.get_fp_bias()
        scores_bias = torch.tensor(2**(expo_width-1)) if self.scores_quantizer.get_fp_bias() is None else self.scores_quantizer.get_fp_bias()
        attention_weights_bias = torch.tensor(2**(expo_width-1)) if self.attention_weights_quantizer.get_fp_bias() is None else self.attention_weights_quantizer.get_fp_bias()
        context_layer_bias = torch.tensor(2**(expo_width-1)) if self.context_layer_quantizer.get_fp_bias() is None else self.context_layer_quantizer.get_fp_bias()
        
        scores = self.approx_multiply(query_layer, key_layer, q_bias, k_bias, scores_bias)
        
        if self.quantize_input and self._quant_a and self.res_quantizer_flag:
            scores = self.scores_quantizer(scores)
        
        head_dim = query_layer.size(-1)
        scaled_scores = scores / torch.sqrt(torch.tensor(head_dim, dtype=scores.dtype))
        attention_weights = F.softmax(scaled_scores, dim=-1)
        
        if self.quantize_input and self._quant_a and self.res_quantizer_flag:
            attention_weights = self.attention_weights_quantizer(attention_weights)
        
        context_layer = self.approx_multiply(attention_weights, value_layer, attention_weights_bias, v_bias, context_layer_bias)
        
        if self.quantize_input and self._quant_a and self.res_quantizer_flag:
            context_layer = self.context_layer_quantizer(context_layer)
        
        return context_layer
    
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
        
        # approx self-attention
        # print(f"self.fix_ranges_flag: {self.fix_ranges_flag}, self.approx_flag: {self.approx_flag}")
        if self.quantize_input and self._quant_a:
            query_layer = self.q_quantizer(query_layer)
            key_layer = self.k_quantizer(key_layer)
            key_layer = key_layer.transpose(-2, -1)
            value_layer = self.v_quantizer(value_layer)
        
        if self.fix_ranges_flag == False or self.original_quantize_res:
            context_layer = self.run_forward(query_layer, key_layer, value_layer)
        
        if self.res_quantizer_flag and self.approx_flag:
            context_layer = self.run_forward_fix_ranges(query_layer, key_layer, value_layer)
        
        # print(f"query_layer: {query_layer.shape}, key_layer: {key_layer.transpose(-2, -1).shape}, value_layer: {value_layer.shape}, context_layer: {context_layer.shape}")
        
        context_layer = context_layer.permute(0, 2, 1, 3).contiguous()
        new_context_layer_shape = context_layer.size()[:-2] + (self.all_head_size,)
        context_layer = context_layer.view(new_context_layer_shape)
        if not self.quantize_input and self._quant_a:
            context_layer = self.quantize_activations(context_layer)
        return context_layer
            

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
        

class QuantizedDistilledVisionTransformerForImageClassification(QuantizedModel):
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

def deit_quantized_approx(pretrained=True, model_dir=None, load_type="fp32", **qparams):
    fp_model = DistilledVisionTransformerForImageClassification.from_pretrained('/home/zou/data/deit-tiny-patch16-224')
    quant_model = QuantizedDistilledVisionTransformerForImageClassification(fp_model, **qparams)
    return quant_model
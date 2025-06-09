import os
import re
import torch
import torch.nn.functional as F
from collections import OrderedDict
from approx.replace_operations_with_approx_ops import quantize_sequential, Flattener, quantize_model, BNQConv
from quantization.base_quantized_classes import QuantizedActivation, FP32Acts
from quantization.base_quantized_model import QuantizedModel
from quantization.quantization_manager import QuantizationManager


from models.fastvit import FastViT, RepCPE, RepMixerBlock, RepMixer
from timm.data import IMAGENET_DEFAULT_MEAN, IMAGENET_DEFAULT_STD
from timm.models.registry import register_model

from functools import partial

from timm.models import load_checkpoint

def _cfg(url="", **kwargs):
    return {
        "url": url,
        "num_classes": 1000,
        "input_size": (3, 256, 256),
        "pool_size": None,
        "crop_pct": 0.95,
        "interpolation": "bicubic",
        "mean": IMAGENET_DEFAULT_MEAN,  # (0.485, 0.456, 0.406)
        "std": IMAGENET_DEFAULT_STD,    # (0.229, 0.224, 0.225)
        "classifier": "head",
        **kwargs,
    }


default_cfgs = {
    "fastvit_t": _cfg(crop_pct=0.9),
    "fastvit_s": _cfg(crop_pct=0.9),
    "fastvit_m": _cfg(crop_pct=0.95),
}

class QuantizedRepMixer(QuantizedActivation):
    def __init__(self, fastvit_rep_mixer_orig, **quant_params):
        super().__init__(**quant_params)
        specials = {}
        self.dim = fastvit_rep_mixer_orig.dim
        self.kernel_size = fastvit_rep_mixer_orig.kernel_size
        self.inference_mode = fastvit_rep_mixer_orig.inference_mode
        
        if self.inference_mode:
            self.reparam_conv = quantize_model(
                fastvit_rep_mixer_orig.reparam_conv,
                specials=specials,
                **quant_params,
            )
        else:
            self.norm = quantize_model(
                fastvit_rep_mixer_orig.norm,
                specials=specials,
                **quant_params,
            )
            self.mixer = quantize_model(
                fastvit_rep_mixer_orig.mixer,
                specials=specials,
                **quant_params,
            )
            self.use_layer_scale = fastvit_rep_mixer_orig.use_layer_scale
            if self.use_layer_scale:
                self.layer_scale = fastvit_rep_mixer_orig.layer_scale
    
    def forward(self, x):
        if hasattr(self, "reparam_conv"):
            x = self.reparam_conv(x)
            return x
        else:
            if self.use_layer_scale:
                x = x + self.layer_scale * (self.mixer(x) - self.norm(x))
            else:
                x = x + self.mixer(x) - self.norm(x)
            return self.quantize_activations(x)

class QuantizedRepMixerBlock(QuantizedActivation):
    def __init__(self, fastvit_rep_mixer_block_orig, **quant_params):
        super().__init__(**quant_params)
        specials = {RepMixer: QuantizedRepMixer}
        self.token_mixer = quantize_model(
            fastvit_rep_mixer_block_orig.token_mixer,
            specials=specials,
            **quant_params,
        )
        self.mlp_hidden_dim = fastvit_rep_mixer_block_orig.mlp_hidden_dim
        self.convffn = quantize_model(
            fastvit_rep_mixer_block_orig.convffn,
            specials=specials,
            **quant_params,
        )
        self.drop_path = fastvit_rep_mixer_block_orig.drop_path
        
        self.use_layer_scale = fastvit_rep_mixer_block_orig.use_layer_scale
        if self.use_layer_scale:
            self.layer_scale = fastvit_rep_mixer_block_orig.layer_scale
    
    def forward(self, x):
        if self.use_layer_scale:
            x = self.token_mixer(x)
            x = x + self.drop_path(self.layer_scale * self.convffn(x))
            x = self.quantize_activations(x)
        else:
            x = self.token_mixer(x)
            x = x + self.drop_path(self.convffn(x))
            x = self.quantize_activations(x)
        return x
        
        

class QuantizedFastViT(QuantizedModel):
    def __init__(self, model_fp, input_size=(1, 3, 256, 256), quant_setup=None, **quant_params):
        super().__init__(input_size)
        specials = {RepMixerBlock: QuantizedRepMixerBlock}
        quantize_input = quant_setup and quant_setup == "LSQ_paper"
        self.patch_embed = quantize_model(
            model_fp.patch_embed,
            specials=specials,
            **quant_params,
        )
        
        self.network = quantize_model(
            model_fp.network,
            specials=specials,
            **quant_params,
        )
        
        self.gap = model_fp.gap
        
        self.conv_exp = quantize_model(
            model_fp.conv_exp,
            specials=specials,
            **quant_params,
        )
        
        self.head = quantize_model(
            model_fp.head,
            specials=specials,
            **quant_params,
        )
        
    def forward_embeddings(self, x: torch.Tensor) -> torch.Tensor:
        x = self.patch_embed(x)
        return x

    def forward_tokens(self, x: torch.Tensor) -> torch.Tensor:
        for idx, block in enumerate(self.network):
            x = block(x)
        # output only the features of last layer for image classification
        return x

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        # input embedding
        x = self.forward_embeddings(x)
        # through backbone
        x = self.forward_tokens(x)

        # for image classification
        x = self.conv_exp(x)
        x = self.gap(x)
        x = x.view(x.size(0), -1)
        cls_out = self.head(x)
        return cls_out



def fastvit_t8_quantized_approx(pretrained=True, model_dir=None, load_type="fp32", **qparams):
    """Instantiate FastViT-T8 model variant."""
    layers = [2, 2, 4, 2]
    embed_dims = [48, 96, 192, 384]
    mlp_ratios = [3, 3, 3, 3]
    downsamples = [True, True, True, True]
    token_mixers = ("repmixer", "repmixer", "repmixer", "repmixer")
    fp_model = FastViT(
        layers,
        token_mixers=token_mixers,
        embed_dims=embed_dims,
        mlp_ratios=mlp_ratios,
        downsamples=downsamples,
        inference_mode=True,
    )
    fp_model.default_cfg = default_cfgs["fastvit_t"]
    checkpoint = '/home/zou/codes/FP8-quantization/model_dir/fastvit_t8_reparam.pth.tar' # inference_mode=True
    # checkpoint = '/home/zou/codes/FP8-quantization/model_dir/fastvit_t8.pth.tar'       # inference_mode=False
    load_checkpoint(fp_model, checkpoint)
    quant_model = QuantizedFastViT(fp_model, **qparams)
    return quant_model


def fastvit_t12_quantized_approx(pretrained=True, model_dir=None, load_type="fp32", **qparams):
    """Instantiate FastViT-T12 model variant."""
    layers = [2, 2, 6, 2]
    embed_dims = [64, 128, 256, 512]
    mlp_ratios = [3, 3, 3, 3]
    downsamples = [True, True, True, True]
    token_mixers = ("repmixer", "repmixer", "repmixer", "repmixer")
    fp_model = FastViT(
        layers,
        token_mixers=token_mixers,
        embed_dims=embed_dims,
        mlp_ratios=mlp_ratios,
        downsamples=downsamples,
        inference_mode=True,
    )
    fp_model.default_cfg = default_cfgs["fastvit_t"]
    checkpoint = '/home/zou/codes/FP8-quantization/model_dir/fastvit_t12_reparam.pth.tar'
    load_checkpoint(fp_model, checkpoint)
    quant_model = QuantizedFastViT(fp_model, **qparams)
    return quant_model

def fastvit_s12_quantized_approx(pretrained=True, model_dir=None, load_type="fp32", **qparams):
    """Instantiate FastViT-S12 model variant."""
    layers = [2, 2, 6, 2]
    embed_dims = [64, 128, 256, 512]
    mlp_ratios = [4, 4, 4, 4]
    downsamples = [True, True, True, True]
    token_mixers = ("repmixer", "repmixer", "repmixer", "repmixer")
    fp_model = FastViT(
        layers,
        token_mixers=token_mixers,
        embed_dims=embed_dims,
        mlp_ratios=mlp_ratios,
        downsamples=downsamples,
        inference_mode=True,
    )
    fp_model.default_cfg = default_cfgs["fastvit_s"]
    checkpoint = '/home/zou/codes/FP8-quantization/model_dir/fastvit_s12_reparam.pth.tar'
    load_checkpoint(fp_model, checkpoint)
    quant_model = QuantizedFastViT(fp_model, **qparams)
    return quant_model



def fastvit_sa12_quantized_approx(pretrained=True, model_dir=None, load_type="fp32", **qparams):
    """Instantiate FastViT-SA12 model variant."""
    layers = [2, 2, 6, 2]
    embed_dims = [64, 128, 256, 512]
    mlp_ratios = [4, 4, 4, 4]
    downsamples = [True, True, True, True]
    pos_embs = [None, None, None, partial(RepCPE, spatial_shape=(7, 7))]
    token_mixers = ("repmixer", "repmixer", "repmixer", "attention")
    # stem, basic_blocks, conv_exp, head   # @Zou: ablation study on each module
    # quant = [False, [False, False, False, False], False, False]
    fp_model = FastViT(
        layers,
        token_mixers=token_mixers,
        embed_dims=embed_dims,
        pos_embs=pos_embs,
        mlp_ratios=mlp_ratios,
        downsamples=downsamples,
        inference_mode=True,
    )
    fp_model.default_cfg = default_cfgs["fastvit_s"]

    checkpoint = '/home/zou/codes/ml-fastvit/weights/fastvit_sa12_reparam.pth.tar'
    load_checkpoint(fp_model, checkpoint)
    quant_model = QuantizedFastViT(fp_model, **qparams)
    return quant_model
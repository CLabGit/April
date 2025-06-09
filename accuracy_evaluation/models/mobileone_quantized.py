import os
import re
import torch
import torch.nn.functional as F
from models.mobileone import mobileone, reparameterize_model, MobileOneBlock, SEBlock
from quantization.autoquant_utils import quantize_model
from quantization.base_quantized_classes import QuantizedActivation
from quantization.base_quantized_model import QuantizedModel

class QuantizedSEBlock(QuantizedActivation):
    def __init__(self, gho_seb_orig, **quant_params):
        super().__init__(**quant_params)
        specials = {}

        self.oup = gho_seb_orig.oup

        self.reduce = quantize_model(
            gho_seb_orig.reduce,
            specials = specials,
            **quant_params
        )

        self.expand = quantize_model(
            gho_seb_orig.expand,
            specials = specials,
            **quant_params
        )

    def forward(self, inputs: torch.Tensor) -> torch.Tensor:
        """ Apply forward pass. """
        b, c, h, w = inputs.size()
        x = F.avg_pool2d(inputs, kernel_size=[h, w])
        x = self.reduce(x)
        x = F.relu(x)
        x = self.expand(x)
        x = torch.sigmoid(x)
        x = x.view(-1, c, 1, 1)
        x = inputs * x
        x = self.quantize_activations(x)
        return x


class QuantizedMobileOneBlock(QuantizedActivation):
    """ Ghost bottleneck w/ optional SE"""
    def __init__(self, gho_mob_orig, **quant_params):
        super().__init__(**quant_params)
        specials = {SEBlock: QuantizedSEBlock}

        self.inference_mode = gho_mob_orig.inference_mode
        self.groups = gho_mob_orig.groups
        self.stride = gho_mob_orig.stride
        self.kernel_size = gho_mob_orig.kernel_size
        self.in_channels = gho_mob_orig.in_channels
        self.out_channels = gho_mob_orig.out_channels
        self.num_conv_branches = gho_mob_orig.num_conv_branches

        self.activation = quantize_model(
            gho_mob_orig.activation,
            specials = specials,
            **quant_params
        )

        self.se = quantize_model(
            gho_mob_orig.se,
            specials = specials,
            **quant_params
        )

        if self.inference_mode:
            self.reparam_conv = quantize_model(
                gho_mob_orig.reparam_conv,
                specials = specials,
                **quant_params
            )
        else:
            self.rbr_skip = quantize_model(
                gho_mob_orig.rbr_skip,
                specials = specials,
                **quant_params
            )
            self.rbr_scale = None
            if self.kernel_size > 1:
                self.rbr_scale = quantize_model(
                    gho_mob_orig.rbr_scale,
                    specials = specials,
                    **quant_params
                )
            self.rbr_conv = quantize_model(
                gho_mob_orig.rbr_conv,
                specials = specials,
                **quant_params
            )
        

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        """ Apply forward pass. """
        # Inference mode forward pass.
        if self.inference_mode:
            return self.activation(self.se(self.reparam_conv(x)))

        # Multi-branched train-time forward pass.
        # Skip branch output
        identity_out = 0
        if self.rbr_skip is not None:
            identity_out = self.rbr_skip(x)

        # Scale branch output
        scale_out = 0
        if self.rbr_scale is not None:
            scale_out = self.rbr_scale(x)

        # Other branches
        out = scale_out + identity_out
        for ix in range(self.num_conv_branches):
            out += self.rbr_conv[ix](x)

        return self.activation(self.se(out))

class QuantizedMobileone(QuantizedModel):
    def __init__(self, model_fp, input_size=(1,3,320,256), quant_setup=None, **quant_params):
        super().__init__(input_size)
        specials = {MobileOneBlock: QuantizedMobileOneBlock}
        # quantize and copy parts from original model
        quantize_input = quant_setup and quant_setup == "LSQ_paper"

        self.stage0 = quantize_model(
            model_fp.stage0,
            specials = specials,
            **quant_params
        )

        self.stage1 = quantize_model(
            model_fp.stage1,
            specials = specials,
            **quant_params
        )

        self.stage2 = quantize_model(
            model_fp.stage2,
            specials = specials,
            **quant_params
        )

        self.stage3 = quantize_model(
            model_fp.stage3,
            specials = specials,
            **quant_params
        )

        self.stage4 = quantize_model(
            model_fp.stage4,
            specials = specials,
            **quant_params
        )

        self.gap = quantize_model(
            model_fp.gap,
            specials = specials,
            **quant_params
        )

        self.linear = quantize_model(
            model_fp.linear,
            specials = specials,
            **quant_params
        )

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        """ Apply forward pass. """
        x = self.stage0(x)
        x = self.stage1(x)
        x = self.stage2(x)
        x = self.stage3(x)
        x = self.stage4(x)
        x = self.gap(x)
        x = x.view(x.size(0), -1)
        x = self.linear(x)
        return x

def mobileone_quantized(pretrained=True, model_dir=None, load_type="fp32", **qparams):
    # Load model from pretrained FP32 weights
    fp_model = mobileone()
    model_dir = "/home/zou/codes/April/model_dir/mobileone_s0.pth.tar"
    assert os.path.exists(model_dir)
    print(f"Loading pretrained weights from {model_dir}")
    state_dict = torch.load(model_dir)
    fp_model.load_state_dict(state_dict)
    fp_model = reparameterize_model(fp_model)
    quant_model = QuantizedMobileone(fp_model, **qparams)

    return quant_model

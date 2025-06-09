#!/usr/bin/env python
# Copyright (c) 2022 Qualcomm Technologies, Inc.
# All Rights Reserved.

import copy

from timm.models.layers.activations import Swish, HardSwish, HardSigmoid
from timm.models.layers.activations_me import SwishMe, HardSwishMe, HardSigmoidMe
from torch import nn

from quantization.base_quantized_classes import QuantizedModule
from quantization.quantization_manager import QuantizationManager
from quantization.range_estimators import RangeEstimators

activations_set = [
    nn.ReLU,
    nn.ReLU6,
    nn.Hardtanh,
    nn.Sigmoid,
    nn.Tanh,
    nn.GELU,
    nn.PReLU,
    Swish,
    SwishMe,
    HardSwish,
    HardSwishMe,
    HardSigmoid,
    HardSigmoidMe,
]


class QuantizationHijacker(QuantizedModule):
    """Mixin class that 'hijacks' the forward pass in a module to perform quantization and
    dequantization on the weights and output distributions.

    Usage:
    To make a quantized nn.Linear layer:
    class HijackedLinear(QuantizationHijacker, nn.Linear):
        pass
    """

    def __init__(self, *args, activation: nn.Module = None, **kwargs):

        super().__init__(*args, **kwargs)
        if activation:
            assert isinstance(activation, tuple(activations_set)), str(activation)

        self.activation_function = copy.deepcopy(activation) if activation else None

        self.activation_quantizer = QuantizationManager(
            qmethod=self.act_method,
            init=self.act_range_method,
            qparams=self.act_qparams,
            range_estim_params=self.act_range_options,
        )
        
        self.res_quantizer = QuantizationManager(
            qmethod=self.act_method,
            init=self.act_range_method,
            qparams=self.act_qparams,
            range_estim_params=self.act_range_options,
        )

        if self.weight_range_method == RangeEstimators.current_minmax:
            weight_init_params = dict(percentile=self.percentile)
        else:
            weight_init_params = self.weight_range_options

        self.weight_quantizer = QuantizationManager(
            qmethod=self.method,
            init=self.weight_range_method,
            per_channel=self.per_channel_weights,
            qparams=self.weight_qparams,
            range_estim_params=weight_init_params,
        )
        
    def forward(self, x, offsets=None):
        # self.approx_flag = False
        # self.quantize_after_mult_and_add = False
        # self.res_quantizer_flag = False
        # Quantize input
        if self.quantize_input and self._quant_a:
            x = self.activation_quantizer(x)

        # Get quantized weight
        # print(f"fix_ranges_flag: {self.fix_ranges_flag}, original_quantize_res: {self.original_quantize_res}")
        weight, bias = self.get_params()
        if self.fix_ranges_flag == False or self.original_quantize_res:
            res = self.run_forward(x, weight, bias, offsets=offsets)
            
            # self.approx_flag = self.approx_flag_base
            # self.quantize_after_mult_and_add = self.quantize_after_mult_and_add_base
            # self.res_quantizer_flag = self.res_quantizer_flag_base
            
            if self.quantize_input and self._quant_a and self.res_quantizer_flag:
                res = self.res_quantizer(res)

        if self.res_quantizer_flag and self.quantize_after_mult_and_add:
            res = self.run_forward(x, weight, bias)
        
        if self.res_quantizer_flag and self.approx_flag:
            res = self.run_forward(x, weight, bias, offsets=offsets)

        if (self.quantize_after_mult_and_add or self.approx_flag) and not self.res_quantizer_flag:
            raise ValueError("quantize_after_mult_and_add or approx_flag is set but res_quantizer_flag is not set. " + 
                "you need to set res_quantizer_flag to True if you want to use quantize_after_mult_and_add or approx_flag")
        
        # Apply fused activation function
        if self.activation_function is not None:
            res = self.activation_function(res)

        # Quantize output
        if not self.quantize_input and self._quant_a:
            res = self.activation_quantizer(res)
        return res

    def get_params(self):

        weight, bias = self.get_weight_bias()

        if self._quant_w:
            # print(f"before quantize_weights: {weight}")
            weight = self.quantize_weights(weight)
            # print(f"after quantize_weights: {weight}")
        return weight, bias

    def quantize_weights(self, weights):
        return self.weight_quantizer(weights)
    
    def get_weights_fp_bias(self):
        return self.weight_quantizer.get_fp_bias()
    
    def get_acts_fp_bias(self):
        return self.activation_quantizer.get_fp_bias()
    
    def get_res_fp_bias(self):
        return self.res_quantizer.get_fp_bias()
    
    def get_weight_bias(self):
        bias = None
        if hasattr(self, "bias"):
            bias = self.bias
        return self.weight, bias

    def run_forward(self, x, weight, bias, offsets=None):
        # Performs the actual linear operation of the layer
        raise NotImplementedError()

    def extra_repr(self):
        activation = "input" if self.quantize_input else "output"
        return f"{super().extra_repr()}-{activation}"

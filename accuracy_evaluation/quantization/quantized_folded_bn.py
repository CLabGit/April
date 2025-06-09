#!/usr/bin/env python
# Copyright (c) 2022 Qualcomm Technologies, Inc.
# All Rights Reserved.
import torch
import torch.nn.functional as F
from torch import nn
from torch.nn.modules.conv import _ConvNd

from quantization.hijacker import QuantizationHijacker


class BNFusedHijacker(QuantizationHijacker):
    """Extension to the QuantizationHijacker that fuses batch normalization (BN) after a weight
    layer into a joined module. The parameters and the statistics of the BN layer remain in
    full-precision.
    """

    def __init__(self, *args, **kwargs):
        kwargs.pop("bias", None)  # Bias will be learned by BN params
        super().__init__(*args, **kwargs, bias=False)
        bn_dim = self.get_bn_dim()
        self.register_buffer("running_mean", torch.zeros(bn_dim))
        self.register_buffer("running_var", torch.ones(bn_dim))
        self.momentum = kwargs.pop("momentum", 0.1)
        self.gamma = nn.Parameter(torch.ones(bn_dim))
        self.beta = nn.Parameter(torch.zeros(bn_dim))
        self.epsilon = kwargs.get("eps", 1e-5)
        self.bias = None

    def forward(self, x):
        # self.approx_flag = False
        # self.quantize_after_mult_and_add = False
        # self.res_quantizer_flag = False
        # Quantize input
        if self.quantize_input and self._quant_a:
            x = self.activation_quantizer(x)

        # Get quantized weight
        weight, bias = self.get_params()
        if self.fix_ranges_flag == False or self.original_quantize_res:
            res = self.run_forward(x, weight, bias)
            
            # self.approx_flag = self.approx_flag_base
            # self.quantize_after_mult_and_add = self.quantize_after_mult_and_add_base
            # self.res_quantizer_flag = self.res_quantizer_flag_base
            
            if self.quantize_input and self._quant_a and self.res_quantizer_flag:
                res = self.res_quantizer(res)

        if self.res_quantizer_flag and self.quantize_after_mult_and_add:
            res = self.run_forward(x, weight, bias)
        
        if self.res_quantizer_flag and self.approx_flag:
            res = self.run_forward(x, weight, bias)

        if (self.quantize_after_mult_and_add or self.approx_flag) and not self.res_quantizer_flag:
            raise ValueError("quantize_after_mult_and_add or approx_flag is set but res_quantizer_flag is not set. " + 
                "you need to set res_quantizer_flag to True if you want to use quantize_after_mult_and_add or approx_flag")
        
        # print(f"qamaa_res.shape: {res.shape}")
        # print(f"qaa_res.shape: {res1.shape}")
        # mse = F.mse_loss(res, res1)
        # rmse = torch.sqrt(mse)
        # print(f"RMSE: {rmse}, MSE: {mse}")

        res = F.batch_norm(
            res,
            self.running_mean,
            self.running_var,
            self.gamma,
            self.beta,
            self.training,
            self.momentum,
            self.epsilon,
        )
        # Apply fused activation function
        if self.activation_function is not None:
            res = self.activation_function(res)

        # Quantize output
        if not self.quantize_input and self._quant_a:
            res = self.activation_quantizer(res)
        return res

    def get_bn_dim(self):
        if isinstance(self, nn.Linear):
            return self.out_features
        elif isinstance(self, _ConvNd):
            return self.out_channels
        else:
            msg = (
                f"Unsupported type used: {self}. Must be a linear or (transpose)-convolutional "
                f"nn.Module"
            )
            raise NotImplementedError(msg)

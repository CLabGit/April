#!/usr/bin/env python
# Copyright (c) 2022 Qualcomm Technologies, Inc.
# All Rights Reserved.

import torch
from torch import nn

from quantization.quantization_manager import QuantizationManager
from quantization.quantizers import QuantizerBase, AsymmetricUniformQuantizer
from quantization.range_estimators import (
    RangeEstimatorBase,
    CurrentMinMaxEstimator,
    RunningMinMaxEstimator,
)


def _set_layer_learn_ranges(layer):
    if isinstance(layer, QuantizationManager):
        if layer.quantizer.is_initialized:
            layer.learn_ranges()


def _set_layer_fix_ranges(layer):
    if isinstance(layer, QuantizationManager):
        if layer.quantizer.is_initialized:
            layer.fix_ranges()
    if isinstance(layer, QuantizedModule):
        layer.fix_ranges_flag = True


def _set_layer_estimate_ranges(layer):
    if isinstance(layer, QuantizationManager):
        layer.estimate_ranges()
    if isinstance(layer, QuantizedModule):
        layer.fix_ranges_flag = False


def _set_layer_estimate_ranges_train(layer):
    if isinstance(layer, QuantizationManager):
        if layer.quantizer.is_initialized:
            layer.estimate_ranges_train()
            
def _set_layer_approx_calculation(layer):
    if isinstance(layer, QuantizedModule):
        if layer.approx_flag is not None:
            layer.approx_calculation()


class QuantizedModule(nn.Module):
    """
    Parent class for a quantized module. It adds the basic functionality of switching the module
    between quantized and full precision mode. It also defines the cached parameters and handles
    the reset of the cache properly.
    """

    def __init__(
        self,
        *args,
        method: QuantizerBase = AsymmetricUniformQuantizer,
        act_method=None,
        weight_range_method: RangeEstimatorBase = CurrentMinMaxEstimator,
        act_range_method: RangeEstimatorBase = RunningMinMaxEstimator,
        n_bits=8,
        n_bits_act=None,
        per_channel_weights=False,
        percentile=None,
        weight_range_options=None,
        act_range_options=None,
        scale_domain="linear",
        act_quant_kwargs={},
        weight_quant_kwargs={},
        quantize_input=False,
        fp8_kwargs=None,
        **kwargs
    ):
        kwargs.pop("act_quant_dict", None)
        self.custom_approx_params = kwargs.pop("custom_approx_params", None)
        self.run_method = kwargs.pop("run_method", None)
        self.approx_flag = self.run_method["approx_flag"]
        self.quantize_after_mult_and_add = self.run_method["quantize_after_mult_and_add"]
        self.res_quantizer_flag = self.run_method["res_quantizer_flag"]
        self.original_quantize_res = self.run_method["original_quantize_res"]
        # self.approx_flag_base = self.run_method["approx_flag"]
        # self.quantize_after_mult_and_add_base = self.run_method["quantize_after_mult_and_add"]
        # self.res_quantizer_flag_base = self.run_method["res_quantizer_flag"]
        
        if self.custom_approx_params['test_baseline']:
            self.flex_bias = False
        elif self.custom_approx_params['test_best_allnorm']:
            self.flex_bias = True
        elif self.custom_approx_params['test_casestudy']:
            self.flex_bias = self.custom_approx_params['with_flexbias']
            
        super().__init__(*args, **kwargs)

        self.method = method
        self.act_method = act_method or method
        self.n_bits = n_bits
        self.n_bits_act = n_bits_act or n_bits
        self.per_channel_weights = per_channel_weights
        self.percentile = percentile
        self.weight_range_method = weight_range_method
        self.weight_range_options = weight_range_options if weight_range_options else {}
        self.act_range_method = act_range_method
        self.act_range_options = act_range_options if act_range_options else {}
        self.scale_domain = scale_domain
        self.quantize_input = quantize_input
        self.fp8_kwargs = fp8_kwargs or {}

        self.quant_params = None
        self.register_buffer("_quant_w", torch.BoolTensor([False]))
        self.register_buffer("_quant_a", torch.BoolTensor([False]))

        self.act_qparams = dict(
            n_bits=self.n_bits_act,
            scale_domain=self.scale_domain,
            flex_bias=self.flex_bias,
            **act_quant_kwargs,
            **self.fp8_kwargs,
        )
        self.weight_qparams = dict(
            n_bits=self.n_bits,
            scale_domain=self.scale_domain,
            flex_bias=self.flex_bias,
            **weight_quant_kwargs,
            **self.fp8_kwargs,
        )
        
        self.fix_ranges_flag = False
        # self.approx_flag = False
        # self.quantize_after_mult_and_add = False
        

    def quantized_weights(self):
        self._quant_w = torch.BoolTensor([True])

    def full_precision_weights(self):
        self._quant_w = torch.BoolTensor([False])

    def quantized_acts(self):
        self._quant_a = torch.BoolTensor([True])

    def full_precision_acts(self):
        self._quant_a = torch.BoolTensor([False])

    def quantized(self):
        self.quantized_weights()
        self.quantized_acts()

    def full_precision(self):
        self.full_precision_weights()
        self.full_precision_acts()

    def get_quantizer_status(self):
        return dict(quant_a=self._quant_a.item(), quant_w=self._quant_w.item())

    def set_quantizer_status(self, quantizer_status):
        if quantizer_status["quant_a"]:
            self.quantized_acts()
        else:
            self.full_precision_acts()

        if quantizer_status["quant_w"]:
            self.quantized_weights()
        else:
            self.full_precision_weights()

    def learn_ranges(self):
        self.apply(_set_layer_learn_ranges)

    def fix_ranges(self):
        self.apply(_set_layer_fix_ranges)
        self.fix_ranges_flag = True
        # self.approx_flag = self.approx_flag_base
        # self.quantize_after_mult_and_add = self.quantize_after_mult_and_add_base

    def estimate_ranges(self):
        self.apply(_set_layer_estimate_ranges)
        self.fix_ranges_flag = False
        # self.approx_flag = False
        # self.quantize_after_mult_and_add = False

    def estimate_ranges_train(self):
        self.apply(_set_layer_estimate_ranges_train)

    def extra_repr(self):
        quant_state = "weight_quant={}, act_quant={}".format(
            self._quant_w.item(), self._quant_a.item()
        )
        parent_repr = super().extra_repr()
        return "{},\n{}".format(parent_repr, quant_state) if parent_repr else quant_state
    
    def approx_calculation(self):
        self.approx_flag = True
        self.apply(_set_layer_approx_calculation)


class QuantizedActivation(QuantizedModule):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.activation_quantizer = QuantizationManager(
            qmethod=self.act_method,
            qparams=self.act_qparams,
            init=self.act_range_method,
            range_estim_params=self.act_range_options,
        )

    def quantize_activations(self, x):
        if self._quant_a:
            return self.activation_quantizer(x)
        else:
            return x

    def forward(self, x):
        return self.quantize_activations(x)


class FP32Acts(nn.Module):
    def forward(self, x):
        return x

    def reset_ranges(self):
        pass

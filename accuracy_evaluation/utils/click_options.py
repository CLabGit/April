#!/usr/bin/env python
# Copyright (c) 2022 Qualcomm Technologies, Inc.
# All Rights Reserved.

import click

from models import QuantArchitectures
from functools import wraps, partial
from quantization.quantization_manager import QMethods
from quantization.range_estimators import RangeEstimators, OptMethod
from utils import split_dict, DotDict, ClickEnumOption, seed_all
from utils.imagenet_dataloaders import ImageInterpolation

click.option = partial(click.option, show_default=True)

_HELP_MSG = (
    "Enforce determinism also on the GPU by disabling CUDNN and setting "
    "`torch.set_deterministic(True)`. In many cases this comes at the cost of efficiency "
    "and performance."
)


def base_options(func):
    @click.option(
        "--images-dir", type=click.Path(exists=True), help="Root directory of images", required=True
    )
    @click.option("--max-epochs", default=90, type=int, help="Maximum number of training epochs.")
    @click.option(
        "--interpolation",
        type=ClickEnumOption(ImageInterpolation),
        default=ImageInterpolation.bilinear.name,
        help="Desired interpolation to use for resizing.",
    )
    @click.option(
        "--save-checkpoint-dir",
        type=click.Path(exists=False),
        default=None,
        help="Directory where to save checkpoints (model, optimizer, lr_scheduler).",
    )
    @click.option(
        "--tb-logging-dir", default=None, type=str, help="The logging directory " "for tensorboard"
    )
    @click.option("--cuda/--no-cuda", is_flag=True, default=True, help="Use GPU")
    @click.option("--batch-size", default=128, type=int, help="Mini-batch size")
    @click.option("--num-workers", default=16, type=int, help="Number of workers for data loading")
    @click.option("--seed", default=None, type=int, help="Random number generator seed to set")
    @click.option("--deterministic/--nondeterministic", default=False, help=_HELP_MSG)
    # Architecture related options
    @click.option(
        "--architecture",
        type=ClickEnumOption(QuantArchitectures),
        required=True,
        help="Quantized architecture",
    )
    @click.option(
        "--model-dir",
        type=click.Path(exists=True),
        default=None,
        help="Path for model directory. If the model does not exist it will downloaded "
        "from a URL",
    )
    @click.option(
        "--pretrained/--no-pretrained",
        is_flag=True,
        default=True,
        help="Use pretrained model weights",
    )
    @click.option(
        "--progress-bar/--no-progress-bar", is_flag=True, default=False, help="Show progress bar"
    )
    @wraps(func)
    def func_wrapper(config, *args, **kwargs):
        config.base, remaining_kwargs = split_dict(
            kwargs,
            [
                "images_dir",
                "max_epochs",
                "interpolation",
                "save_checkpoint_dir",
                "tb_logging_dir",
                "cuda",
                "batch_size",
                "num_workers",
                "seed",
                "model_dir",
                "architecture",
                "pretrained",
                "deterministic",
                "progress_bar",
            ],
        )

        seed, deterministic = config.base.seed, config.base.deterministic

        if seed is None:
            if deterministic is True:
                raise ValueError("Enforcing determinism without providing a seed is not supported")
        else:
            seed_all(seed=seed, deterministic=deterministic)

        return func(config, *args, **remaining_kwargs)

    return func_wrapper


class multi_optimizer_options:
    """
    An instance of this class is a callable object to serve as a decorator;
    hence the lower case class name.

    Among the CLI options defined in the decorator, `--{prefix-}optimizer-type`
    requires special attention. Default value for that variable for
    {prefix-}optimizer is the value in use by the main optimizer.

    Examples:
        @multi_optimizer_options('quant')
        @pass_config
        def command(config):
            ...
    """

    def __init__(self, prefix: str = ""):
        self.optimizer_name = prefix + "_optimizer" if prefix else "optimizer"
        self.prefix_option = prefix + "-" if prefix else ""
        self.prefix_attribute = prefix + "_" if prefix else ""

    def __call__(self, func):
        prefix_option = self.prefix_option
        prefix_attribute = self.prefix_attribute

        @click.option(
            f"--{prefix_option}optimizer",
            default="SGD",
            type=click.Choice(["SGD", "Adam"], case_sensitive=False),
            help=f"Class name of torch Optimizer to be used.",
        )
        @click.option(
            f"--{prefix_option}learning-rate",
            default=None,
            type=float,
            help="Initial learning rate.",
        )
        @click.option(
            f"--{prefix_option}momentum", default=0.9, type=float, help=f"Optimizer momentum."
        )
        @click.option(
            f"--{prefix_option}weight-decay",
            default=None,
            type=float,
            help="Weight decay for the network.",
        )
        @click.option(
            f"--{prefix_option}learning-rate-schedule",
            default=None,
            type=str,
            help="Learning rate scheduler, 'MultiStepLR:10:20:40' or "
            "'cosine:1e-4' for cosine decay",
        )
        @wraps(func)
        def func_wrapper(config, *args, **kwargs):
            base_arg_names = [
                "optimizer",
                "learning_rate",
                "momentum",
                "weight_decay",
                "learning_rate_schedule",
            ]

            optimizer_opt = DotDict()

            # Collect basic arguments
            for arg in base_arg_names:
                option_name = prefix_attribute + arg
                optimizer_opt[arg] = kwargs.pop(option_name)

            # config.{prefix_attribute}optimizer = optimizer_opt
            setattr(config, prefix_attribute + "optimizer", optimizer_opt)

            return func(config, *args, **kwargs)

        return func_wrapper


def qat_options(func):
    @click.option(
        "--reestimate-bn-stats/--no-reestimate-bn-stats",
        is_flag=True,
        default=True,
        help="Reestimates the BN stats before every evaluation.",
    )
    @click.option(
        "--grad-scaling/--no-grad-scaling",
        is_flag=True,
        default=False,
        help="Do gradient scaling as in LSQ paper.",
    )
    @click.option(
        "--sep-quant-optimizer/--no-sep-quant-optimizer",
        is_flag=True,
        default=False,
        help="Use a separate optimizer for the quantizers.",
    )
    @multi_optimizer_options("quant")
    @oscillations_dampen_options
    @oscillations_freeze_options
    @wraps(func)
    def func_wrapper(config, *args, **kwargs):
        config.qat, remainder_kwargs = split_dict(
            kwargs, ["reestimate_bn_stats", "grad_scaling", "sep_quant_optimizer"]
        )
        return func(config, *args, **remainder_kwargs)

    return func_wrapper


def oscillations_dampen_options(func):
    @click.option(
        "--oscillations-dampen-weight",
        default=None,
        type=float,
        help="If given, adds a oscillations dampening to the loss with given  " "weighting.",
    )
    @click.option(
        "--oscillations-dampen-aggregation",
        type=click.Choice(["sum", "mean", "kernel_mean"]),
        default="kernel_mean",
        help="Aggregation type for bin regularization loss.",
    )
    @click.option(
        "--oscillations-dampen-weight-final",
        type=float,
        default=None,
        help="Dampening regularization final value for annealing schedule.",
    )
    @click.option(
        "--oscillations-dampen-anneal-start",
        default=0.25,
        type=float,
        help="Start of annealing (relative to total number of iterations).",
    )
    @wraps(func)
    def func_wrapper(config, *args, **kwargs):
        config.osc_damp, remainder_kwargs = split_dict(
            kwargs,
            [
                "oscillations_dampen_weight",
                "oscillations_dampen_aggregation",
                "oscillations_dampen_weight_final",
                "oscillations_dampen_anneal_start",
            ],
            "oscillations_dampen",
        )

        return func(config, *args, **remainder_kwargs)

    return func_wrapper


def oscillations_freeze_options(func):
    @click.option(
        "--oscillations-freeze-threshold",
        default=0.0,
        type=float,
        help="If greater than 0, we will freeze oscillations which frequency (EMA) is "
        "higher  than the given threshold. Frequency is defined as 1/period length.",
    )
    @click.option(
        "--oscillations-freeze-ema-momentum",
        default=0.001,
        type=float,
        help="The momentum to calculate the EMA frequency of the oscillation. In case"
        "freezing is used, this should be at least 2-3 times lower than the "
        "freeze threshold.",
    )
    @click.option(
        "--oscillations-freeze-use-ema/--no-oscillation-freeze-use-ema",
        is_flag=True,
        default=True,
        help="Uses an EMA of past x_int to find the correct freezing int value.",
    )
    @click.option(
        "--oscillations-freeze-max-bits",
        default=4,
        type=int,
        help="Max bit-width for oscillation tracking and freezing. If layers weight is in"
        "higher bits we do not track or freeze oscillations.",
    )
    @click.option(
        "--oscillations-freeze-threshold-final",
        type=float,
        default=None,
        help="Oscillation freezing final value for annealing schedule.",
    )
    @click.option(
        "--oscillations-freeze-anneal-start",
        default=0.25,
        type=float,
        help="Start of annealing (relative to total number of iterations).",
    )
    @wraps(func)
    def func_wrapper(config, *args, **kwargs):
        config.osc_freeze, remainder_kwargs = split_dict(
            kwargs,
            [
                "oscillations_freeze_threshold",
                "oscillations_freeze_ema_momentum",
                "oscillations_freeze_use_ema",
                "oscillations_freeze_max_bits",
                "oscillations_freeze_threshold_final",
                "oscillations_freeze_anneal_start",
            ],
            "oscillations_freeze",
        )

        return func(config, *args, **remainder_kwargs)

    return func_wrapper


def quantization_options(func):
    # Weight quantization options
    @click.option(
        "--weight-quant/--no-weight-quant",
        is_flag=True,
        default=True,
        help="Run evaluation weight quantization or use FP32 weights",
    )
    @click.option(
        "--qmethod",
        type=ClickEnumOption(QMethods),
        default=QMethods.symmetric_uniform.name,
        help="Quantization scheme to use.",
    )
    @click.option(
        "--weight-quant-method",
        default=RangeEstimators.current_minmax.name,
        type=ClickEnumOption(RangeEstimators),
        help="Method to determine weight quantization clipping thresholds.",
    )
    @click.option(
        "--weight-opt-method",
        default=OptMethod.grid.name,
        type=ClickEnumOption(OptMethod),
        help="Optimization procedure for activation quantization clipping thresholds",
    )
    @click.option(
        "--num-candidates",
        type=int,
        default=None,
        help="Number of grid points for grid search in MSE range method.",
    )
    @click.option("--n-bits", default=8, type=int, help="Default number of quantization bits.")
    @click.option(
        "--per-channel/--no-per-channel",
        is_flag=True,
        default=False,
        help="If given, quantize each channel separately.",
    )
    # Activation quantization options
    @click.option(
        "--act-quant/--no-act-quant",
        is_flag=True,
        default=True,
        help="Run evaluation with activation quantization or use FP32 activations",
    )
    @click.option(
        "--qmethod-act",
        type=ClickEnumOption(QMethods),
        default=None,
        help="Quantization scheme for activation to use. If not specified `--qmethod` " "is used.",
    )
    @click.option(
        "--n-bits-act", default=None, type=int, help="Number of quantization bits for activations."
    )
    @click.option(
        "--act-quant-method",
        default=RangeEstimators.running_minmax.name,
        type=ClickEnumOption(RangeEstimators),
        help="Method to determine activation quantization clipping thresholds",
    )
    @click.option(
        "--act-opt-method",
        default=OptMethod.grid.name,
        type=ClickEnumOption(OptMethod),
        help="Optimization procedure for activation quantization clipping thresholds",
    )
    @click.option(
        "--act-num-candidates",
        type=int,
        default=None,
        help="Number of grid points for grid search in MSE/SQNR/Cross-entropy",
    )
    @click.option(
        "--act-momentum",
        type=float,
        default=None,
        help="Exponential averaging factor for running_minmax",
    )
    @click.option(
        "--num-est-batches",
        type=int,
        default=1,
        help="Number of training batches to be used for activation range estimation",
    )
    # Other options
    @click.option(
        "--quant-setup",
        default="all",
        type=click.Choice(["all", "LSQ", "FP_logits", "fc4", "fc4_dw8", "LSQ_paper"]),
        help="Method to quantize the network.",
    )
    @click.option(
        "--quantize-input/--no-quantize-input",
        is_flag=True,
        default=False,
        help="If given, quantize the input data.",
    )
    @wraps(func)
    def func_wrapper(config, *args, **kwargs):
        config.quant, remainder_kwargs = split_dict(
            kwargs,
            [
                "qmethod",
                "qmethod_act",
                "weight_quant_method",
                "weight_opt_method",
                "num_candidates",
                "n_bits",
                "n_bits_act",
                "per_channel",
                "act_quant",
                "weight_quant",
                "quant_setup",
                "num_est_batches",
                "act_momentum",
                "act_num_candidates",
                "act_opt_method",
                "act_quant_method",
                "quantize_input",
            ],
        )

        config.quant.qmethod_act = config.quant.qmethod_act or config.quant.qmethod

        return func(config, *args, **remainder_kwargs)

    return func_wrapper


def fp8_options(func):
    # Weight quantization options
    @click.option("--fp8-maxval", type=float, default=None)
    @click.option("--fp8-mantissa-bits", type=int, default=4)
    @click.option("--fp8-set-maxval/--no-fp8-set-maxval", is_flag=True, default=False)
    @click.option("--fp8-learn-maxval/--no-fp8-learn-maxval", is_flag=True, default=False)
    @click.option(
        "--fp8-learn-mantissa-bits/--no-fp8-learn-mantissa-bits", is_flag=True, default=False
    )
    @click.option(
        "--fp8-mse-include-mantissa-bits/--no-fp8-mse-include-mantissa-bits",
        is_flag=True,
        default=True,
    )
    @click.option("--fp8-allow-unsigned/--no-fp8-allow-unsigned", is_flag=True, default=False)
    @wraps(func)
    def func_wrapper(config, *args, **kwargs):
        config.fp8, remainder_kwargs = split_dict(
            kwargs,
            [
                "fp8_maxval",
                "fp8_mantissa_bits",
                "fp8_set_maxval",
                "fp8_learn_maxval",
                "fp8_learn_mantissa_bits",
                "fp8_mse_include_mantissa_bits",
                "fp8_allow_unsigned",
            ],
        )
        return func(config, *args, **remainder_kwargs)

    return func_wrapper

def run_method_options(func):
    @click.option("--approx_flag/--no-approx_flag", is_flag=True, default=False, help="Enable or disable approx_flag in QuantizedModule.")
    @click.option("--quantize-after-mult-and-add/--no-quantize-after-mult-and-add", is_flag=True, default=False, help="Enable or disable quantize-after-mult-and-add.")
    @click.option("--res-quantizer-flag/--no-res-quantizer-flag", is_flag=True, default=False, help="Enable or disable res-quantizer.")
    @click.option("--original-quantize-res/--no-original-quantize-res", is_flag=True, default=False, help="Enable or disable original-quantize-res.")
    @wraps(func)
    def func_wrapper(config, *args, **kwargs):
        config.run_method, remainder_kwargs = split_dict(
            kwargs,
            [
                "approx_flag",
                "quantize_after_mult_and_add",
                "res_quantizer_flag",
                "original_quantize_res",
            ],
        )
        return func(config, *args, **remainder_kwargs)
    
    return func_wrapper

def approx_options(func):
    # Approximation options
    @click.option("--expo-width", type=int, default=3)
    @click.option("--mant-width", type=int, default=4)
    @click.option("--dnsmp-factor", type=int, default=3, help="Down-Sample-Compensation factor")
    @click.option("--withComp/--no-withComp", is_flag=True, default=False, help="Enable or disable withComp.")
    # v9
    @click.option("--with_approx/--no-with_approx", is_flag=True, default=False, help="Enable or disable with-approx in custom_matmul_vectorize.")
    @click.option("--with_s2nn2s_opt/--no-with_s2nn2s_opt", is_flag=True, default=False, help="Enable or disable with-s2nn2s-opt.")
    @click.option("--sim_hw_add_OFUF/--no-sim_hw_add_OFUF", is_flag=True, default=False, help="Enable or disable sim-hw-add-OFUF.")
    @click.option("--with_OF_opt/--no-with_OF_opt", is_flag=True, default=False, help="Enable or disable with-OF-opt.")
    @click.option("--with_UF_opt/--no-with_UF_opt", is_flag=True, default=False, help="Enable or disable with-OF-UF-opt.")
    @click.option("--golden-clip-OF/--no-golden-clip-OF", is_flag=True, default=False, help="Enable or disable golden-clip-OF.")
    @click.option("--quant_btw_mult_accu/--no-quant_btw_mult_accu", is_flag=True, default=True, help="Enable or disable double-quant.")
    @click.option("--debug-mode/--no-debug-mode", is_flag=True, default=False, help="Enable or disable debug-mode.")
    @click.option("--self-check-mode/--no-self-check-mode", is_flag=True, default=False, help="Enable or disable self-check-mode.")
    # v11
    @click.option("--test_golden/--no-test_golden", is_flag=True, default=False, help="Enable or disable test_golden.")
    @click.option("--test_golden_quant_btw_mult_accu/--no-test_golden_quant_btw_mult_accu", is_flag=True, default=False, help="Enable or disable test_golden_quant_btw_mult_accu.")
    @click.option("--test_golden_quant_btw_mult_accu_use_flexbias/--no-test_golden_quant_btw_mult_accu_use_flexbias", is_flag=True, default=False, help="Enable or disable test_golden_quant_btw_mult_accu_use_flexbias.")
    @click.option("--test_baseline/--no-test_baseline", is_flag=True, default=False, help="Enable or disable test_baseline.")
    @click.option("--test_best_allnorm/--no-test_best_allnorm", is_flag=True, default=False, help="Enable or disable test_best_allnorm.")
    @click.option("--test_best_s2nn2s/--no-test_best_s2nn2s", is_flag=True, default=False, help="Enable or disable test_best_s2nn2s.")
    @click.option("--test_casestudy/--no-test_casestudy", is_flag=True, default=False, help="Enable or disable test_casestudy.")
    @click.option("--with_compensation/--no-with_compensation", is_flag=True, default=False, help="Enable or disable with_compensation.")
    @click.option("--with_view_all_as_norm/--no-with_view_all_as_norm", is_flag=True, default=False, help="Enable or disable with_view_all_as_norm.")
    @click.option("--with_flexbias/--no-with_flexbias", is_flag=True, default=False, help="Enable or disable with_flexbias.")
    # @click.option("--with_s2nn2s_opt/--no-with_s2nn2s_opt", is_flag=True, default=False, help="Enable or disable with_s2nn2s_opt.")
    # @click.option("--debug-mode/--no-debug-mode", is_flag=True, default=False, help="Enable or disable debug-mode.")
    # @click.option("--self-check-mode/--no-self-check-mode", is_flag=True, default=False, help="Enable or disable self-check-mode.")
    # output_dir
    @click.option("--approx-output-dir", type=click.Path(exists=False), help="Output directory for the results.")
    @wraps(func)
    def func_wrapper(config, *args, **kwargs):
        config.approx, remainder_kwargs = split_dict(
            kwargs,
            [
                "expo_width",
                "mant_width",
                "dnsmp_factor",
                "withcomp",
                # v9
                "with_approx",
                "with_s2nn2s_opt",
                "sim_hw_add_ofuf", # 不可以使用大写字母
                "with_of_opt",
                "with_uf_opt",
                "golden_clip_of",
                "quant_btw_mult_accu",
                "debug_mode",
                "self_check_mode",
                # v11
                "test_golden",
                "test_golden_quant_btw_mult_accu",
                "test_golden_quant_btw_mult_accu_use_flexbias",
                "test_baseline",
                "test_best_allnorm",
                "test_best_s2nn2s",
                "test_casestudy",
                "with_compensation",
                "with_view_all_as_norm",
                "with_flexbias",
                # "with_s2nn2s_opt",
                # "debug_mode",
                # "self_check_mode",
                "approx_output_dir",
            ],
        )
        return func(config, *args, **remainder_kwargs)
    
    return func_wrapper

def quant_params_dict(config):
    weight_range_options = {}
    if config.quant.weight_quant_method == RangeEstimators.MSE:
        weight_range_options = dict(opt_method=config.quant.weight_opt_method)
    if config.quant.num_candidates is not None:
        weight_range_options["num_candidates"] = config.quant.num_candidates

    act_range_options = {}
    if config.quant.act_quant_method == RangeEstimators.MSE:
        act_range_options = dict(opt_method=config.quant.act_opt_method)
    if config.quant.act_num_candidates is not None:
        act_range_options["num_candidates"] = config.quant.num_candidates

    qparams = {
        "method": config.quant.qmethod.cls,
        "n_bits": config.quant.n_bits,
        "n_bits_act": config.quant.n_bits_act,
        "act_method": config.quant.qmethod_act.cls,
        "per_channel_weights": config.quant.per_channel,
        "quant_setup": config.quant.quant_setup,
        "weight_range_method": config.quant.weight_quant_method.cls,
        "weight_range_options": weight_range_options,
        "act_range_method": config.quant.act_quant_method.cls,
        "act_range_options": act_range_options,
        "quantize_input": True if config.quant.quant_setup == "LSQ_paper" or config.quant.quantize_input else False,
    }

    if config.quant.qmethod.name.startswith("fp_quantizer"):
        fp8_kwargs = {
            k.replace("fp8_", ""): v for k, v in config.fp8.items() if k.startswith("fp8")
        }
        qparams["fp8_kwargs"] = fp8_kwargs

    return qparams

def run_method_dict(config):
    run_method = {
        "approx_flag": config.run_method.approx_flag,
        "quantize_after_mult_and_add": config.run_method.quantize_after_mult_and_add,
        "res_quantizer_flag": config.run_method.res_quantizer_flag,
        "original_quantize_res": config.run_method.original_quantize_res,
    }
    
    return run_method

def approx_params_dict(config):
    approx_params = {
        "expo_width": config.approx.expo_width,
        "mant_width": config.approx.mant_width,
        "dnsmp_factor": config.approx.dnsmp_factor,
        "withComp": config.approx.withcomp,
        # v9
        "with_approx": config.approx.with_approx,
        "with_s2nn2s_opt": config.approx.with_s2nn2s_opt,
        "sim_hw_add_OFUF": config.approx.sim_hw_add_ofuf,
        "with_OF_opt": config.approx.with_of_opt,
        "with_UF_opt": config.approx.with_uf_opt,
        "golden_clip_OF": config.approx.golden_clip_of,
        "quant_btw_mult_accu": config.approx.quant_btw_mult_accu,
        "debug_mode": config.approx.debug_mode,
        "self_check_mode": config.approx.self_check_mode,
        # v11
        "test_golden": config.approx.test_golden,
        "test_golden_quant_btw_mult_accu": config.approx.test_golden_quant_btw_mult_accu,
        "test_golden_quant_btw_mult_accu_use_flexbias": config.approx.test_golden_quant_btw_mult_accu_use_flexbias,
        "test_baseline": config.approx.test_baseline,
        "test_best_allnorm": config.approx.test_best_allnorm,
        "test_best_s2nn2s": config.approx.test_best_s2nn2s,
        "test_casestudy": config.approx.test_casestudy,
        "with_compensation": config.approx.with_compensation,
        "with_view_all_as_norm": config.approx.with_view_all_as_norm,
        "with_flexbias": config.approx.with_flexbias,
        # "with_s2nn2s_opt": config.approx.with_s2nn2s_opt,
        # "debug_mode": config.approx.debug_mode,
        # "self_check_mode": config.approx.self_check_mode,
    }
    
    return approx_params
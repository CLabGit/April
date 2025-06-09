import copy

import torch.nn as nn
from approx.approx_calculation import *
from models.mobilenet_v2 import InvertedResidual, MobileNetV2
from models.mobilenet_v2_quantized import QuantizedInvertedResidual, BNQConv
from quantization.base_quantized_classes import QuantizedActivation

from quantization.autoquant_utils import QuantConv, QuantLinear

def replace_conv2d_with_numpy(module):
    for name, child in module.named_children():
        if isinstance(child, nn.Conv2d):
            setattr(module, name, CustomConv2dNumPy(child.in_channels, child.out_channels, 
                                                    child.kernel_size, stride=child.stride,
                                                    padding=child.padding, dilation=child.dilation,
                                                    groups=child.groups, bias=child.bias is not None))
        elif isinstance(child, nn.Sequential) or isinstance(child, InvertedResidual):
            replace_conv2d_with_numpy(child)
        elif isinstance(child, nn.ModuleList):
            for i, submodule in enumerate(child):
                replace_conv2d_with_numpy(submodule)
                
def replace_linear_with_numpy(module):
    for name, child in module.named_children():
        if isinstance(child, nn.Linear):
            setattr(module, name, CustomLinearNumPy(child.in_features, child.out_features, bias=child.bias is not None))
        elif isinstance(child, nn.Sequential) or isinstance(child, InvertedResidual):
            replace_linear_with_numpy(child)
        elif isinstance(child, nn.ModuleList):
            for i, submodule in enumerate(child):
                replace_linear_with_numpy(submodule)

def replace_operations_in_mobilenet_v2(model):
    replace_conv2d_with_numpy(model)
    replace_linear_with_numpy(model)


'''
replace the conv2d and linear operations in the mobilenet_v2_quantized model with numpy operations
'''
def replace_conv2d_with_numpy(module):
    for name, child in module.named_children():
        if isinstance(child, QuantConv):
            new_conv = QCustomConv2dNumPy(child.in_channels, child.out_channels, 
                                         child.kernel_size, stride=child.stride,
                                         padding=child.padding, dilation=child.dilation,
                                         groups=child.groups, bias=child.bias is not None)
            # 复制权重和偏置
            new_conv.weight.data = child.weight.data
            if child.bias is not None:
                new_conv.bias.data = child.bias.data
            setattr(module, name, new_conv)
        elif isinstance(child, BNQConv):
            new_conv = QCustomBNConv2dNumPy(child.in_channels, child.out_channels, 
                                         child.kernel_size, stride=child.stride,
                                         padding=child.padding, dilation=child.dilation,
                                         groups=child.groups, bias=child.bias is not None)
            # 复制权重和偏置
            new_conv.weight.data = child.weight.data
            if child.bias is not None:
                new_conv.bias.data = child.bias.data
            setattr(module, name, new_conv)
        elif isinstance(child, (nn.Sequential, InvertedResidual, QuantizedInvertedResidual, QuantizedActivation)):
            replace_conv2d_with_numpy(child)
        elif isinstance(child, nn.ModuleList):
            for i, submodule in enumerate(child):
                replace_conv2d_with_numpy(submodule)

def replace_linear_with_numpy(module):
    for name, child in module.named_children():
        if isinstance(child, QuantLinear):
            new_linear = QCustomLinearNumPy(child.in_features, child.out_features, bias=child.bias is not None)
            # 复制权重和偏置
            new_linear.weight.data = child.weight.data
            if child.bias is not None:
                new_linear.bias.data = child.bias.data
            setattr(module, name, new_linear)
        elif isinstance(child, (nn.Sequential, InvertedResidual, QuantizedInvertedResidual, QuantizedActivation)):
            replace_linear_with_numpy(child)
        elif isinstance(child, nn.ModuleList):
            for i, submodule in enumerate(child):
                replace_linear_with_numpy(submodule)

'''
replace the conv2d and linear operations in the mobilenet_v2_quantized model with cupy operations
'''
def replace_conv2d_with_cupy(module):
    for name, child in module.named_children():
        if isinstance(child, QuantConv):
            new_conv = QCustomConv2dCuPy(child.in_channels, child.out_channels, 
                                         child.kernel_size, stride=child.stride,
                                         padding=child.padding, dilation=child.dilation,
                                         groups=child.groups, bias=child.bias is not None)
            # 复制权重和偏置
            new_conv.weight.data = child.weight.data
            if child.bias is not None:
                new_conv.bias.data = child.bias.data
            new_conv = new_conv.to(child.weight.device)
            setattr(module, name, new_conv)
        elif isinstance(child, BNQConv):
            new_conv = QCustomBNConv2dCuPy(child.in_channels, child.out_channels, 
                                         child.kernel_size, stride=child.stride,
                                         padding=child.padding, dilation=child.dilation,
                                         groups=child.groups, bias=child.bias is not None)
            # 复制权重和偏置
            new_conv.weight.data = child.weight.data
            if child.bias is not None:
                new_conv.bias.data = child.bias.data
            new_conv = new_conv.to(child.weight.device)
            setattr(module, name, new_conv)
        elif isinstance(child, (nn.Sequential, InvertedResidual, QuantizedInvertedResidual, QuantizedActivation)):
            replace_conv2d_with_cupy(child)
        elif isinstance(child, nn.ModuleList):
            for i, submodule in enumerate(child):
                replace_conv2d_with_cupy(submodule)

def replace_linear_with_cupy(module):
    for name, child in module.named_children():
        if isinstance(child, QuantLinear):
            new_linear = QCustomLinearCuPy(child.in_features, child.out_features, bias=child.bias is not None)
            # 复制权重和偏置
            new_linear.weight.data = child.weight.data
            if child.bias is not None:
                new_linear.bias.data = child.bias.data
            new_linear = new_linear.to(child.weight.device)
            setattr(module, name, new_linear)
        elif isinstance(child, (nn.Sequential, InvertedResidual, QuantizedInvertedResidual, QuantizedActivation)):
            replace_linear_with_cupy(child)
        elif isinstance(child, nn.ModuleList):
            for i, submodule in enumerate(child):
                replace_linear_with_cupy(submodule)

'''
replace the conv2d and linear operations in the mobilenet_v2_quantized model with torch operations
'''         
def create_and_copy_conv(child, new_conv_class, **quant_params):
    device = child.weight.device
    dtype = child.weight.dtype
    if isinstance(child, BNQConv):
        new_conv = new_conv_class(
            child.in_channels, child.out_channels, 
            child.kernel_size, stride=child.stride,
            padding=child.padding, dilation=child.dilation,
            groups=child.groups, bias=child.bias is not None, **quant_params
        ).to(device=device, dtype=dtype)
        new_conv.gamma.data = child.gamma.data
        new_conv.beta.data = child.beta.data
        new_conv.running_mean.data = child.running_mean.data
        new_conv.running_var.data = child.running_var.data
        new_conv.epsilon = child.epsilon
    else:
        new_conv = new_conv_class(
            child.in_channels, child.out_channels, 
            child.kernel_size, stride=child.stride,
            padding=child.padding, dilation=child.dilation,
            groups=child.groups, bias=child.bias is not None, **quant_params
        ).to(device=device, dtype=dtype)
    # 复制权重和偏置
    new_conv.weight.data = child.weight.data
    if child.bias is not None:
        new_conv.bias.data = child.bias.data
    new_conv.padding_mode = child.padding_mode
    return new_conv

def create_and_copy_linear(child, new_linear_class, **quant_params):
    device = child.weight.device
    dtype = child.weight.dtype
    new_linear = new_linear_class(
        child.in_features, child.out_features, bias=child.bias is not None, **quant_params
    ).to(device=device, dtype=dtype)
    # 复制权重和偏置
    new_linear.weight.data = child.weight.data
    if child.bias is not None:
        new_linear.bias.data = child.bias.data
    return new_linear

def replace_conv2d_with_torch(module, **quant_params):
    for name, child in module.named_children():
        if isinstance(child, QuantConv):
            new_conv = create_and_copy_conv(child, QCustomConv2dTorch, **quant_params)
            setattr(module, name, new_conv)
        elif isinstance(child, BNQConv):
            new_conv = create_and_copy_conv(child, QCustomBNConv2dTorch, **quant_params)
            setattr(module, name, new_conv)
        elif isinstance(child, (nn.Sequential, InvertedResidual, QuantizedInvertedResidual, QuantizedActivation)):
            # new_child = replace_conv2d_with_torch(child)
            # setattr(module, name, new_child)
            replace_conv2d_with_torch(child)
        elif isinstance(child, nn.ModuleList):
            copychild = copy.deepcopy(child)
            for i, submodule in enumerate(copychild):
                new_submodule = replace_conv2d_with_torch(submodule)
                child[i] = new_submodule

def replace_linear_with_torch(module, **quant_params):
    for name, child in module.named_children():
        if isinstance(child, QuantLinear):
            new_linear = create_and_copy_linear(child, QCustomLinearTorch, **quant_params)
            setattr(module, name, new_linear)
        elif isinstance(child, (nn.Sequential, InvertedResidual, QuantizedInvertedResidual, QuantizedActivation)):
            # new_child = replace_linear_with_torch(child)
            # setattr(module, name, new_child)
            replace_linear_with_torch(child)
        elif isinstance(child, nn.ModuleList):
            copychild = copy.deepcopy(child)
            for i, submodule in enumerate(copychild):
                new_submodule = replace_linear_with_torch(submodule)
                child[i] = new_submodule
  

def replace_operations_in_mobilenet_v2_quantized(model, **quant_params):
    # replace_conv2d_with_numpy(model)
    # replace_linear_with_numpy(model)
    # replace_conv2d_with_cupy(model)
    # replace_linear_with_cupy(model)
    replace_conv2d_with_torch(model, **quant_params)
    replace_linear_with_torch(model, **quant_params)

# Example usage
if __name__ == "__main__":
    from models.mobilenet_v2_quantized import mobilenetv2_quantized  # Add this import
    model = mobilenetv2_quantized(pretrained=True, model_dir='/home/zou/codes/FP8-quantization/model_dir/mobilenet_v2.pth.tar')
    replace_operations_in_mobilenet_v2_quantized(model)

    print(model)


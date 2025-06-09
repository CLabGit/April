import time

import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F

import cupy as cp

from approx_matmul_whole_v1 import *

class CustomConv2dNumPy_v3(nn.Conv2d):
    def __init__(self, in_channels, out_channels, kernel_size, stride=1, padding=0, dilation=1, groups=1, bias=True):
        super(CustomConv2dNumPy_v3, self).__init__(
            in_channels, out_channels, kernel_size, stride, padding, dilation, groups, bias)
        
        self.use_bias = bias is not None

    # MARK: This im2col is fast enough
    def im2col(self, input_data, kernel_height, kernel_width, stride, padding, dilation):
        batch_size, channels, height, width = input_data.shape
        
        # 计算输出尺寸
        out_height = (height + 2 * padding[0] - dilation[0] * (kernel_height - 1) - 1) // stride[0] + 1
        out_width = (width + 2 * padding[1] - dilation[1] * (kernel_width - 1) - 1) // stride[1] + 1
        
        # 填充输入
        padded_input = np.pad(input_data, ((0, 0), (0, 0), (padding[0], padding[0]), (padding[1], padding[1])), mode='constant')
        
        # 初始化输出数组
        col = np.zeros((batch_size, channels, kernel_height, kernel_width, out_height, out_width))
        
        # 填充输出数组
        for y in range(kernel_height):
            y_max = y + stride[0] * out_height
            for x in range(kernel_width):
                x_max = x + stride[1] * out_width
                col[:, :, y, x, :, :] = padded_input[:, :, y:y_max:stride[0], x:x_max:stride[1]]
        
        # 重塑数组以匹配矩阵乘法的要求
        col = col.transpose(0, 4, 5, 1, 2, 3).reshape(batch_size * out_height * out_width, -1)
        
        return col


    def multiply(self, x, y):
        return x * y


    def forward(self, input):
        # 确保输入是连续的
        input = input.contiguous()
        
        # 将输入转换为NumPy数组
        input_np = input.detach().cpu().numpy()
        weight_np = self.weight.detach().cpu().numpy()
        
        batch_size, in_channels, in_height, in_width = input_np.shape
        out_channels, in_channels_per_group, kernel_height, kernel_width = weight_np.shape
        
        # 使用im2col将输入转换为列
        input_col = self.im2col(input_np, kernel_height, kernel_width, self.stride, self.padding, self.dilation)

        # 重塑权重以匹配矩阵乘法的要求
        weight_col = weight_np.reshape(out_channels, -1)

        # print(input_col.shape)
        # print(weight_col.shape)

        # 执行矩阵乘法 (标准版)
        # output = np.matmul(input_col, weight_col.T)

        # 执行矩阵乘法（抽离版）
        weight_col_T = weight_col.T
        output = self.multiply(input_col[:, :, np.newaxis], weight_col_T[np.newaxis, :, :]).sum(axis=1)


        # 计算输出尺寸
        out_height = (in_height + 2 * self.padding[0] - self.dilation[0] * (kernel_height - 1) - 1) // self.stride[0] + 1
        out_width = (in_width + 2 * self.padding[1] - self.dilation[1] * (kernel_width - 1) - 1) // self.stride[1] + 1
        
        # 重塑输出
        output = output.reshape(batch_size, out_height, out_width, out_channels).transpose(0, 3, 1, 2)
        
        # 添加偏置（如果有）
        if self.use_bias:
            bias_np = self.bias.detach().cpu().numpy()
            output += bias_np.reshape(1, -1, 1, 1)
        
        # 将结果转回PyTorch张量
        return torch.from_numpy(output).to(input.device).type(input.dtype)


class CustomConv2dCuPy_v3(nn.Conv2d):
    def __init__(self, in_channels, out_channels, kernel_size, stride=1, padding=0, dilation=1, groups=1, bias=True):
        super(CustomConv2dCuPy_v3, self).__init__(
            in_channels, out_channels, kernel_size, stride, padding, dilation, groups, bias)
        
        self.use_bias = bias is not None

    # MARK: This im2col is fast enough
    def im2col(self, input_data, kernel_height, kernel_width, stride, padding, dilation):
        batch_size, channels, height, width = input_data.shape
        
        # 计算输出尺寸
        out_height = (height + 2 * padding[0] - dilation[0] * (kernel_height - 1) - 1) // stride[0] + 1
        out_width = (width + 2 * padding[1] - dilation[1] * (kernel_width - 1) - 1) // stride[1] + 1
        
        # 填充输入
        padded_input = cp.pad(input_data, ((0, 0), (0, 0), (padding[0], padding[0]), (padding[1], padding[1])), mode='constant')
        
        # 初始化输出数组
        col = cp.zeros((batch_size, channels, kernel_height, kernel_width, out_height, out_width))
        
        # 填充输出数组
        for y in range(kernel_height):
            y_max = y * dilation[0] + out_height * stride[0]
            for x in range(kernel_width):
                x_max = x * dilation[1] + out_width * stride[1]
                col[:, :, y, x, :, :] = padded_input[:, :, y*dilation[0]:y_max:stride[0], x*dilation[1]:x_max:stride[1]]
        
        # 重塑数组以匹配矩阵乘法的要求
        col = col.transpose(0, 4, 5, 1, 2, 3).reshape(batch_size * out_height * out_width, -1)
        
        return col


    def multiply(self, x, y):
        # return cp.multiply(x, y)
        return cp.matmul(x, y)

    def forward(self, input):
        # 确保输入是连续的
        input = input.contiguous()
        
        # 直接将 PyTorch tensor 转换为 CuPy array
        input_cp = cp.asarray(input.detach())
        weight_cp = cp.asarray(self.weight.detach())
        
        batch_size, in_channels, in_height, in_width = input_cp.shape
        out_channels, in_channels_per_group, kernel_height, kernel_width = weight_cp.shape
        
        # Compute output dimensions
        out_height = (in_height + 2 * self.padding[0] - self.dilation[0] * (kernel_height - 1) - 1) // self.stride[0] + 1
        out_width = (in_width + 2 * self.padding[1] - self.dilation[1] * (kernel_width - 1) - 1) // self.stride[1] + 1
        
        # Reshape input using im2col
        input_col = self.im2col(input_cp, kernel_height, kernel_width, self.stride, self.padding, self.dilation)
        # input_col shape: (batch_size * out_height * out_width, in_channels * kernel_height * kernel_width)
        
        # Handle grouped convolution
        group_in_channels = in_channels // self.groups
        group_out_channels = out_channels // self.groups
        
        # Reshape weights
        weight_col = weight_cp.reshape(out_channels, -1)  # Shape: (out_channels, in_channels_per_group * kernel_height * kernel_width)
        
        # Initialize output
        output = cp.zeros((batch_size * out_height * out_width, out_channels))
        
        for g in range(self.groups):
            # Indices for input
            start_in = g * group_in_channels * kernel_height * kernel_width
            end_in = (g + 1) * group_in_channels * kernel_height * kernel_width
            input_group = input_col[:, start_in:end_in]
            
            # Indices for output channels
            start_out = g * group_out_channels
            end_out = (g + 1) * group_out_channels
            weight_group = weight_col[start_out:end_out, :]  # Shape: (group_out_channels, in_channels_per_group * kernel_height * kernel_width)
            
            # Perform matrix multiplication
            # output_group = self.multiply(input_group[:, :, np.newaxis], weight_group.T[np.newaxis, :, :]).sum(axis=1)
            output_group = self.multiply(input_group, weight_group.T)
            
            # Store the output
            output[:, start_out:end_out] = output_group
        
        # Reshape output
        output = output.reshape(batch_size, out_height, out_width, out_channels)
        output = output.transpose(0, 3, 1, 2)  # Shape: (batch_size, out_channels, out_height, out_width)
        
        if self.use_bias:
            bias_cp = cp.asarray(self.bias.detach())
            output += bias_cp.reshape(1, -1, 1, 1)
        
        return torch.as_tensor(output, device=input.device, dtype=input.dtype)
        
        
    # def forward(self, input):
    #     # 确保输入是连续的
    #     input = input.contiguous()
        
    #     # 直接将 PyTorch tensor 转换为 CuPy array
    #     input_cp = cp.asarray(input.detach())
    #     weight_cp = cp.asarray(self.weight.detach())
        
    #     batch_size, in_channels, in_height, in_width = input_cp.shape
    #     out_channels, in_channels_per_group, kernel_height, kernel_width = weight_cp.shape
        
    #     input_col = self.im2col(input_cp, kernel_height, kernel_width, self.stride, self.padding, self.dilation)
    #     weight_col = weight_cp.reshape(out_channels, -1)
        
    #     weight_col_T = weight_col.T
    #     output = self.multiply(input_col[:, :, cp.newaxis], weight_col_T[cp.newaxis, :, :]).sum(axis=1)
        
    #     out_height = (in_height + 2 * self.padding[0] - self.dilation[0] * (kernel_height - 1) - 1) // self.stride[0] + 1
    #     out_width = (in_width + 2 * self.padding[1] - self.dilation[1] * (kernel_width - 1) - 1) // self.stride[1] + 1
        
    #     output = output.reshape(batch_size, out_height, out_width, out_channels).transpose(0, 3, 1, 2)
        
    #     if self.use_bias:
    #         bias_cp = cp.asarray(self.bias.detach())
    #         output += bias_cp.reshape(1, -1, 1, 1)
        
    #     return torch.as_tensor(output, device=input.device, dtype=input.dtype)


class QCustomConv2dNumPy(nn.Conv2d):
    def __init__(self, in_channels, out_channels, kernel_size, stride=1, padding=0, dilation=1, groups=1, bias=True):
        super(QCustomConv2dNumPy, self).__init__(
            in_channels, out_channels, kernel_size, stride, padding, dilation, groups, bias)
        
        self.use_bias = bias is not None
        
    # MARK: This im2col is fast enough
    def im2col(self, input_data, kernel_height, kernel_width, stride, padding, dilation):
        batch_size, channels, height, width = input_data.shape
        
        # 计算输出尺寸
        out_height = (height + 2 * padding[0] - dilation[0] * (kernel_height - 1) - 1) // stride[0] + 1
        out_width = (width + 2 * padding[1] - dilation[1] * (kernel_width - 1) - 1) // stride[1] + 1
        
        # 填充输入
        padded_input = np.pad(input_data, ((0, 0), (0, 0), (padding[0], padding[0]), (padding[1], padding[1])), mode='constant')
        
        # 初始化输出数组
        col = np.zeros((batch_size, channels, kernel_height, kernel_width, out_height, out_width))
        
        # 填充输出数组
        for y in range(kernel_height):
            y_max = y * dilation[0] + out_height * stride[0]
            for x in range(kernel_width):
                x_max = x * dilation[1] + out_width * stride[1]
                col[:, :, y, x, :, :] = padded_input[:, :, y*dilation[0]:y_max:stride[0], x*dilation[1]:x_max:stride[1]]
        
        # 重塑数组以匹配矩阵乘法的要求
        col = col.transpose(0, 4, 5, 1, 2, 3).reshape(batch_size * out_height * out_width, -1)
        
        return col
    
    def multiply(self, x, y):
        # return np.multiply(x, y)
        return np.matmul(x, y)
    
    def run_forward(self, x, weight, bias, offsets=None):
        x = x.contiguous()
        # Convert input to NumPy arrays
        input_np = x.detach().cpu().numpy()
        weight_np = weight.detach().cpu().numpy()
        
        batch_size, in_channels, in_height, in_width = input_np.shape
        out_channels, in_channels_per_group, kernel_height, kernel_width = weight_np.shape
        
        # Compute output dimensions
        out_height = (in_height + 2 * self.padding[0] - self.dilation[0] * (kernel_height - 1) - 1) // self.stride[0] + 1
        out_width = (in_width + 2 * self.padding[1] - self.dilation[1] * (kernel_width - 1) - 1) // self.stride[1] + 1
        
        # Reshape input using im2col
        input_col = self.im2col(input_np, kernel_height, kernel_width, self.stride, self.padding, self.dilation)
        # input_col shape: (batch_size * out_height * out_width, in_channels * kernel_height * kernel_width)
        
        # Handle grouped convolution
        group_in_channels = in_channels // self.groups
        group_out_channels = out_channels // self.groups
        
        # Reshape weights
        weight_col = weight_np.reshape(out_channels, -1)  # Shape: (out_channels, in_channels_per_group * kernel_height * kernel_width)
        
        # Initialize output
        output = np.zeros((batch_size * out_height * out_width, out_channels))
        
        for g in range(self.groups):
            # Indices for input
            start_in = g * group_in_channels * kernel_height * kernel_width
            end_in = (g + 1) * group_in_channels * kernel_height * kernel_width
            input_group = input_col[:, start_in:end_in]
            
            # Indices for output channels
            start_out = g * group_out_channels
            end_out = (g + 1) * group_out_channels
            weight_group = weight_col[start_out:end_out, :]  # Shape: (group_out_channels, in_channels_per_group * kernel_height * kernel_width)
            
            # Perform matrix multiplication
            # output_group = self.multiply(input_group[:, :, np.newaxis], weight_group.T[np.newaxis, :, :]).sum(axis=1)
            output_group = self.multiply(input_group, weight_group.T)

            # Store the output
            output[:, start_out:end_out] = output_group
        
        # Reshape output
        output = output.reshape(batch_size, out_height, out_width, out_channels)
        output = output.transpose(0, 3, 1, 2)  # Shape: (batch_size, out_channels, out_height, out_width)
        
        # Add bias if needed
        if bias is not None:
            bias_np = bias.detach().cpu().numpy()
            output += bias_np.reshape(1, -1, 1, 1)
        
        # Convert back to torch tensor
        return torch.from_numpy(output).to(x.device).type(x.dtype)
    
    def forward(self, input):
        return self.run_forward(input, self.weight, self.bias)

class QCustomConv2dNumPy_approx(QCustomConv2dNumPy):
    def __init__(self, in_channels, out_channels, kernel_size, stride=1, padding=0, dilation=1, groups=1, bias=True):
        super(QCustomConv2dNumPy_approx, self).__init__(
            in_channels, out_channels, kernel_size, stride, padding, dilation, groups, bias)
        
        self.use_bias = bias is not None
    
    def multiply(self, x, y):
        expo_width = 2
        mant_width = 5
        withComp = True
        
        comp_table_NN = get_comp_table_NN(expo_width, mant_width, withComp)
        custom_result_vectorize = custom_matmul_vectorize(
            x, y, 
            expo_width, 
            mant_width, 
            comp_table_NN   = comp_table_NN, 
            sim_hw_add_OFUF = False, 
            with_OF_opt     = False, 
            with_UF_opt     = False, 
            debug_mode      = False
            # debug_mode      = True 
        )
        return custom_result_vectorize
    
    def forward(self, input):
        return self.run_forward(input, self.weight, self.bias)
    

class QCustomConv2dTorch(nn.Conv2d):
    def __init__(self, in_channels, out_channels, kernel_size, stride=1, padding=0, dilation=1, groups=1, bias=True):
        super(QCustomConv2dTorch, self).__init__(
            in_channels, out_channels, kernel_size, stride, padding, dilation, groups, bias)
        
        self.use_bias = bias is not None
        
    def im2col(self, input_data, kernel_height, kernel_width, stride, padding, dilation):
        # 使用 F.unfold 实现 im2col
        # 输入形状: (batch_size, channels, height, width)
        # unfold 后的形状: (batch_size, channels * kernel_height * kernel_width, L)
        # 其中 L = 输出的高度 * 输出的宽度
        input_unfold = F.unfold(input_data, 
                                kernel_size=(kernel_height, kernel_width),
                                dilation=dilation,
                                padding=padding,
                                stride=stride)
        return input_unfold  # 形状: (batch_size, channels * kernel_height * kernel_width, L)
    
    
    def multiply(self, x, y):
        return torch.matmul(x, y)
    
    def run_forward(self, x, weight, bias, offsets=None):
        batch_size, in_channels, in_height, in_width = x.shape
        out_channels, _, kernel_height, kernel_width = weight.shape
        
        # 计算输出尺寸
        out_height = (in_height + 2 * self.padding[0] - self.dilation[0] * (kernel_height - 1) - 1) // self.stride[0] + 1
        out_width = (in_width + 2 * self.padding[1] - self.dilation[1] * (kernel_width - 1) - 1) // self.stride[1] + 1
        
        # 展开输入张量
        input_unfold = self.im2col(x, kernel_height, kernel_width, self.stride, self.padding, self.dilation)
        # 形状: (batch_size, in_channels * kernel_height * kernel_width, out_height * out_width)
        
        # 处理分组卷积
        groups = self.groups
        group_in_channels = in_channels // groups
        group_out_channels = out_channels // groups
        
        # 重塑输入张量以适应分组
        input_unfold = input_unfold.view(batch_size, groups, group_in_channels * kernel_height * kernel_width, -1)
        # 形状: (batch_size, groups, group_in_channels * kernel_height * kernel_width, out_height * out_width)
        
        # 重塑权重张量
        weight = weight.view(groups, group_out_channels, group_in_channels * kernel_height * kernel_width)
        # 形状: (groups, group_out_channels, group_in_channels * kernel_height * kernel_width)
        
        # 执行批量矩阵乘法
        # 首先交换 input_unfold 的最后两个维度以匹配矩阵乘法要求
        input_unfold = input_unfold.permute(0, 1, 3, 2)
        # 形状: (batch_size, groups, out_height * out_width, group_in_channels * kernel_height * kernel_width)
        
        # 进行矩阵乘法
        # output = torch.matmul(input_unfold, weight.transpose(2, 1))
        output = self.multiply(input_unfold, weight.transpose(2, 1))
        # 形状: (batch_size, groups, out_height * out_width, group_out_channels)
        
        # 调整输出形状
        output = output.permute(0, 1, 3, 2).contiguous()
        # 形状: (batch_size, groups, group_out_channels, out_height * out_width)
        
        output = output.view(batch_size, out_channels, out_height, out_width)
        # 形状: (batch_size, out_channels, out_height, out_width)
        
        # 添加偏置（如果有）
        if bias is not None:
            output += bias.view(1, -1, 1, 1)
        
        return output
    
    def forward(self, input):
        return self.run_forward(input, self.weight, self.bias)


def compare_conv2d_implementations(batch_size, in_channels, in_height, in_width, out_channels, kernel_size, stride, padding, dilation, groups):
    
    # Create instances of both convolution layers
    standard_conv = nn.Conv2d(in_channels, out_channels, kernel_size, stride, padding, dilation, groups)
    # custom_conv = CustomConv2dNumPy_v3(in_channels, out_channels, kernel_size, stride, padding, dilation)
    custom_conv = QCustomConv2dNumPy(in_channels, out_channels, kernel_size, stride, padding, dilation, groups)
    
    cupy_conv = CustomConv2dCuPy_v3(in_channels, out_channels, kernel_size, stride, padding, dilation, groups)
    approx_conv = QCustomConv2dNumPy_approx(in_channels, out_channels, kernel_size, stride, padding, dilation, groups)
    
    torch_conv = QCustomConv2dTorch(in_channels, out_channels, kernel_size, stride, padding, dilation, groups)
    
    # Ensure both layers have the same weights and biases
    with torch.no_grad():
        custom_conv.weight.copy_(standard_conv.weight)
        custom_conv.bias.copy_(standard_conv.bias)
        cupy_conv.weight.copy_(standard_conv.weight)
        cupy_conv.bias.copy_(standard_conv.bias)
        approx_conv.weight.copy_(standard_conv.weight)
        approx_conv.bias.copy_(standard_conv.bias)
        torch_conv.weight.copy_(standard_conv.weight)
        torch_conv.bias.copy_(standard_conv.bias)

    # Create random input tensor
    # input_tensor = torch.randn(batch_size, in_channels, in_height, in_width)
    input_tensor = generate_input_tensor(batch_size, in_channels, in_height, in_width)

    # Forward pass through both layers
    warmup_epoch = 10
    test_epoch = 100
    with torch.no_grad():
        standard_start_time = time.time()
        for _ in range(test_epoch):
            standard_output = standard_conv(input_tensor)
        standard_end_time = time.time()
        standard_time = (standard_end_time - standard_start_time) / test_epoch

        custom_start_time = time.time()
        for _ in range(test_epoch):
            custom_output = custom_conv(input_tensor)
        custom_end_time = time.time()
        custom_time = (custom_end_time - custom_start_time) / test_epoch
        
        approx_start_time = time.time()
        for _ in range(test_epoch):
            approx_output = approx_conv(input_tensor)
        approx_end_time = time.time()
        approx_time = (approx_end_time - approx_start_time) / test_epoch
        
        torch_start_time = time.time()
        for _ in range(test_epoch):
            torch_output = torch_conv(input_tensor)
        torch_end_time = time.time()
        torch_time = (torch_end_time - torch_start_time) / test_epoch
        
        # warmup
        for _ in range(warmup_epoch):
            _ = cupy_conv(input_tensor)
        cupy_start_time = time.time()
        for _ in range(test_epoch):
            cupy_output = cupy_conv(input_tensor)
        cupy_end_time = time.time()
        cupy_time = (cupy_end_time - cupy_start_time) / test_epoch

    # Compare outputs
    abs_diff_np = torch.abs(custom_output - standard_output)
    max_diff_np = torch.max(abs_diff_np).item()
    mean_diff_np = torch.mean(abs_diff_np).item()
    relative_diff_np = torch.mean(abs_diff_np / (torch.abs(standard_output) + 1e-8)).item()
    
    abs_diff_approx = torch.abs(approx_output - standard_output)
    max_diff_approx = torch.max(abs_diff_approx).item()
    mean_diff_approx = torch.mean(abs_diff_approx).item()
    relative_diff_approx = torch.mean(abs_diff_approx / (torch.abs(standard_output) + 1e-8)).item()

    abs_diff_cp = torch.abs(cupy_output - standard_output)
    max_diff_cp = torch.max(abs_diff_cp).item()
    mean_diff_cp = torch.mean(abs_diff_cp).item()
    relative_diff_cp = torch.mean(abs_diff_cp / (torch.abs(standard_output) + 1e-8)).item()
    
    print(f"torch_output: {torch_output}")
    print(f"standard_output: {standard_output}")
    abs_diff_torch = torch.abs(torch_output - standard_output)
    max_diff_torch = torch.max(abs_diff_torch).item()
    mean_diff_torch = torch.mean(abs_diff_torch).item()
    relative_diff_torch = torch.mean(abs_diff_torch / (torch.abs(standard_output) + 1e-8)).item()
    # are_close = torch.allclose(torch_output, standard_output, rtol=1e-5, atol=1e-8)
    # print(f"Are the outputs close? {are_close}")
    # abs_diff_cp = 0
    # max_diff_cp = 0
    # mean_diff_cp = 0
    # relative_diff_cp = 0

    # print(f'Abs diff: {abs_diff_np}, {abs_diff_cp}')
    print(f"Max absolute difference: numpy: {max_diff_np}, cupy: {max_diff_cp}, approx: {max_diff_approx}, torch: {max_diff_torch}")
    print(f"Mean absolute difference: numpy: {mean_diff_np}, cupy: {mean_diff_cp}, approx: {mean_diff_approx}, torch: {mean_diff_torch}")
    print(f"Mean relative difference: numpy: {relative_diff_np}, cupy: {relative_diff_cp}, approx: {relative_diff_approx}, torch: {relative_diff_torch}")
    print(f"Standard time: {standard_time}")
    print(f"Custom time: {custom_time}")
    print(f"CuPy time: {cupy_time}")
    print(f"Approx time: {approx_time}")
    print(f"Torch time: {torch_time}")
    # cupy_time = 0
    return max_diff_np, max_diff_cp, mean_diff_np, mean_diff_cp, relative_diff_np, relative_diff_cp, standard_time, custom_time, cupy_time


def generate_input_tensor(batch_size, in_channels, in_height, in_width):
    expo_width = 2
    mant_width = 5


    fp_bias  = (2**(expo_width - 1)) - 1
    max_norm = (2**fp_bias) * (2 - 2**(-mant_width))
    min_norm = 2**(1 - fp_bias)
    shape = (batch_size, in_channels, in_height, in_width)
    
    abs_max = max_norm / 100     # No OF, No UF
    # abs_max = max_norm
    abs_min = min_norm
    return torch.from_numpy(random_numpy_matrix(shape, abs_min, abs_max)).to(torch.float32)





if __name__ == "__main__":

    # Test with different configurations
    configurations = [
        {'batch_size': 1, 'in_channels': 3, 'in_height': 32, 'in_width': 32, 'out_channels': 15, 'kernel_size': 3, 'stride': 1, 'padding': 1, 'dilation': 1, 'groups': 3},
        # {'batch_size': 4, 'in_channels': 64, 'in_height': 64, 'in_width': 64, 'out_channels': 128, 'kernel_size': 5, 'stride': 2, 'padding': 2, 'dilation': 1, 'groups': 1},
        # {'batch_size': 4, 'in_channels': 3, 'in_height': 224, 'in_width': 224, 'out_channels': 16, 'kernel_size': 3, 'stride': 1, 'padding': 1, 'dilation': 1, 'groups': 1},    # Don't care about dilation
    ]

    for i, config in enumerate(configurations):
        print(f"\nTest configuration {i + 1}:")
        compare_conv2d_implementations(**config)
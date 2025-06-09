import sys
import os

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))

# 将上级目录添加到 sys.path
sys.path.append(parent_dir)

import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import numpy as np
from quantization.quantizers.fp8_quantizer import *

def draw_hist(weights, name):
    weights_flattened = weights.view(-1).cpu().numpy()
    min_value = weights.min().item()
    max_value = weights.max().item()
    mean_value = weights.mean().item()
    std_value = weights.std().item()
    # 绘制直方图
    plt.figure()
    plt.hist(weights_flattened, bins=100)
    plt.title(f'Histogram of {name} Layer Weights')
    plt.xlabel('Weight value')
    plt.ylabel('Frequency')
    
    plt.axvline(min_value, color='r', linestyle='--', label=f'Min: {min_value:.4f}')
    plt.axvline(max_value, color='g', linestyle='--', label=f'Max: {max_value:.4f}')
    plt.axvline(mean_value, color='b', linestyle='--', label=f'Mean: {mean_value:.4f}')
    plt.axvline(mean_value + std_value, color='y', linestyle='--', label=f'Mean + Std: {mean_value + std_value:.4f}')
    plt.axvline(mean_value - std_value, color='y', linestyle='--', label=f'Mean - Std: {mean_value - std_value:.4f}')
   

# bias_values = [1, 2, 3, 4]
# n_bits = 8
# expo_bits_values = [2, 3, 4]
# expo_bits = 3

# # test = gen(n_bits, expo_bits, 1)
# # print(test)

# fig, axs = plt.subplots(len(bias_values), len(expo_bits_values), figsize=(10, 2 * len(bias_values)))

# for j, expo_bits in enumerate(expo_bits_values):
#     all_fp8_values = []
#     for bias in bias_values:
#         all_values = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias)
#         fp8_values = np.array(all_values)
#         all_fp8_values.append(fp8_values)

#     global_min = min(np.min(fp8) for fp8 in all_fp8_values)
#     global_max = max(np.max(fp8) for fp8 in all_fp8_values)



#     for i, bias in enumerate(bias_values):
#         fp8_values = all_fp8_values[i]
        
#         # 绘制FP8的值范围
#         axs[i][j].plot(fp8_values, np.zeros_like(fp8_values), 'o', markersize=2, label=f'Bias: {bias}')
#         axs[i][j].set_title(f"FP8 E{expo_bits}M{n_bits-1-expo_bits} Representation Range (Bias: {bias})")
#         axs[i][j].set_yticks([])  # 不显示Y轴
#         axs[i][j].axhline(0, color='black', linewidth=1)  # 添加X轴
#         axs[i][j].set_xlim([global_min - 0.05, global_max + 0.05])  # 设置相同的X轴范围
#         # axs[i][j].set_xscale('log', base=2)  # 设置x轴为以2为底的对数刻度
#         axs[i][j].set_xlabel("FP8 Values")
#         axs[i][j].legend()  # 添加图例

# plt.tight_layout()  # 自动调整子图间距
# plt.show()
n_bits = 8
'''
fixed bias
'''
expo_bits = 2
bias = 2 ** (expo_bits - 1)
fp8_5m2e_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias)
fp8_5m2e_whole = np.array(fp8_5m2e_whole)

expo_bits = 3
bias = 2 ** (expo_bits - 1)
fp8_4m3e_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias)
fp8_4m3e_whole = np.array(fp8_4m3e_whole)

expo_bits = 4
bias = 2 ** (expo_bits - 1)
fp8_3m4e_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias)
fp8_3m4e_whole = np.array(fp8_3m4e_whole)

expo_bits = 5
bias = 2 ** (expo_bits - 1)
fp8_2m5e_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias)
fp8_2m5e_whole = np.array(fp8_2m5e_whole)

uniform_quantization = np.arange(0, 128)
fp8_5m2e = fp8_5m2e_whole[fp8_5m2e_whole > 0]
fp8_4m3e = fp8_4m3e_whole[fp8_4m3e_whole > 0]
fp8_3m4e = fp8_3m4e_whole[fp8_3m4e_whole > 0]
fp8_2m5e = fp8_2m5e_whole[fp8_2m5e_whole > 0]

data_fp8 = [uniform_quantization, fp8_5m2e, fp8_4m3e, fp8_3m4e, fp8_2m5e]
labels_fp8 = ["Uniform quantization", "FP8 5M2E", "FP8 4M3E", "FP8 3M4E", "FP8 2M5E"]

'''
E2M5 flexible bias
'''
expo_bits = 2
bias1 = -1
fp8_5m2eb0_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias1)
fp8_5m2eb0_whole = np.array(fp8_5m2eb0_whole)

expo_bits = 2
bias2 = 1
fp8_5m2eb1_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias2)
fp8_5m2eb1_whole = np.array(fp8_5m2eb1_whole)

expo_bits = 2
bias3 = 3
fp8_5m2eb2_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias3)
fp8_5m2eb2_whole = np.array(fp8_5m2eb2_whole)

expo_bits = 2
bias4 = 5
fp8_5m2eb3_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias4)
fp8_5m2eb3_whole = np.array(fp8_5m2eb3_whole)


fp8_5m2eb0 = fp8_5m2eb0_whole[fp8_5m2eb0_whole > 0]
fp8_5m2eb1 = fp8_5m2eb1_whole[fp8_5m2eb1_whole > 0]
fp8_5m2eb2 = fp8_5m2eb2_whole[fp8_5m2eb2_whole > 0]
fp8_5m2eb3 = fp8_5m2eb3_whole[fp8_5m2eb3_whole > 0]

data_5m2e = [fp8_5m2eb0, fp8_5m2eb1, fp8_5m2eb2, fp8_5m2eb3]
labels_5m2e = [f"FP8 E2M5B{bias1}", f"FP8 E2M5B{bias2}", f"FP8 E2M5B{bias3}", f"FP8 E2M5B{bias4}"]

'''
E3M4 flexible bias
'''
expo_bits = 3

bias1 = 1
bias2 = 3
bias3 = 5
bias4 = 7

fp8_4m3eb1_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias1)
fp8_4m3eb1_whole = np.array(fp8_4m3eb1_whole)

fp8_4m3eb2_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias2)
fp8_4m3eb2_whole = np.array(fp8_4m3eb2_whole)

fp8_4m3eb3_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias3)
fp8_4m3eb3_whole = np.array(fp8_4m3eb3_whole)

fp8_4m3eb4_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias4)
fp8_4m3eb4_whole = np.array(fp8_4m3eb4_whole)

fp8_4m3eb1 = fp8_4m3eb1_whole[fp8_4m3eb1_whole > 0]
fp8_4m3eb2 = fp8_4m3eb2_whole[fp8_4m3eb2_whole > 0]
fp8_4m3eb3 = fp8_4m3eb3_whole[fp8_4m3eb3_whole > 0]
fp8_4m3eb4 = fp8_4m3eb4_whole[fp8_4m3eb4_whole > 0]

data_4m3e = [fp8_4m3eb1, fp8_4m3eb2, fp8_4m3eb3, fp8_4m3eb4]
labels_4m3e = [f"FP8 E3M4B{bias1}", f"FP8 E3M4B{bias2}", f"FP8 E3M4B{bias3}", f"FP8 E3M4B{bias4}"]

'''
E4M3 flexible bias
'''
expo_bits = 4

bias1 = 5
bias2 = 7
bias3 = 9
bias4 = 11

fp8_3m4eb1_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias1)
fp8_3m4eb1_whole = np.array(fp8_3m4eb1_whole)

fp8_3m4eb2_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias2)
fp8_3m4eb2_whole = np.array(fp8_3m4eb2_whole)

fp8_3m4eb3_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias3)
fp8_3m4eb3_whole = np.array(fp8_3m4eb3_whole)

fp8_3m4eb4_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias4)
fp8_3m4eb4_whole = np.array(fp8_3m4eb4_whole)

fp8_3m4eb1 = fp8_3m4eb1_whole[fp8_3m4eb1_whole > 0]
fp8_3m4eb2 = fp8_3m4eb2_whole[fp8_3m4eb2_whole > 0]
fp8_3m4eb3 = fp8_3m4eb3_whole[fp8_3m4eb3_whole > 0]
fp8_3m4eb4 = fp8_3m4eb4_whole[fp8_3m4eb4_whole > 0]

data_3m4e = [fp8_3m4eb1, fp8_3m4eb2, fp8_3m4eb3, fp8_3m4eb4]
labels_3m4e = [f"FP8 E4M3B{bias1}", f"FP8 E4M3B{bias2}", f"FP8 E4M3B{bias3}", f"FP8 E4M3B{bias4}"]

'''
E5M2 flexible bias
'''
expo_bits = 5

bias1 = 13
bias2 = 15
bias3 = 17
bias4 = 19

fp8_2m5eb1_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias1)
fp8_2m5eb1_whole = np.array(fp8_2m5eb1_whole)

fp8_2m5eb2_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias2)
fp8_2m5eb2_whole = np.array(fp8_2m5eb2_whole)

fp8_2m5eb3_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias3)
fp8_2m5eb3_whole = np.array(fp8_2m5eb3_whole)

fp8_2m5eb4_whole = gen(n_bits=n_bits, exponent_bits=expo_bits, bias=bias4)
fp8_2m5eb4_whole = np.array(fp8_2m5eb4_whole)

fp8_2m5eb1 = fp8_2m5eb1_whole[fp8_2m5eb1_whole > 0]
fp8_2m5eb2 = fp8_2m5eb2_whole[fp8_2m5eb2_whole > 0]
fp8_2m5eb3 = fp8_2m5eb3_whole[fp8_2m5eb3_whole > 0]
fp8_2m5eb4 = fp8_2m5eb4_whole[fp8_2m5eb4_whole > 0]

data_2m5e = [fp8_2m5eb1, fp8_2m5eb2, fp8_2m5eb3, fp8_2m5eb4]
labels_2m5e = [f"FP8 E5M2B{bias1}", f"FP8 E5M2B{bias2}", f"FP8 E5M2B{bias3}", f"FP8 E5M2B{bias4}"]


bins = np.logspace(-20, 16, num=50, base=2)  # 使用对数间隔的 bin


data = [data_fp8, data_5m2e, data_4m3e, data_3m4e, data_2m5e]
# data = [data_fp8, data_5m2e]
labels = [labels_fp8, labels_5m2e, labels_4m3e, labels_3m4e, labels_2m5e]
# labels = [labels_fp8, labels_5m2e]

max_y = 0
for fp_data in data:
    for d in fp_data:
        counts, _ = np.histogram(d, bins=bins)
        max_y = max(max_y, counts.max())

# 绘制直方图
fig, axs = plt.subplots(len(data), 1, figsize=(10, 2 * len(data)))
# plt.figure(figsize=(10, 3))
for i, fp_data in enumerate(data):
    ax = axs[i] if len(data) > 1 else axs  # 处理当只有一个子图的情况
    fp_labels = labels[i]
    for j, d in enumerate(fp_data):
        ax.hist(d, bins=bins, alpha=0.7, label=fp_labels[j], edgecolor='black', linewidth=0.5)

    ax.set_xscale('log')
    ax.set_ylim(0, max_y+3)  # 设置所有子图的 y 轴范围一致

    # 设置x轴刻度标签为2的指数形式
    def format_func(value, tick_number):
        if value > 0:
            return f'$2^{{{int(np.log2(value))}}}$'
        else:
            return ''

    ax.xaxis.set_major_formatter(FuncFormatter(format_func))

    if i > 0:
        ax.text(0.01, 0.95, f"E{i+1}M{6-i}_B*", transform=ax.transAxes,
                fontsize=10, verticalalignment='top', horizontalalignment='left',
                bbox=dict(facecolor='white', alpha=0.5, edgecolor='none'))
    if i == len(data) - 1:
        ax.set_xlabel("Value range (log scale)")
    # ax.set_xlabel("Value range (log scale)")
    ax.set_ylabel("Frequency")
    
    # ax.set_title(f"Histogram of {fp_labels}")
    ax.legend()

plt.tight_layout()  # 自动调整子图间距
plt.show()
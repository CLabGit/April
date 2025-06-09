import torch

from quantization.quantizers.fp8_quantizer import *


n_bits = 8
expo_bits = 3
mant_bits = n_bits - expo_bits - 1
# bias = 10

# all_values = gen(n_bits, expo_bits, bias)
# maxval = get_max_value(expo_bits, bias)
# print(f'maxval bias=10: {maxval}')
# print(f'all_values bias=10: {all_values}')
# print(f'len(all_values) bias=10: {len(all_values)}')

# bias = 8

# all_values = gen(n_bits, expo_bits, bias)
# maxval = get_max_value(expo_bits, bias)
# print(f'maxval bias=8: {maxval}')
# print(f'all_values bias=8: {all_values}')
# print(f'len(all_values) bias=8: {len(all_values)}')

# bias = 6

# all_values = gen(n_bits, expo_bits, bias)
# maxval = get_max_value(expo_bits, bias)
# print(f'maxval bias=6: {maxval}')
# print(f'all_values bias=6: {all_values}')
# print(f'len(all_values) bias=6: {len(all_values)}')

# bias = 4

# all_values = gen(n_bits, expo_bits, bias)
# maxval = get_max_value(expo_bits, bias)
# print(f'maxval bias=4: {maxval}')
# print(f'all_values bias=4: {all_values}')
# print(f'len(all_values) bias=4: {len(all_values)}')

# bias = 2

# all_values = gen(n_bits, expo_bits, bias)
# maxval = get_max_value(expo_bits, bias)
# print(f'maxval bias=2: {maxval}')
# print(f'all_values bias=2: {all_values}')
# print(f'len(all_values) bias=2: {len(all_values)}')

# bias = 0

# all_values = gen(n_bits, expo_bits, bias)
# maxval = get_max_value(expo_bits, bias)
# print(f'maxval bias=0: {maxval}')
# print(f'all_values bias=0: {all_values}')
# print(f'len(all_values) bias=0: {len(all_values)}')


# bias = 2 ** (expo_bits - 1)
bias = 10

all_values = gen(n_bits, expo_bits, bias)
maxval = get_max_value(expo_bits, bias)
all_values_tensor = torch.Tensor(all_values)
input_tensor = torch.randn(1, 5, 5)
tensor_maxval = torch.Tensor([maxval])
tensor_mantissa_bits = torch.Tensor([float(mant_bits)])
print(f"all_values bias={bias}: {all_values}")

result, tensor_bias = quantize_to_fp8_ste_MM(all_values_tensor, n_bits, tensor_maxval, tensor_mantissa_bits, 1)
print(all_values_tensor.numpy())
print(result.numpy(), tensor_bias.numpy())
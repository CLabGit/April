import torch
import timeit
import numpy as np

import time

# ===================================================== Copy from here ===================================================== #


def custom_matmul_vectorize(A, B, expo_width, mant_width,
                            comp_table_NN,
                            sim_hw_add_OFUF=False, with_OF_opt=False, with_UF_opt=False, debug_mode=False):
                            
    assert A.shape[1] == B.shape[0]

    # * Parameters preparing
    initial_time = time.time()
    fp_bias = (2**(expo_width - 1)) - 1
    bias_double = 2 * fp_bias
    max_norm = (2**fp_bias) * (2 - 2**(-mant_width))
    min_norm = 2**(1 - fp_bias)
    min_subnorm = (2**(1 - fp_bias)) * 2**(-mant_width)
    max_expo = 2**expo_width - 1
    max_mant = 2**mant_width - 1
    mant_scale = 2**mant_width
    max_norm_int = 2**(expo_width+mant_width) - 1
    OF_UF_mod = 2**(expo_width+mant_width)
    prepare_time = time.time() - initial_time

    clip_OF = True    # ! CHECKME

    # * View FP as Int
    initial_time = time.time()
    A_sign, A_expo, A_mant, A_valid = float_to_fpany_absint_vectorize_torch(A, fp_bias, max_norm, min_norm, min_subnorm, max_expo, max_mant, mant_scale, clip_OF=clip_OF, return_extract=True)
    B_sign, B_expo, B_mant, B_valid = float_to_fpany_absint_vectorize_torch(B, fp_bias, max_norm, min_norm, min_subnorm, max_expo, max_mant, mant_scale, clip_OF=clip_OF, return_extract=True)

    A_expo_mant_int = A_expo * mant_scale + A_mant
    B_expo_mant_int = B_expo * mant_scale + B_mant
    expo_mant_int_time = time.time() - initial_time

    B_neg = -((2 ** (expo_width-1) - 1) << mant_width)

    # * Approximate calculation
    initial_time = time.time()
    temp_result_int_3d = approx_mult(A_expo_mant_int.unsqueeze(2), B_expo_mant_int.unsqueeze(0), B_neg)
    approx_time = time.time() - initial_time

    # * Look up for compensation
    initial_time = time.time()
    comp_int = check_for_comp(A_mant.unsqueeze(2), B_mant.unsqueeze(0), comp_table_NN)
    comp_time = time.time() - initial_time

    # * Add the compensation
    initial_time = time.time()
    temp_result_int_3d = temp_result_int_3d + comp_int

    # * Overflow & Underflow Masking
    overflow_mask = (temp_result_int_3d > max_norm_int)
    underflow_mask = (temp_result_int_3d < 0)

    # * Simulate the Overflow & Underflow situation --- when using hardware adder to approximate multiplication
    if sim_hw_add_OFUF:
        # Use Modulo Operation to simulate Overflow & Underflow of hardware adder
        temp_result_int_3d = temp_result_int_3d % OF_UF_mod

        if with_OF_opt:
            temp_result_int_3d = torch.where(overflow_mask, torch.tensor(max_norm_int, device=A.device), temp_result_int_3d)

        if with_UF_opt:
            temp_result_int_3d = torch.where(underflow_mask, temp_result_int_3d % mant_scale, temp_result_int_3d)

    # * Sign of the result
    sign_result_3d = mult(A_sign.unsqueeze(2), B_sign.unsqueeze(0))

    # * View FP as Int -> View FP as FP
    approx_result_fp_3d = fpany_absint_to_float_vectorize_torch(sign=sign_result_3d, abs_int=temp_result_int_3d, expo=None, mant=None, fp_bias=fp_bias, mant_scale=mant_scale)

    # * 3d -> 2d
    approx_result_2d = approx_result_fp_3d.sum(dim=1)
    result_time = time.time() - initial_time
    
    time_dict = {
        "prepare_time": prepare_time,
        "expo_mant_int_time": expo_mant_int_time,
        "approx_time": approx_time,
        "comp_time": comp_time,
        "result_time": result_time,
    }
    # 按时间从长到短排序并输出
    sorted_times = sorted(time_dict.items(), key=lambda x: x[1], reverse=True)
    
    print("\nTime breakdown (sorted from longest to shortest):")
    for name, duration in sorted_times:
        print(f"{name:<20}: {duration} seconds")

    total_time = sum(time_dict.values())
    print(f"\nTotal time: {total_time} seconds")

    return approx_result_2d



def mult(x, y):
    return x * y


def custom_matmul(A, B):

    result_3d = mult(A.unsqueeze(2), B.unsqueeze(0))
    result_2d = result_3d.sum(dim=1)

    return result_2d


def approx_mult(x_int, y_int, B_neg):

    mult_result = x_int + y_int + B_neg
    
    return mult_result



def check_for_comp(x_mant, y_mant, comp_table_NN):
    """
    Check the comparison table for given mantissa values.
    
    Args:
    x_mant (torch.Tensor): Mantissa values for x, used as row indices
    y_mant (torch.Tensor): Mantissa values for y, used as column indices
    comp_table_NN (torch.Tensor): 2D comparison table
    
    Returns:
    torch.Tensor: Boolean tensor with comparison results
    """
    return comp_table_NN[x_mant.long(), y_mant.long()]



def float_to_fpany_absint_vectorize_torch(values, 
                                          fp_bias, max_norm, min_norm, min_subnorm, max_expo, max_mant, mant_scale, 
                                          clip_OF=True,
                                          return_extract=True
                                          ):
    """
    Vectorize Version of Generic Conversion: FP64 -> Custom Floating Point Binary.
    It will return each parts in int form.

    Args:
        values (torch.Tensor) : Floating-Point values (FP64 / FP32 / FP16) of the fp 
        fp_bias     (int)     : = (2**(expo_width - 1)) - 1
        max_norm    (float)   : = (2**bias) * (2 - 2**(-mant_width))
        min_norm    (float)   : = 2**(1 - bias)
        min_subnorm (float)   : = (2**(1 - bias)) * 2**(-mant_width)
        max_expo    (int)     : = 2**expo_width - 1
        max_mant    (int)     : = 2**mant_width - 1
        mant_scale  (int)     : = 2**mant_width
        clip_OF     (bool)    : Whether to clip the overflow value to max_norm or not. (default True)
        return_extract (bool) : Whether to return the expo & mant in separate or added way. 
    """
    device = values.device
    values = torch.as_tensor(values, dtype=torch.float32, device=device)  # ensure it's a torch tensor with float32 dtype

    
    sign = torch.where(values < 0, torch.tensor(-1, dtype=torch.float32, device=device), torch.tensor(1, dtype=torch.float32, device=device))
    
    abs_values = torch.abs(values)

    # Initialize
    expo = torch.zeros_like(values, dtype=torch.float32)
    mant = torch.zeros_like(values, dtype=torch.float32)

    # Masking
    zero_mask     = (abs_values == 0)
    subnorm_mask  = (abs_values >= min_subnorm) & (abs_values < min_norm)
    norm_mask     = (abs_values >= min_norm) & (abs_values <= max_norm)
    overflow_mask = (abs_values > max_norm)
    valid_mask    = zero_mask | subnorm_mask | norm_mask
    
    if clip_OF:
        view_as_norm_mask = norm_mask
    else:
        view_as_norm_mask = norm_mask | overflow_mask

    # * Norm (if not clipping overflow, then overflow can also be processed like norm)
    expo = torch.where(view_as_norm_mask, torch.floor(torch.log2(abs_values)), expo)
    mant = torch.where(view_as_norm_mask, 
                       torch.minimum(torch.round((abs_values / (2**expo) - 1) * mant_scale), 
                                     torch.tensor(max_mant, dtype=torch.float32, device=device)), 
                       mant)
    expo = torch.where(view_as_norm_mask, expo + fp_bias, expo)

    # * Sub-Norm
    mant = torch.where(subnorm_mask, 
                       torch.minimum(torch.round((abs_values / min_norm) * mant_scale), 
                                     torch.tensor(max_mant, dtype=torch.float32, device=device)), 
                       mant)

    # * Overflow
    if clip_OF:
        expo = torch.where(overflow_mask, torch.tensor(max_expo, dtype=torch.float32, device=device), expo)
        mant = torch.where(overflow_mask, torch.tensor(max_mant, dtype=torch.float32, device=device), mant)

    # * Valid data point
    valid = torch.where(valid_mask, torch.tensor(1, dtype=torch.float32, device=device), torch.tensor(0, dtype=torch.float32, device=device))

    # * Turn to Int
    expo = expo.to(torch.int32)
    mant = mant.to(torch.int32)

    if return_extract:
        return sign, expo, mant, valid
    else:
        return sign, expo * mant_scale + mant, valid




def fpany_absint_to_float_vectorize_torch(sign=None, abs_int=None, expo=None, mant=None, fp_bias=None, mant_scale=None):
    """
    Vectorize Version of Generic Conversion: Custom Floating Point Binary -> FP64

    Args:
        sign (torch.Tensor)    : Sign of the values (-1 or 1)
        abs_int (torch.Tensor) : Input tensor (FP view in absolute integer, abs_int = expo << mant_width + mant). If not given, use expo & mant.
        expo (torch.Tensor)    : Exponent tensor. If not given, use abs_int.
        mant (torch.Tensor)    : Mantissa tensor. If not given, use abs_int.
        fp_bias (int)          : The bias of the FP
        mant_scale (int)       : = 2**mant_width.
    """

    if abs_int is not None:
        abs_int = torch.as_tensor(abs_int)    # ensure it's a torch tensor
        # expo = abs_int // mant_scale
        expo = torch.div(abs_int, mant_scale, rounding_mode='floor')
        mant = abs_int % mant_scale
    else:
        expo = torch.as_tensor(expo)          # ensure it's a torch tensor
        mant = torch.as_tensor(mant)          # ensure it's a torch tensor

    subnorm_mask = (expo == 0)
    norm_mask = (expo > 0)

    values = torch.zeros_like(expo, dtype=torch.float32)
    
    values = torch.where(subnorm_mask, 2.0**(1-fp_bias) * (mant/mant_scale), values)
    values = torch.where(norm_mask, 2.0**(expo-fp_bias) * (1 + (mant/mant_scale)), values)

    if sign is not None:
        sign = torch.as_tensor(sign)  # ensure it's a torch tensor
        values = values * sign

    return values



def quant_to_fp_any_vectorize_torch(arr, expo_width, mant_width, clip_OF=True):
    """
    Quantize a PyTorch tensor to floating point representation with specified exponent and mantissa widths.

    Parameters:
    arr (torch.Tensor) : Input tensor to be quantized
    expo_width (int)   : Width of the exponent in bits
    mant_width (int)   : Width of the mantissa in bits
    clip_OF    (bool)  : Whether to clip the overflow value to max_norm or not. (default True)
                         If not, then the expo will actually extend to hold the overflow value.

    Returns:
    torch.Tensor: Quantized tensor with the same shape as input
    """

    # * Parameters preparing
    fp_bias     = (2**(expo_width - 1)) - 1
    max_norm    = (2**fp_bias) * (2 - 2**(-mant_width))
    min_norm    = 2**(1 - fp_bias)
    min_subnorm = (2**(1 - fp_bias)) * 2**(-mant_width)
    max_expo    = 2**expo_width - 1
    max_mant    = 2**mant_width - 1
    mant_scale  = 2**mant_width

    sign, expo, mant, valid = float_to_fpany_absint_vectorize_torch(
        arr, fp_bias, max_norm, min_norm, min_subnorm, max_expo, max_mant, mant_scale, clip_OF, return_extract=True
    )

    fp_values = fpany_absint_to_float_vectorize_torch(sign=sign, abs_int=None, expo=expo, mant=mant, fp_bias=fp_bias, mant_scale=mant_scale)

    return fp_values




# MARK: This is just a simplified method.
def get_comp_table_NN(expo_width, mant_width, withComp, dnsmp_factor, device):


    if ((expo_width, mant_width) == (4, 3)) and withComp:
        comp_table_NN = torch.tensor([
            [0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 1, 1, 0, 0],
            [0, 0, 0, 1, 1, 1, 1, 0],
            [0, 0, 1, 1, 1, 1, 1, 0],
            [0, 1, 1, 1, 1, 1, 0, 0],
            [0, 1, 1, 1, 1, 1, 0, 0],
            [0, 0, 1, 1, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0]
        ], dtype=torch.int32, device=device)

    elif ((expo_width, mant_width) == (3, 4)) and withComp:
        if dnsmp_factor == 3:
            comp_table_NN = torch.tensor([
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 0, 0],
                [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 0, 0],
                [0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 0, 0],
                [0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 0, 0],
                [0, 0, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 0, 0],
                [0, 0, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 0, 0],
                [0, 0, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0],
                [0, 0, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0],
                [0, 0, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0],
                [0, 0, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0],
                [0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
                [0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            ], dtype=torch.int32, device=device)
        elif dnsmp_factor >= 4:
            comp_table_NN = torch.tensor([
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0],
                [0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0],
                [0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 0],
                [0, 0, 0, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 0],
                [0, 0, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 0],
                [0, 0, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 0],
                [0, 0, 1, 1, 2, 2, 2, 3, 2, 2, 2, 1, 1, 1, 1, 0],
                [0, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 0, 0],
                [0, 1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0],
                [0, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 0, 0],
                [0, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0, 0],
                [0, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
                [0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0],
                [0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            ], dtype=torch.int32, device=device)


    elif ((expo_width, mant_width) == (2, 5)) and withComp:
        if dnsmp_factor == 3:
            comp_table_NN = torch.tensor([
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                [0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1],
                [0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1],
                [0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1],
                [0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1],
                [0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 4, 4, 4, 4, 2, 2, 2, 2, 1, 1, 1, 1],
                [0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 4, 4, 4, 4, 2, 2, 2, 2, 1, 1, 1, 1],
                [0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 4, 4, 4, 4, 2, 2, 2, 2, 1, 1, 1, 1],
                [0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 4, 4, 4, 4, 2, 2, 2, 2, 1, 1, 1, 1],
                [0, 0, 0, 0, 2, 2, 2, 2, 4, 4, 4, 4, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1],
                [0, 0, 0, 0, 2, 2, 2, 2, 4, 4, 4, 4, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1],
                [0, 0, 0, 0, 2, 2, 2, 2, 4, 4, 4, 4, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1],
                [0, 0, 0, 0, 2, 2, 2, 2, 4, 4, 4, 4, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1],
                [1, 1, 1, 1, 3, 3, 3, 3, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0],
                [1, 1, 1, 1, 3, 3, 3, 3, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0],
                [1, 1, 1, 1, 3, 3, 3, 3, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0],
                [1, 1, 1, 1, 3, 3, 3, 3, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0],
                [1, 1, 1, 1, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0],
                [1, 1, 1, 1, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0],
                [1, 1, 1, 1, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0],
                [1, 1, 1, 1, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0],
                [1, 1, 1, 1, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
                [1, 1, 1, 1, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
                [1, 1, 1, 1, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
                [1, 1, 1, 1, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
                [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            ], dtype=torch.int32, device=device)
        
        elif dnsmp_factor == 4:
            comp_table_NN = torch.tensor([
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 0, 0],
                [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 0, 0],
                [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 0, 0],
                [0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 3, 3, 2, 2, 2, 2, 0, 0],
                [0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 3, 3, 2, 2, 2, 2, 0, 0],
                [0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 2, 2, 1, 1, 0, 0],
                [0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 2, 2, 1, 1, 0, 0],
                [0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 2, 2, 2, 2, 1, 1, 0, 0],
                [0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 2, 2, 2, 2, 1, 1, 0, 0],
                [0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 2, 2, 2, 2, 1, 1, 0, 0],
                [0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 2, 2, 2, 2, 1, 1, 0, 0],
                [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 0, 0],
                [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 0, 0],
                [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0],
                [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0],
                [0, 0, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0],
                [0, 0, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0],
                [0, 0, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0],
                [0, 0, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0],
                [0, 0, 2, 2, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
                [0, 0, 2, 2, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
                [0, 0, 2, 2, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
                [0, 0, 2, 2, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
                [0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0],
                [0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0],
                [0, 0, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            ], dtype=torch.int32, device=device)

        elif dnsmp_factor >= 5:
            comp_table_NN = torch.tensor([
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 0],
                [0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 0],
                [0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 0],
                [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 0],
                [0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 3, 3, 2, 2, 2, 1, 1, 0],
                [0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 3, 3, 2, 2, 2, 1, 1, 0],
                [0, 0, 0, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 0],
                [0, 0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 1, 1, 1, 0],
                [0, 0, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1, 0],
                [0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1, 0],
                [0, 0, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 0],
                [0, 0, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 0],
                [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 0],
                [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 0],
                [0, 1, 1, 1, 2, 3, 3, 3, 4, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 3, 3, 3, 2, 2, 2, 2, 2, 1, 1, 1, 0, 0],
                [0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 4, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0],
                [0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 4, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0],
                [0, 1, 1, 2, 2, 3, 4, 4, 4, 5, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 0, 0],
                [0, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 0, 0],
                [0, 1, 1, 2, 3, 3, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0],
                [0, 1, 1, 2, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0, 0],
                [0, 1, 1, 2, 3, 3, 4, 4, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0],
                [0, 1, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
                [0, 1, 2, 2, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
                [0, 1, 2, 2, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0],
                [0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0],
                [0, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            ], dtype=torch.int32, device=device)

    else:
        comp_table_NN = torch.zeros((2**mant_width, 2**mant_width), dtype=torch.int32)

    return comp_table_NN


# ===================================================== Copy to here ===================================================== #



# def compare_accuracy_and_speed(size=(32, 32), num_runs=1000):
#     # 创建随机矩阵
#     A_torch = torch.rand(size)
#     B_torch = torch.rand(size)
#     A_numpy = A_torch.numpy()
#     B_numpy = B_torch.numpy()

#     # 测试准确度
#     custom_result = custom_matmul(A_torch, B_torch)
#     torch_result = torch.matmul(A_torch, B_torch)
#     numpy_result = np.matmul(A_numpy, B_numpy)

#     print("准确度比较:")
#     print("Custom vs Torch matmul difference:", torch.max(torch.abs(custom_result - torch_result)).item())
#     print("Custom vs Numpy matmul difference:", np.max(np.abs(custom_result.numpy() - numpy_result)))

#     # 测试速度
#     custom_time = timeit.timeit(lambda: custom_matmul(A_torch, B_torch), number=num_runs) / num_runs
#     torch_time = timeit.timeit(lambda: torch.matmul(A_torch, B_torch), number=num_runs) / num_runs
#     numpy_time = timeit.timeit(lambda: np.matmul(A_numpy, B_numpy), number=num_runs) / num_runs

#     print("\n速度比较 (平均 1000 次运行):")
#     print(f"Custom method: {custom_time:.6f} seconds")
#     print(f"Torch matmul: {torch_time:.6f} seconds")
#     print(f"Numpy matmul: {numpy_time:.6f} seconds")


# if __name__ == "__main__":

#     compare_accuracy_and_speed()






def random_numpy_matrix(shape, abs_min, abs_max, expo_width, mant_width):
    """
    Generate a random NumPy matrix with values in the range [abs_min, abs_max] and [-abs_max, -abs_min]
    
    Parameters:
    shape (tuple): The shape of the output matrix
    abs_min (float): The minimum absolute value
    abs_max (float): The maximum absolute value
    
    Returns:
    numpy.ndarray: A random matrix with the specified shape and value range
    """

    # 种子固定  
    np.random.seed(42)
    
    # Generate a random matrix in the range [-1, 1]
    random_matrix = np.random.rand(*shape) * 2 - 1
    
    # Create a mask for positive and negative values
    mask = random_matrix >= 0
    
    # Scale the matrix to the desired range
    scaled_matrix = np.where(mask, 
                             random_matrix * (abs_max - abs_min) + abs_min,
                             random_matrix * (abs_max - abs_min) - abs_min
                            )

    # Quantize the nums
    quantized_matrix = quant_to_fp_any_vectorize_torch(scaled_matrix, expo_width, mant_width, clip_OF=True)

    return quantized_matrix.numpy()





if __name__ == "__main__":

    expo_width = 2
    mant_width = 5

    # MARK: 如果讲这里都打开，似乎补偿已经没有什么多大的影响了，因为OF本身带来的误差已经很巨大了
    # sim_hw_add_OFUF = True
    # with_OF_opt     = True
    # with_UF_opt     = True
    # golden_clip_OF  = True

    # MARK: 如果这里都关闭，则误差补偿还是有挺大影响的，矩阵尺寸越大，修补与不修补的效果差别越大
    sim_hw_add_OFUF = False
    with_OF_opt     = False
    with_UF_opt     = False
    golden_clip_OF  = False


    # MARK: 只要计算过程中不出现 Overflow 和 Underflow，那么这个是有用的，用面积可以换来理论上的 0 误差
    # dnsmp_factor = 3
    # dnsmp_factor = 4
    dnsmp_factor = 5

    debug_mode = True
    # debug_mode = False


    fp_bias  = (2**(expo_width - 1)) - 1
    max_norm = (2**fp_bias) * (2 - 2**(-mant_width))
    min_norm = 2**(1 - fp_bias)
    
    # shape_A = (4, 4)
    # shape_B = (4, 4)
    # shape_A = (6, 6)
    # shape_B = (6, 6)
    # shape_A = (8, 8)
    # shape_B = (8, 8)
    # shape_A = (32, 32)
    # shape_B = (32, 32)
    shape_A = (128, 128)
    shape_B = (128, 128)

    # abs_max = max_norm / 800     # With UF
    # abs_max = max_norm / 100     # With UF
    # abs_max = max_norm / 10       # 
    # abs_max = max_norm / 4       # With OF
    # abs_max = max_norm
    # abs_min = min_norm * 2


    # MARK: For E3M4: No OF, No UF
    if (expo_width, mant_width) == (3, 4):
        abs_max = max_norm / 4
        abs_min = min_norm * 2

    # MARK: For E2M5: No OF, No UF
    if (expo_width, mant_width) == (2, 5):
        abs_max = max_norm / 2
        abs_min = min_norm


    print("abs_max =", abs_max)
    print("abs_min =", abs_min)

    # Generate quantized random matrix
    A = random_numpy_matrix(shape_A, abs_min, abs_max, expo_width, mant_width)
    B = random_numpy_matrix(shape_B, abs_min, abs_max, expo_width, mant_width)

    print("\nMatrix A =\n", A)
    print("\nMatrix B =\n", B)


    # * Golden Result (Quantization after sum)
    classical_result      = np.matmul(A, B)
    # custom_result_forloop = custom_matmul_single(A, B)

    # * Golden Result (Quantization before sum)
    golden_result_withquant_3d = mult(A[:, :, np.newaxis], B[np.newaxis, :, :])
    golden_result_withquant_3d = quant_to_fp_any_vectorize_torch(golden_result_withquant_3d, expo_width, mant_width, clip_OF=golden_clip_OF)    # ? Quantization before sum
    golden_result_withquant_2d = golden_result_withquant_3d.numpy().sum(axis=1)


    # * Approx Result (No Comp)
    custom_result_vectorize_nocomp = custom_matmul_vectorize(
        A, B, 
        expo_width, 
        mant_width, 
        comp_table_NN   = np.zeros((2**mant_width, 2**mant_width), dtype=int), 
        sim_hw_add_OFUF = sim_hw_add_OFUF, 
        with_OF_opt     = with_OF_opt, 
        with_UF_opt     = with_UF_opt, 
        debug_mode      = debug_mode
    )

    custom_result_vectorize_nocomp = custom_result_vectorize_nocomp.numpy()



    # * Error = Golden - Approx
    # df_err_before_comp, df_err_for_comp , df_err_after_comp, _ = \
    #     mant_table_fp_any_dualop_NN(expo_width, mant_width, operation="Mult", withComp=True, dnsmp_factor=dnsmp_factor)

    # print(df_err_for_comp)
    # comp_table_NN = df_err_for_comp.to_numpy(dtype=int)
    # # print(comp_table_NN)

    comp_table_NN = get_comp_table_NN(expo_width, mant_width, withComp=True, dnsmp_factor=dnsmp_factor)
    # print(comp_table_NN)


    # * Approx Result (with Comp)
    custom_result_vectorize_withcomp = custom_matmul_vectorize(
        A, B, 
        expo_width, 
        mant_width, 
        comp_table_NN   = comp_table_NN, 
        sim_hw_add_OFUF = sim_hw_add_OFUF, 
        with_OF_opt     = with_OF_opt, 
        with_UF_opt     = with_UF_opt, 
        debug_mode      = debug_mode
    )

    custom_result_vectorize_withcomp = custom_result_vectorize_withcomp.numpy()

    if debug_mode:
        print("\ngolden_result_withquant_3d =\n", golden_result_withquant_3d)


    # Error calculation
    # error_matrix = classical_result - custom_result_forloop
    # error_matrix = custom_result_forloop - custom_result_vectorize           # Golden result that doesn't do Quantization before sum


    # golden_result = classical_result                   # 有时候可能这个效果更好？
    golden_result = golden_result_withquant_2d
    print(f"\ngolden_result:\n {golden_result}")

    error_matrix_nocomp   = golden_result - custom_result_vectorize_nocomp        # Golden result that do Quantization before sum
    error_matrix_withcomp = golden_result - custom_result_vectorize_withcomp      # Golden result that do Quantization before sum

    print(f"\nerror_matrix_nocomp:\n {error_matrix_nocomp}")
    print(f"\nerror_matrix_withcomp:\n {error_matrix_withcomp}")


    error_nocomp   = np.abs(error_matrix_nocomp)
    error_withcomp = np.abs(error_matrix_withcomp)

    print(f"\nMax Error (No Comp): {np.max(error_nocomp)}")
    print(f"Max Error (With Comp): {np.max(error_withcomp)}")

    print(f"Mean Error (No Comp): {np.mean(error_nocomp)}")
    print(f"Mean Error (With Comp): {np.mean(error_withcomp)}")

    rmse_nocomp   = np.sqrt(np.mean(error_matrix_nocomp ** 2))
    rmse_withcomp = np.sqrt(np.mean(error_matrix_withcomp ** 2))
    print(f"RMSE (No Comp): {rmse_nocomp}")
    print(f"RMSE (With Comp): {rmse_withcomp}")


    if False:

        # 性能比较
        number = 100  # 运行次数

        classical_time        = timeit.timeit(lambda: np.matmul(A, B), number=number) / number
        custom_time_forloop   = timeit.timeit(lambda: custom_matmul_single(A, B), number=number) / number
        custom_time_vectorize = timeit.timeit(lambda: custom_matmul_vectorize(A, B, expo_width, mant_width), number=number) / number

        print(f"np.matmul 平均耗时: {classical_time:.6f} 秒")
        print(f"custom_matmul_single 平均耗时: {custom_time_forloop:.6f} 秒")
        print(f"custom_matmul_vectorize 平均耗时: {custom_time_vectorize:.6f} 秒")
        # print(f"性能比: {custom_time / classical_time:.2f}")
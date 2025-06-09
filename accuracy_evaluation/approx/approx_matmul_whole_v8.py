import torch
import timeit
import numpy as np
import random


# ===================================================== Copy from here ===================================================== #


def custom_matmul_vectorize(A, B, expo_width, mant_width,
                            custom_bias_A, custom_bias_B, custom_bias_R,
                            error_table_NN,
                            sim_hw_add_OFUF=False, with_OF_opt=False, with_UF_opt=False, golden_clip_OF=False,
                            double_quant=True,
                            debug_mode=False, self_check_mode=False
                            ):
                            
    assert A.shape[1] == B.shape[0]


    # * Parameters preparing
    A_param_dict = param_prepare(expo_width, mant_width, custom_bias=custom_bias_A, debug_mode=debug_mode)
    B_param_dict = param_prepare(expo_width, mant_width, custom_bias=custom_bias_B, debug_mode=debug_mode)
    R_param_dict = param_prepare(expo_width, mant_width, custom_bias=custom_bias_R, debug_mode=debug_mode)


    # * View FP as Int
    A_expo, A_mant = float_to_fpany_absint_torch(A_param_dict, A, clip_OF=False, return_extract=True)
    B_expo, B_mant = float_to_fpany_absint_torch(B_param_dict, B, clip_OF=False, return_extract=True)

    # print("\n A_expo=\n", A_expo)
    # print("\n B_expo=\n", B_expo)


    # * Golden
    golden_result_3d = A.unsqueeze(2) * B.unsqueeze(0)
    # print("\n golden_result_3d=\n", golden_result_3d.numpy())
    
    # Quantization between mult & accumulate
    if double_quant:    
        golden_result_3d = quant_to_fp_any_vectorize_torch(golden_result_3d, expo_width, mant_width, custom_bias=custom_bias_R, clip_OF=golden_clip_OF)
        # print("\n quanted_golden_result_3d=\n", golden_result_3d.numpy())

    golden_result_sign_3d = torch.where(golden_result_3d < 0, torch.tensor(-1, dtype=torch.float32), torch.tensor(1, dtype=torch.float32))
    


    # # MARK: Just Assume the Expo is correct, no matter it's S-S, S-N or N-N (Actually this will need to be improved)
    golden_expo_3d, _ = float_to_fpany_absint_torch(R_param_dict, golden_result_3d, clip_OF=False, return_extract=True)

    norm_mask_3d = ((A_expo.unsqueeze(2) > 0) & (B_expo.unsqueeze(0) > 0) & (golden_expo_3d > 0))
    # print(norm_mask_3d)

    # -- Release --
    del golden_expo_3d
    torch.cuda.empty_cache()


    # * Approx
    B_combine_neg = -(custom_bias_A + custom_bias_B - custom_bias_R)
    approx_expo = mult_result_expo(A_expo.unsqueeze(2), B_expo.unsqueeze(0), B_combine_neg)

    # -- Release --
    del A_expo, B_expo
    torch.cuda.empty_cache()


    mant_scale = R_param_dict["mant_scale"]

    with_approx = True
    # with_approx = False

    approx_result_3d = torch.where(
        norm_mask_3d, 
        # * Only for Normal
        (    
            # (2.0**(golden_expo_3d - torch.tensor(custom_bias_R, dtype=torch.int32)))          # 很神奇，这里用这个反而是不行的？
            (2.0**(approx_expo - torch.tensor(custom_bias_R, dtype=torch.int32)))
            * mult_result_mant(mant_width, A_mant.unsqueeze(2), B_mant.unsqueeze(0), error_table_NN, with_approx)
            * golden_result_sign_3d
        ),
        # 2.0**(1-custom_bias_R) * (result_mant_3d/mant_scale) * golden_result_sign_3d
        golden_result_3d
    )
    # print("\n approx_result_3d=\n", approx_result_3d.numpy())
    

    # * Self Checking mode
    if self_check_mode:
        golden_result_2d = golden_result_3d.sum(dim=1)
        norm_num = norm_mask_3d.sum().item()
        total_num = norm_mask_3d.numel()
        norm_percentage = (norm_num / total_num) * 100


    # -- Release --
    del A_mant, B_mant, approx_expo, golden_result_sign_3d, golden_result_3d, norm_mask_3d
    torch.cuda.empty_cache()


    # * Approx Sum up
    if double_quant:
        approx_result_3d = quant_to_fp_any_vectorize_torch(approx_result_3d, expo_width, mant_width, custom_bias=custom_bias_R, clip_OF=golden_clip_OF)
    # print("\n quanted_approx_result_3d=\n", approx_result_3d.numpy())
    approx_result_2d = approx_result_3d.sum(dim=1)

    del approx_result_3d
    torch.cuda.empty_cache()


    # * Self Checking mode
    if self_check_mode:
        error = abs(golden_result_2d - approx_result_2d)
        print("\n====== Self-Checking Mode ======")
        print(f"Normal Values in result_3d: {norm_num}/{total_num} = {norm_percentage:.1f}%")
        print(f"Max  Error : {torch.max(error)}")
        print(f"Mean Error : {torch.mean(error)}")
        print(f"RMSE       : {torch.sqrt(torch.mean(error ** 2))}")

        # print("\n golden_result_2d=\n", golden_result_2d.numpy())
        # print("\n approx_result_2d=\n", approx_result_2d.numpy())

        del golden_result_2d, error
        torch.cuda.empty_cache()


    return approx_result_2d



def mult_result_expo(x_expo, y_expo, B_combine_neg):

    return x_expo + y_expo + B_combine_neg


def mult_result_mant(mant_width, x_mant, y_mant, error_table_NN, with_approx):
    
    # MARK: Only For Normal values
    if with_approx:
        return (1 + x_mant*(2**(-mant_width))) * (1 + y_mant*(2**(-mant_width))) - ((2**(-mant_width)) * error_table_NN[x_mant.long(), y_mant.long()])
    else:
        return (1 + x_mant*(2**(-mant_width))) * (1 + y_mant*(2**(-mant_width)))




def param_prepare(expo_width, mant_width, custom_bias=None, debug_mode=False):

    # * Bias can be custom
    if custom_bias is not None:
        fp_bias = custom_bias
    else:
        fp_bias = int((2**(expo_width - 1)) - 1)
    
    # * Parameters preparing
    bias_double  = int(2 * fp_bias)
    max_expo     = int(2**expo_width - 1)
    max_mant     = int(2**mant_width - 1)
    max_norm     = (2**(max_expo - fp_bias)) * (2 - 2**(-mant_width))
    min_norm     = 2**(1 - fp_bias)
    min_subnorm  = (2**(1 - fp_bias)) * 2**(-mant_width)
    mant_scale   = int(2**mant_width)
    max_norm_int = int(2**(expo_width+mant_width) - 1)
    OF_UF_mod    = int(2**(expo_width+mant_width))

    param_dict = {
        "expo_width"   : expo_width   ,
        "mant_width"   : mant_width   ,
        "fp_bias"      : fp_bias      , 
        "bias_double"  : bias_double  , 
        "max_norm"     : max_norm     , 
        "min_norm"     : min_norm     , 
        "min_subnorm"  : min_subnorm  , 
        "max_expo"     : max_expo     , 
        "max_mant"     : max_mant     , 
        "mant_scale"   : mant_scale   , 
        "max_norm_int" : max_norm_int , 
        "OF_UF_mod"    : OF_UF_mod    , 
    }

    if debug_mode:
        print(f"\n======== Parameters preparation for FP{1+expo_width+mant_width}_E{expo_width}M{mant_width} ========")
        for key, value in param_dict.items():
            print(f"{type(value)} : {key} = {value}")
        print("=====================================================\n")

    return param_dict



def float_to_fpany_absint_torch(param_dict, values, clip_OF=False, return_extract=True):

    """
    Vectorize Version of Generic Conversion: FP64 -> Custom Floating Point Binary.
    It will return each parts in int form.

    Args:
        values (torch.Tensor) : Floating-Point values (FP64 / FP32 / FP16) of the fp 
        param_dict     (dict) : parameters provided
        clip_OF        (bool) : Whether to clip the overflow value to max_norm or not. (default True)
        return_extract (bool) : Whether to return the expo & mant in separate or added way. 
    """


    # * Parameters preparing
    mant_width = param_dict["mant_width"]
    fp_bias    = param_dict["fp_bias"]
    max_norm   = param_dict["max_norm"]
    min_norm   = param_dict["min_norm"]
    max_expo   = param_dict["max_expo"]
    max_mant   = param_dict["max_mant"]
    mant_scale = param_dict["mant_scale"]

    # * Preprocess
    values = torch.as_tensor(values, dtype=torch.float32)    # Ensure it's a torch tensor with float32 dtype

    # * Extracting
    # torch.frexp(): Decomposes input into mantissa and exponent tensors, such that input = mantissa ∈ (-1,1) x 2^exponent
    mant, expo = torch.frexp(values)

    # * Consider SubNormal
    subnorm_mask = (torch.abs(values) < min_norm)    # Indicate which values are Sub-Normal

    # print("subnorm_mask =", subnorm_mask)
    # print("mant_scale =", mant_scale)

    subnorm_leftshift_extra = fp_bias - 1 + mant_width    # Pre calculate this for faster speed

    # * Adjust Mant
    mant = torch.clamp(torch.round(
        torch.where(subnorm_mask, 
        torch.ldexp(torch.abs(mant), expo + subnorm_leftshift_extra),                      # torch.abs(mant) << (expo + subnorm_leftshift_extra)
        torch.ldexp((torch.abs(mant)*2-1), torch.tensor(mant_width, dtype=torch.int32))    # (torch.abs(mant)*2-1) << mant_width
    )), max=max_mant).to(torch.int32)


    # * Adjust Expo 
    expo = torch.where(subnorm_mask, torch.tensor(0, dtype=torch.int32), expo + torch.tensor(int(fp_bias - 1), dtype=torch.int32))

    # * Overflow
    if clip_OF:
        overflow_mask = (values < -max_norm) | (values > max_norm)
        expo = torch.where(overflow_mask, torch.tensor(max_expo, dtype=torch.int32), expo)
        mant = torch.where(overflow_mask, torch.tensor(max_mant, dtype=torch.int32), mant)

    if return_extract:
        return expo, mant
    else:
        return expo * torch.tensor(mant_scale, dtype=torch.int32) + mant



def fpany_absint_to_float_torch(param_dict, sign=None, abs_int=None, expo=None, mant=None):
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

    # * Parameters preparing
    fp_bias    = param_dict["fp_bias"]
    mant_scale = param_dict["mant_scale"]


    if abs_int is not None:
        abs_int = torch.as_tensor(abs_int)    # ensure it's a torch tensor
        expo = torch.div(abs_int, mant_scale, rounding_mode='floor')    # expo = abs_int // mant_scale
        mant = abs_int % mant_scale
    else:
        expo = torch.as_tensor(expo)          # ensure it's a torch tensor
        mant = torch.as_tensor(mant)          # ensure it's a torch tensor

    subnorm_mask = (expo == 0)

    values = torch.where(subnorm_mask, 2.0**(1-fp_bias) * (mant/mant_scale), 2.0**(expo-fp_bias) * (1 + (mant/mant_scale)))

    if sign is not None:
        sign = torch.as_tensor(sign)  # ensure it's a torch tensor
        values = values * sign

    return values



def quant_to_fp_any_vectorize_torch(arr, expo_width, mant_width, custom_bias=None, clip_OF=True):
    """
    Quantize a PyTorch tensor to floating point representation with specified exponent and mantissa widths.

    Parameters:
    arr (torch.Tensor) : Input tensor to be quantized
    expo_width  (int)  : Width of the exponent in bits
    mant_width  (int)  : Width of the mantissa in bits
    custom_bias (int)  : Custom bias can be provided by user
    clip_OF    (bool)  : Whether to clip the overflow value to max_norm or not. (default True)
                         If not, then the expo will actually extend to hold the overflow value.

    Returns:
    torch.Tensor: Quantized tensor with the same shape as input
    """

    arr = torch.as_tensor(arr, dtype=torch.float32)

    # * Parameters preparing
    param_dict = param_prepare(expo_width=expo_width, mant_width=mant_width, custom_bias=custom_bias, debug_mode=False)

    # * view as fp -> view as int
    expo, mant = float_to_fpany_absint_torch(param_dict=param_dict, values=arr, clip_OF=clip_OF, return_extract=True)

    sign = torch.where(arr < 0, torch.tensor(-1, dtype=torch.float32), torch.tensor(1, dtype=torch.float32))

    # * view as int -> view as fp
    fp_values = fpany_absint_to_float_torch(param_dict=param_dict, sign=sign, abs_int=None, expo=expo, mant=mant)

    return fp_values



def show_value_space(expo_width, mant_width, custom_bias, show_style=1):

    pair_space = []
    for expo in range(2**expo_width):
        for mant in range(2**mant_width):
            pair_space.append([expo, mant])

    value_space = torch.tensor([ i for i in range(0, 2**(expo_width+mant_width))])
    param_dict = param_prepare(expo_width, mant_width, custom_bias=custom_bias, debug_mode=False)
    value_space_fp = fpany_absint_to_float_torch(param_dict=param_dict, sign=None, abs_int=value_space, expo=None, mant=None)


    if show_style == 0:
        pass
    elif show_style == 1:
        print(f"The value space of E{expo_width}M{mant_width}, bias={custom_bias} is: (in S-N mode) \n", value_space_fp.numpy())
    elif show_style == 2:
        print(f"The value space of E{expo_width}M{mant_width}, bias={custom_bias} is: (in S-N mode) \n")
        for i in range(len(value_space)):
            print(f"expo={pair_space[i][0]}, mant={pair_space[i][1]}, value={value_space_fp[i]}")

    return value_space_fp


# def clamp_to_norm(values, param_dict):
#     min_norm = param_dict["min_norm"]
#     values[values.abs() < min_norm] = values[values.abs() < 1].sign() * min_norm
#     return values




comp_table_NN_list = [

    torch.tensor([
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 1, 1, 0, 0],
        [0, 0, 0, 1, 1, 1, 1, 0],
        [0, 0, 1, 1, 1, 1, 1, 0],
        [0, 1, 1, 1, 1, 1, 0, 0],
        [0, 1, 1, 1, 1, 1, 0, 0],
        [0, 0, 1, 1, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0]
    ], dtype=torch.int8, device='cuda'),

    torch.tensor([
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0],
        [0, 0, 0, 0,-1, 0, 0, 0, 0, 0,-1,-1, 0, 0, 1, 0],
        [0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0],
        [0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0],
        [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,-1, 0, 0, 1, 0],
        [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
        [0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1,-1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
        [0, 1,-1, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0,-1, 0, 0],
        [0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0],
        [0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    ], dtype=torch.int8, device='cuda'),

    torch.tensor([
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
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    ], dtype=torch.int8, device='cuda'),

    torch.tensor([
        [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],
        [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1],
        [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0,-1],
        [ 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0,-1],
        [ 0, 0, 0, 0,-1, 0, 0, 0,-1,-1,-1,-1, 0, 0, 0, 0,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0,-1, 1, 0, 0,-1],
        [ 0, 0, 0, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 1, 0, 0,-1],
        [ 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0,-1,-1, 1, 0, 0,-1],
        [ 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0,-1,-1, 1, 0, 0,-1],
        [ 0, 0, 0, 1,-1,-1, 0, 0,-1,-1,-1, 0,-1,-1, 0, 0,-1,-1,-1,-1, 0, 0, 0,-1, 1, 1, 0, 0, 1, 0, 0,-1],
        [ 0, 0, 1, 1,-1,-1, 0, 0,-1, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 1, 1, 0, 0, 0, 0, 0,-1],
        [ 0, 0, 1, 1,-1, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,-1, 0, 0,-1,-1, 1, 0, 0, 0, 0, 0, 0,-1],
        [ 0, 0, 1, 1,-1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0,-1, 0, 0,-1,-1, 1, 0, 0, 0, 0, 0, 0,-1],
        [ 0, 0, 1, 1, 0, 0, 0, 1,-1,-1, 0, 0,-1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1],
        [ 0, 0, 1, 1, 0, 0, 0, 1,-1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0,-1],
        [ 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0,-1],
        [ 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 0,-1],
        [-1, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0,-1, 1, 1, 0, 0],
        [-1, 0, 0, 1,-1, 0, 0, 1,-1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0,-1,-1, 1, 1, 0, 0],
        [-1, 0, 0, 1,-1, 0, 0, 1,-1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0,-1,-1, 1, 1, 0, 0],
        [-1, 0, 0, 1,-1, 0, 1, 1,-1, 0,-1,-1, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1, 1, 1, 0, 0],
        [-1, 0, 0, 1,-1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0],
        [-1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0],
        [-1, 0, 0, 1, 0, 0, 1, 1, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 1, 0, 0, 0],
        [-1, 0, 0, 1, 0, 0, 1, 1,-1,-1,-1,-1, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 1, 0, 0, 0],
        [-1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [-1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [-1, 0, 1, 1, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0],
        [-1, 0, 1, 1,-1,-1,-1,-1, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0],
        [-1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
        [-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    ], dtype=torch.int8, device='cuda'),

    torch.tensor([
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0],
        [0, 0, 0, 0,-1, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 1, 0, 0,-1, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0,-1, 1, 0],
        [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 1, 0],
        [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 1, 0],
        [0, 0,-1, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 1, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 1, 0],
        [0, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 0, 0, 0,-1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0],
        [0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 0, 0, 0,-1, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 1, 0],
        [0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
        [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1,-1, 0, 0, 1,-1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1,-1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0],
        [0, 1,-1, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
        [0, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0],
        [0, 1,-1, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
        [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
        [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1, 0, 0,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    ], dtype=torch.int8, device='cuda'),

    torch.tensor([
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
    ], dtype=torch.int8, device='cuda'),
    
] 




# MARK: This is just a simplified method.
def get_error_table_NN(expo_width, mant_width, withComp, dnsmp_factor):
    
    if ((expo_width, mant_width) == (4, 3)):
        
        if withComp:
            error_table_NN = torch.zeros((2**mant_width, 2**mant_width), dtype=torch.int32, device='cuda')
        else:
            error_table_NN = comp_table_NN_list[0]

    elif ((expo_width, mant_width) == (3, 4)):

        if withComp:
            if dnsmp_factor == 3:
                error_table_NN = comp_table_NN_list[1]
            elif dnsmp_factor >= 4:
                error_table_NN = torch.zeros((2**mant_width, 2**mant_width), dtype=torch.int32, device='cuda')
        else:
            error_table_NN = comp_table_NN_list[2]
    
    elif ((expo_width, mant_width) == (2, 5)):
        if withComp:
            if dnsmp_factor == 3:
                error_table_NN = comp_table_NN_list[3]
            
            elif dnsmp_factor == 4:
                error_table_NN = comp_table_NN_list[4]

            elif dnsmp_factor == 5:
                error_table_NN = torch.zeros((2**mant_width, 2**mant_width), dtype=torch.int32, device='cuda')

        else:
            error_table_NN = comp_table_NN_list[5]

    else:
        error_table_NN = torch.zeros((2**mant_width, 2**mant_width), dtype=torch.int32, device='cuda')
        raise ValueError("Invalid combination of expo_width and mant_width")

    return error_table_NN












# ===================================================== Copy to here ===================================================== #


if __name__ == "__main__":

    def get_error_table_NN_cpu(expo_width, mant_width, withComp, dnsmp_factor):
        
        if ((expo_width, mant_width) == (4, 3)):
            
            if withComp:
                error_table_NN = torch.zeros((2**mant_width, 2**mant_width), dtype=torch.int32)
            else:
                error_table_NN = torch.tensor([
                    [0, 0, 0, 0, 0, 0, 0, 0],
                    [0, 0, 0, 0, 1, 1, 0, 0],
                    [0, 0, 0, 1, 1, 1, 1, 0],
                    [0, 0, 1, 1, 1, 1, 1, 0],
                    [0, 1, 1, 1, 1, 1, 0, 0],
                    [0, 1, 1, 1, 1, 1, 0, 0],
                    [0, 0, 1, 1, 0, 0, 0, 0],
                    [0, 0, 0, 0, 0, 0, 0, 0]
                ], dtype=torch.int32)


        elif ((expo_width, mant_width) == (3, 4)):

            if withComp:
                if dnsmp_factor == 3:
                    error_table_NN = torch.tensor([
                        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0],
                        [0, 0, 0, 0,-1, 0, 0, 0, 0, 0,-1,-1, 0, 0, 1, 0],
                        [0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0],
                        [0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0],
                        [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
                        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
                        [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,-1, 0, 0, 1, 0],
                        [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
                        [0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 1,-1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
                        [0, 1,-1, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0,-1, 0, 0],
                        [0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0],
                        [0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                    ], dtype=torch.int32)
                elif dnsmp_factor >= 4:
                    error_table_NN = torch.zeros((2**mant_width, 2**mant_width), dtype=torch.int32)
            else:
                error_table_NN = torch.tensor([
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
                    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                ], dtype=torch.int32)
        

        elif ((expo_width, mant_width) == (2, 5)):
            if withComp:
                if dnsmp_factor == 3:
                    error_table_NN = torch.tensor([
                        [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],
                        [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1],
                        [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0,-1],
                        [ 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0,-1],
                        [ 0, 0, 0, 0,-1, 0, 0, 0,-1,-1,-1,-1, 0, 0, 0, 0,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0,-1, 1, 0, 0,-1],
                        [ 0, 0, 0, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 1, 0, 0,-1],
                        [ 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0,-1,-1, 1, 0, 0,-1],
                        [ 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0,-1,-1, 1, 0, 0,-1],
                        [ 0, 0, 0, 1,-1,-1, 0, 0,-1,-1,-1, 0,-1,-1, 0, 0,-1,-1,-1,-1, 0, 0, 0,-1, 1, 1, 0, 0, 1, 0, 0,-1],
                        [ 0, 0, 1, 1,-1,-1, 0, 0,-1, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 1, 1, 0, 0, 0, 0, 0,-1],
                        [ 0, 0, 1, 1,-1, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,-1, 0, 0,-1,-1, 1, 0, 0, 0, 0, 0, 0,-1],
                        [ 0, 0, 1, 1,-1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0,-1, 0, 0,-1,-1, 1, 0, 0, 0, 0, 0, 0,-1],
                        [ 0, 0, 1, 1, 0, 0, 0, 1,-1,-1, 0, 0,-1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1],
                        [ 0, 0, 1, 1, 0, 0, 0, 1,-1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0,-1],
                        [ 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0,-1],
                        [ 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 0,-1],
                        [-1, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0,-1, 1, 1, 0, 0],
                        [-1, 0, 0, 1,-1, 0, 0, 1,-1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0,-1,-1, 1, 1, 0, 0],
                        [-1, 0, 0, 1,-1, 0, 0, 1,-1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0,-1,-1, 1, 1, 0, 0],
                        [-1, 0, 0, 1,-1, 0, 1, 1,-1, 0,-1,-1, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1, 1, 1, 0, 0],
                        [-1, 0, 0, 1,-1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0],
                        [-1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0],
                        [-1, 0, 0, 1, 0, 0, 1, 1, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 1, 0, 0, 0],
                        [-1, 0, 0, 1, 0, 0, 1, 1,-1,-1,-1,-1, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 1, 0, 0, 0],
                        [-1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [-1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [-1, 0, 1, 1, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0],
                        [-1, 0, 1, 1,-1,-1,-1,-1, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0],
                        [-1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
                        [-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                    ], dtype=torch.int32)
                
                elif dnsmp_factor == 4:
                    error_table_NN = torch.tensor([
                        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0],
                        [0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 1, 0],
                        [0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0],
                        [0, 0, 0, 0,-1, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 1, 0, 0,-1, 1, 0],
                        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0,-1, 1, 0],
                        [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 1, 0],
                        [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 1, 0],
                        [0, 0,-1, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 1, 0, 1, 0],
                        [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 1, 0],
                        [0, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 0, 0, 0,-1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0],
                        [0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0],
                        [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
                        [0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 0, 0, 0,-1, 0, 0, 1, 0],
                        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 1, 0],
                        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 1, 0],
                        [0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
                        [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 1,-1, 0, 0, 1,-1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 1,-1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0],
                        [0, 1,-1, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
                        [0, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0],
                        [0, 1,-1, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
                        [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
                        [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 1, 0, 0,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                    ], dtype=torch.int32)

                elif dnsmp_factor == 5:
                    error_table_NN = torch.zeros((2**mant_width, 2**mant_width), dtype=torch.int32)

            else:
                error_table_NN = torch.tensor([
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
                ], dtype=torch.int32)

        else:
            error_table_NN = torch.zeros((2**mant_width, 2**mant_width), dtype=torch.int32)
            raise ValueError("Invalid combination of expo_width and mant_width")

        return error_table_NN



    def random_tensor_gen(expo_width, mant_width, custom_bias, row, col, seed=None):

        if seed is not None:
            np.random.seed(seed)
            random.seed(seed)

        value_space = show_value_space(expo_width, mant_width, custom_bias, show_style=0)

        random_tensor = torch.tensor([[np.random.choice(value_space.numpy()) * random.choice([-1, 1]) for _ in range(col)] for _ in range(row)])

        return random_tensor




if __name__ == "__main__":

    expo_width = 3
    mant_width = 4


    withComp = True
    # withComp = False

    sim_hw_add_OFUF = False
    with_OF_opt     = False
    with_UF_opt     = False
    golden_clip_OF  = False


    # dnsmp_factor = 3
    # dnsmp_factor = 4
    dnsmp_factor = 5


    Iact_bias = 3
    Wght_bias = 5
    Oact_bias = 5


    # * Given Tensor
    # Error is not obvious with the Given Tensor
    Iact = torch.tensor([
        [0.5625 , 0.3438, 0.1328125 , 0.0859375],
        [0.0625 , 0.3750, 0.4375    , 0.5      ],
        [0.96875, 1     , 1.0625    , 1.125    ],
    ])
    Wght = torch.tensor([
        [-0.109375 ], 
        [-0.2421875], 
        [0.05859375],
        [0.2109375 ] 
    ])
    Iact_quanted = quant_to_fp_any_vectorize_torch(Iact, expo_width, mant_width, custom_bias=Iact_bias, clip_OF=True)
    Wght_quanted = quant_to_fp_any_vectorize_torch(Wght, expo_width, mant_width, custom_bias=Wght_bias, clip_OF=True)
    # print("\n Iact_quanted=\n", Iact_quanted.numpy())
    # print("\n Wght_quanted=\n", Wght_quanted.numpy())


    # * Random Tensor
    seed = 5
    Iact_random = random_tensor_gen(expo_width, mant_width, custom_bias=Iact_bias, row=128, col=9, seed=seed)
    Wght_random = random_tensor_gen(expo_width, mant_width, custom_bias=Wght_bias, row=9, col=1, seed=seed)
    # print("\n Iact_random=\n", Iact_random.numpy())
    # print("\n Wght_random=\n", Wght_random.numpy())



    # np.set_printoptions(suppress=True)
    # show_value_space(expo_width, mant_width, custom_bias=Wght_bias, show_style=1)
    # show_value_space(expo_width, mant_width, custom_bias=Wght_bias, show_style=2)


    # error_table_NN = get_error_table_NN_cpu(expo_width, mant_width, withComp, dnsmp_factor)
    error_table_NN = get_error_table_NN(expo_width, mant_width, withComp, dnsmp_factor)
    # print(error_table_NN)
    # print(error_table_NN.abs().sum())

    custom_matmul_vectorize(
        # A               = Iact_quanted, 
        # B               = Wght_quanted, 
        A               = Iact_random, 
        B               = Wght_random, 
        expo_width      = expo_width, 
        mant_width      = mant_width,
        custom_bias_A   = Iact_bias,
        custom_bias_B   = Wght_bias,
        custom_bias_R   = Oact_bias,
        error_table_NN  = error_table_NN,
        sim_hw_add_OFUF = sim_hw_add_OFUF, 
        with_OF_opt     = with_OF_opt, 
        with_UF_opt     = with_UF_opt, 
        golden_clip_OF  = golden_clip_OF,
        double_quant    = True,
        debug_mode      = False, 
        self_check_mode = True
    )
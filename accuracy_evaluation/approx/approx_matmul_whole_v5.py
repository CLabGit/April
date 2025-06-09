import torch
import timeit
import numpy as np



# ===================================================== Copy from here ===================================================== #


def custom_matmul_vectorize(A, B, expo_width, mant_width,
                            custom_bias,
                            comp_table_NN,
                            sim_hw_add_OFUF=False, with_OF_opt=False, with_UF_opt=False, golden_clip_OF=False,
                            debug_mode=False, self_check_mode=False
                            ):
                            
    assert A.shape[1] == B.shape[0]

    # * Parameters preparing
    param_dict = param_prepare(expo_width, mant_width, custom_bias=custom_bias, debug_mode=debug_mode)


    clip_OF = True     # ! CHECKME
    # clip_OF = False    # ! CHECKME

    # * View FP as Int
    A_expo, A_mant = float_to_fpany_absint_vectorize_torch_new(
        values     = A, 
        mant_width = mant_width, 
        fp_bias    = param_dict["fp_bias"], 
        max_norm   = param_dict["max_norm"], 
        min_norm   = param_dict["min_norm"], 
        max_expo   = param_dict["max_expo"], 
        max_mant   = param_dict["max_mant"], 
        mant_scale = param_dict["mant_scale"], 
        clip_OF = clip_OF, return_extract = True
    )

    B_expo, B_mant = float_to_fpany_absint_vectorize_torch_new(
        values     = B, 
        mant_width = mant_width, 
        fp_bias    = param_dict["fp_bias"], 
        max_norm   = param_dict["max_norm"], 
        min_norm   = param_dict["min_norm"], 
        max_expo   = param_dict["max_expo"], 
        max_mant   = param_dict["max_mant"], 
        mant_scale = param_dict["mant_scale"], 
        clip_OF = clip_OF, return_extract = True
    )



    # B_neg = -((2 ** (expo_width-1) - 1) << mant_width)    # old
    B_neg = -(param_dict["fp_bias"] << mant_width)          # MARK: I should use this

    mant_scale = param_dict["mant_scale"]

    # 2d-Tensor * scalar + 2d-Tensor
    A_expo_mant_int = A_expo * mant_scale + A_mant
    B_expo_mant_int = B_expo * mant_scale + B_mant

    if debug_mode:
        print("==============================")
        print("B_neg =", B_neg)
        print("A_expo =\n", A_expo)
        print("A_mant =\n", A_mant)
        print("B_expo =\n", B_expo)
        print("B_mant =\n", B_mant)
        print("A_expo_mant_int =\n", A_expo_mant_int)
        print("B_expo_mant_int =\n", B_expo_mant_int)
        print("A_expo_mant_int is:", A_expo_mant_int.dtype)
        print("B_expo_mant_int is:", B_expo_mant_int.dtype)
        print("==============================")

    # -- Release --
    del A_expo, B_expo
    torch.cuda.empty_cache()


    temp_result_int_3d = approx_mult_new(
        A_expo_mant_int.unsqueeze(2), B_expo_mant_int.unsqueeze(0), B_neg,    # Approximation parts
        A_mant.unsqueeze(2), B_mant.unsqueeze(0), comp_table_NN,              # Compensation parts
        sim_hw_add_OFUF, with_OF_opt, with_UF_opt,                            # Overflow & Underflow parts
        OF_UF_mod    = param_dict["OF_UF_mod"], 
        max_norm_int = param_dict["max_norm_int"], 
        mant_scale   = param_dict["mant_scale"]
    )

    # print("temp_result_int_3d =\n", temp_result_int_3d)

    if self_check_mode:
        subnorm_num = torch.sum(temp_result_int_3d < mant_scale).item()
        total_num = temp_result_int_3d.numel()
        subnorm_percent = (subnorm_num / total_num) * 100


    # -- Release --
    del A_expo_mant_int, B_expo_mant_int, A_mant, B_mant
    torch.cuda.empty_cache()


    # * Sign of the result
    A_sign = torch.where(A < 0, torch.tensor(-1, dtype=torch.float32), torch.tensor(1, dtype=torch.float32))
    B_sign = torch.where(B < 0, torch.tensor(-1, dtype=torch.float32), torch.tensor(1, dtype=torch.float32))
    result_sign_3d = A_sign.unsqueeze(2) * B_sign.unsqueeze(0)

    # -- Release --
    del A_sign, B_sign
    torch.cuda.empty_cache()


    # * View FP as Int -> View FP as FP
    approx_result_fp_3d = fpany_absint_to_float_vectorize_torch(
        sign=result_sign_3d, abs_int=temp_result_int_3d, expo=None, mant=None, 
        fp_bias=param_dict["fp_bias"], mant_scale=param_dict["mant_scale"]
    )

    
    if debug_mode:
        print("approx_result_fp_3d =", approx_result_fp_3d)


    # -- Release --
    del result_sign_3d, temp_result_int_3d
    torch.cuda.empty_cache()


    # * 3d -> 2d
    approx_result_2d = approx_result_fp_3d.sum(dim=1)

    # -- Release --
    del approx_result_fp_3d
    torch.cuda.empty_cache()


    # * Self Checking mode
    if self_check_mode:
        golden_result_withquant_2d = golden_result_withquant(A, B, expo_width, mant_width, custom_bias=custom_bias, golden_clip_OF=golden_clip_OF)
        error = abs(golden_result_withquant_2d - approx_result_2d)
        print("\n====== Self-Checking Mode ======")
        print(f"Sub-Normal Values in approx_result_fp_3d: {subnorm_num}/{total_num} = {subnorm_percent:.1f}%")
        print(f"Max  Error : {torch.max(error)}")
        print(f"Mean Error : {torch.mean(error)}")
        print(f"RMSE       : {torch.sqrt(torch.mean(error ** 2))}")

        # -- Release --
        del golden_result_withquant_2d, error
        torch.cuda.empty_cache()


    return approx_result_2d



def approx_mult_new(x_int, y_int, B_neg, x_mant, y_mant, comp_table_NN, 
    sim_hw_add_OFUF, with_OF_opt, with_UF_opt,
    OF_UF_mod, max_norm_int, mant_scale):

    # * Approximate calculation + Compensation
    temp_result_int = x_int + y_int + B_neg + comp_table_NN[x_mant.long(), y_mant.long()]

    # * Simulate the Overflow & Underflow situation --- when using hardware adder to approximate multiplication
    if sim_hw_add_OFUF:
        
        # * Overflow & Underflow Masking
        overflow_mask = (temp_result_int > max_norm_int)
        underflow_mask = (temp_result_int < 0)
        
        # Use Modulo Operation to simulate Overflow & Underflow of hardware adder
        temp_result_int = temp_result_int % OF_UF_mod

        if with_OF_opt:
            temp_result_int = torch.where(overflow_mask, torch.tensor(max_norm_int, dtype=torch.int32, device=A.device), temp_result_int)

        if with_UF_opt:
            temp_result_int = torch.where(underflow_mask, temp_result_int % mant_scale, temp_result_int)

        # -- Release --
        del overflow_mask, underflow_mask
        torch.cuda.empty_cache()

    return temp_result_int



def golden_result_withquant(A, B, expo_width, mant_width, custom_bias, golden_clip_OF):

    # * Golden Result (Quantization before sum)
    golden_result_withquant_3d = A.unsqueeze(2) * B.unsqueeze(0)
    golden_result_withquant_3d = quant_to_fp_any_vectorize_torch(golden_result_withquant_3d, expo_width, mant_width, custom_bias, clip_OF=golden_clip_OF)    # ? Quantization before sum
    golden_result_withquant_2d = golden_result_withquant_3d.sum(dim=1)

    # print("golden_result_withquant_3d =\n", golden_result_withquant_3d)
    # print("golden_result_withquant_2d =\n", golden_result_withquant_2d)

    # -- Release --
    del golden_result_withquant_3d
    torch.cuda.empty_cache()

    return golden_result_withquant_2d



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



def float_to_fpany_absint_vectorize_torch_new(values, mant_width, fp_bias, max_norm, min_norm, max_expo, max_mant, mant_scale, 
                                              clip_OF=False, return_extract=True):

    values = torch.as_tensor(values, dtype=torch.float32)    # Ensure it's a torch tensor with float32 dtype

    # torch.frexp(): Decomposes input into mantissa and exponent tensors, such that input = mantissa ∈ (-1,1) x 2^exponent
    mant, expo = torch.frexp(values)

    subnorm_mask = (torch.abs(values) < min_norm)    # Indicate which values are Sub-Normal

    # print("subnorm_mask =", subnorm_mask)
    # print("mant_scale =", mant_scale)

    subnorm_leftshift_extra = fp_bias - 1 + mant_width    # Pre calculate this for faster speed

    # * Adjust Mant
    mant = torch.clamp(torch.round(
        torch.where(subnorm_mask, 
        torch.ldexp(torch.abs(mant), expo + subnorm_leftshift_extra),                          # = torch.abs(mant) << (expo + subnorm_leftshift_extra)
        torch.ldexp((torch.abs(mant)*2-1), torch.tensor(mant_width, dtype=torch.int32))        # = (torch.abs(mant)*2-1) << mant_width
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
    expo, mant = float_to_fpany_absint_vectorize_torch_new(
        values=arr, mant_width=mant_width, fp_bias=param_dict["fp_bias"], 
        max_norm=param_dict["max_norm"], min_norm=param_dict["min_norm"], 
        max_expo=param_dict["max_expo"], max_mant=param_dict["max_mant"], 
        mant_scale=param_dict["mant_scale"], 
        clip_OF=clip_OF, return_extract=True
    )

    sign = torch.where(arr < 0, torch.tensor(-1, dtype=torch.float32), torch.tensor(1, dtype=torch.float32))

    # * view as int -> view as fp
    fp_values = fpany_absint_to_float_vectorize_torch(
        sign=sign, abs_int=None, expo=expo, mant=mant, 
        fp_bias=param_dict["fp_bias"], mant_scale=param_dict["mant_scale"]
    )

    return fp_values





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
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            ], dtype=torch.int8, device='cuda'),
    torch.tensor([
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
            ], dtype=torch.int8, device='cuda'),
    torch.tensor([
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
def get_comp_table_NN(expo_width, mant_width, withComp, dnsmp_factor, device):


    if ((expo_width, mant_width) == (4, 3)) and withComp:
        comp_table_NN = comp_table_NN_list[0]

    elif ((expo_width, mant_width) == (3, 4)) and withComp:
        if dnsmp_factor == 3:
            comp_table_NN = comp_table_NN_list[1]   
        elif dnsmp_factor >= 4:
            comp_table_NN = comp_table_NN_list[2]

    elif ((expo_width, mant_width) == (2, 5)) and withComp:
        if dnsmp_factor == 3:
            comp_table_NN = comp_table_NN_list[3]
        
        elif dnsmp_factor == 4:
            comp_table_NN = comp_table_NN_list[4]

        elif dnsmp_factor >= 5:
            comp_table_NN = comp_table_NN_list[5]

    else:
        comp_table_NN = torch.zeros((2**mant_width, 2**mant_width), dtype=torch.int32)
        raise ValueError("Invalid combination of expo_width and mant_width")

    return comp_table_NN


# ===================================================== Copy to here ===================================================== #








if __name__ == "__main__":


    # MARK: Only for CPU test
    def get_comp_table_NN_cpu(expo_width, mant_width, withComp, dnsmp_factor):


        if ((expo_width, mant_width) == (4, 3)) and withComp:
            comp_table_NN_cpu = torch.tensor([
                [0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 1, 1, 0, 0],
                [0, 0, 0, 1, 1, 1, 1, 0],
                [0, 0, 1, 1, 1, 1, 1, 0],
                [0, 1, 1, 1, 1, 1, 0, 0],
                [0, 1, 1, 1, 1, 1, 0, 0],
                [0, 0, 1, 1, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0]
            ], dtype=torch.int32)

        elif ((expo_width, mant_width) == (3, 4)) and withComp:
            if dnsmp_factor == 3:
                comp_table_NN_cpu = torch.tensor([
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
                ], dtype=torch.int32)
            elif dnsmp_factor >= 4:
                comp_table_NN_cpu = torch.tensor([
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
                ], dtype=torch.int32)


        elif ((expo_width, mant_width) == (2, 5)) and withComp:
            if dnsmp_factor == 3:
                comp_table_NN_cpu = torch.tensor([
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
                ], dtype=torch.int32)
            
            elif dnsmp_factor == 4:
                comp_table_NN_cpu = torch.tensor([
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
                ], dtype=torch.int32)

            elif dnsmp_factor >= 5:
                comp_table_NN_cpu = torch.tensor([
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
            comp_table_NN_cpu = torch.zeros((2**mant_width, 2**mant_width), dtype=torch.int32)

        return comp_table_NN_cpu




    def random_numpy_matrix(shape, abs_min, abs_max, expo_width, mant_width, custom_bias=None, given_seed=None):
        """
        Generate a random NumPy matrix with values in the range [abs_min, abs_max] and [-abs_max, -abs_min]
        
        Parameters:
        shape (tuple): The shape of the output matrix
        abs_min (float): The minimum absolute value
        abs_max (float): The maximum absolute value
        
        Returns:
        numpy.ndarray: A random matrix with the specified shape and value range
        """

        # Provide the seed you want
        if given_seed is not None:
            np.random.seed(given_seed)
        
        # Generate a random matrix in the range [-1, 1]
        random_matrix = np.random.rand(*shape) * 2 - 1
        
        # Create a mask for positive and negative values
        mask = random_matrix >= 0
        
        # Parameters preparing
        param_dict = param_prepare(expo_width=expo_width, mant_width=mant_width, custom_bias=custom_bias, debug_mode=False)

        if abs_max is None:
            abs_max = param_dict["max_norm"]
        if abs_min is None:
            abs_min = param_dict["min_norm"]

        # Scale the matrix to the desired range
        scaled_matrix = np.where(mask, 
                                random_matrix * (abs_max - abs_min) + abs_min,
                                random_matrix * (abs_max - abs_min) - abs_min
                                )


        # print("Before quant:\n", scaled_matrix)

        # Quantize the nums
        quantized_matrix = quant_to_fp_any_vectorize_torch(
            scaled_matrix, expo_width, mant_width, custom_bias=custom_bias, clip_OF=True
        )

        # print("After quant:\n", quantized_matrix.numpy())

        return quantized_matrix.numpy()




if __name__ == "__main__":

    expo_width = 3
    mant_width = 4

    # MARK:
    # custom_bias = 8    # ! Try to make this smaller
    custom_bias = None

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

    # debug_mode = True
    debug_mode = False

    self_check_mode = True
    # self_check_mode = False

    # * Parameters preparing
    param_dict = param_prepare(expo_width=expo_width, mant_width=mant_width, custom_bias=custom_bias, debug_mode=True)

    shape_A = (2, 2)
    shape_B = (2, 2)
    # shape_A = (4, 4)
    # shape_B = (4, 4)
    # shape_A = (6, 6)
    # shape_B = (6, 6)
    # shape_A = (8, 8)
    # shape_B = (8, 8)
    # shape_A = (32, 32)
    # shape_B = (32, 32)
    # shape_A = (128, 128)
    # shape_B = (128, 128)

    abs_max = param_dict["max_norm"]
    abs_min = param_dict["min_norm"]

    # MARK: For E3M4: No OF, No UF
    if (expo_width, mant_width) == (3, 4):
        abs_max = abs_max / 4
        abs_min = abs_min * 2

    # MARK: For E2M5: No OF, No UF
    if (expo_width, mant_width) == (2, 5):
        abs_max = abs_max / 2
        abs_min = abs_min * 2
    
    print("abs_max =", abs_max)
    print("abs_min =", abs_min)
    print("min_norm =", param_dict["min_norm"])

    # Generate quantized random matrix
    A = random_numpy_matrix(shape_A, abs_min, abs_max, expo_width, mant_width, custom_bias=custom_bias, given_seed=42)
    B = random_numpy_matrix(shape_B, abs_min, abs_max, expo_width, mant_width, custom_bias=custom_bias, given_seed=42)

    print("\nMatrix A =\n", A)
    print("\nMatrix B =\n", B)

    # * Golden Result (Quantization after sum)
    classical_result = np.matmul(A, B)
    print("classical_result =\n", classical_result)

    A = torch.as_tensor(A, dtype=torch.float32)
    B = torch.as_tensor(B, dtype=torch.float32)

    # * Golden Result (Quantization before sum)
    # golden_result_withquant_2d = golden_result_withquant(A, B, expo_width, mant_width, custom_bias=custom_bias, golden_clip_OF=golden_clip_OF)

    # * Approx Result (No Comp)
    custom_result_vectorize_nocomp = custom_matmul_vectorize(
        A, B, 
        expo_width, 
        mant_width, 
        custom_bias     = custom_bias,
        comp_table_NN   = np.zeros((2**mant_width, 2**mant_width), dtype=int), 
        sim_hw_add_OFUF = sim_hw_add_OFUF, 
        with_OF_opt     = with_OF_opt, 
        with_UF_opt     = with_UF_opt, 
        golden_clip_OF  = golden_clip_OF,
        debug_mode      = debug_mode,
        self_check_mode = self_check_mode
    )

    custom_result_vectorize_nocomp = custom_result_vectorize_nocomp.numpy()


    comp_table_NN = get_comp_table_NN_cpu(expo_width, mant_width, withComp=True, dnsmp_factor=dnsmp_factor)
    # print(comp_table_NN)


    # * Approx Result (with Comp)
    custom_result_vectorize_withcomp = custom_matmul_vectorize(
        A, B, 
        expo_width, 
        mant_width, 
        custom_bias     = custom_bias,
        comp_table_NN   = comp_table_NN, 
        sim_hw_add_OFUF = sim_hw_add_OFUF, 
        with_OF_opt     = with_OF_opt, 
        with_UF_opt     = with_UF_opt, 
        golden_clip_OF  = golden_clip_OF,
        debug_mode      = debug_mode,
        self_check_mode = self_check_mode
    )

    custom_result_vectorize_withcomp = custom_result_vectorize_withcomp.numpy()
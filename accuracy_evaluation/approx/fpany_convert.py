import torch
import numpy as np



def param_prepare(expo_width, mant_width, custom_bias=None, debug_mode=False):

    # * Bias can be custom
    if custom_bias is not None:
        fp_bias = custom_bias
    else:
        fp_bias = int((2**(expo_width - 1)) - 1)
    
    # * Parameters preparing
    # For Subnorm & Norm
    bias_double  = int(2 * fp_bias)
    max_expo     = int(2**expo_width - 1)
    max_mant     = int(2**mant_width - 1)
    max_norm     = (2**(max_expo - fp_bias)) * (2 - 2**(-mant_width))
    min_norm     = 2**(1 - fp_bias)
    min_subnorm  = (2**(1 - fp_bias)) * 2**(-mant_width)
    mant_scale   = int(2**mant_width)
    max_norm_int = int(2**(expo_width+mant_width) - 1)
    OF_UF_mod    = int(2**(expo_width+mant_width))
    
    # For All-Norm
    max_value_allnorm  = (2**(max_expo - fp_bias)) * (2 - 2**(-mant_width))
    min_value_allnorm  = (2**(0 - fp_bias)) * (1 + 2**(-mant_width))
    resolution_allnorm = (2**(0 - fp_bias)) * 2**(-mant_width)

    param_dict = {
        "expo_width"         : expo_width         ,
        "mant_width"         : mant_width         ,
        "fp_bias"            : fp_bias            , 
        "bias_double"        : bias_double        , 
        "max_norm"           : max_norm           , 
        "min_norm"           : min_norm           , 
        "min_subnorm"        : min_subnorm        , 
        "max_expo"           : max_expo           , 
        "max_mant"           : max_mant           , 
        "mant_scale"         : mant_scale         , 
        "max_norm_int"       : max_norm_int       , 
        "OF_UF_mod"          : OF_UF_mod          , 
        "max_value_allnorm"  : max_value_allnorm  , 
        "min_value_allnorm"  : min_value_allnorm  , 
        "resolution_allnorm" : resolution_allnorm , 
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


def float_to_fpany_absint_torch_allnorm(param_dict, values, clip_OF=False, return_extract=True):

    """
    MARK: All values will be considered as Normal values.
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
    max_value_allnorm  = param_dict["max_value_allnorm"]
    min_value_allnorm  = param_dict["min_value_allnorm"]
    resolution_allnorm = param_dict["resolution_allnorm"]
    max_expo   = param_dict["max_expo"]
    max_mant   = param_dict["max_mant"]
    mant_scale = param_dict["mant_scale"]

    # * Preprocess
    values = torch.as_tensor(values, dtype=torch.float32)    # Ensure it's a torch tensor with float32 dtype
    
    # * Open this if you want to consider values in [min_value/2, min_value) as min_value
    # But if the input values have already been quantized, than this one is useless
    values = torch.round(values / resolution_allnorm) * resolution_allnorm

    # * Extracting
    # torch.frexp(): Decomposes input into mantissa and exponent tensors, such that input = mantissa ∈ (-1,1) x 2^exponent
    mant, expo = torch.frexp(values)

    # * Consider 0
    zero_mask = (values > -min_value_allnorm) & (values < min_value_allnorm)

    # * Adjust Mant
    mant = torch.clamp(torch.round(
        torch.where(zero_mask, 
        torch.tensor(0, dtype=torch.float32), 
        torch.ldexp((torch.abs(mant)*2-1), torch.tensor(mant_width, dtype=torch.int32))        # = (torch.abs(mant)*2-1) << mant_width
    )), max=max_mant).to(torch.int32)

    # * Adjust Expo
    expo = torch.where(zero_mask, torch.tensor(0, dtype=torch.int32), expo + torch.tensor(int(fp_bias - 1), dtype=torch.int32))

    # * Overflow
    if clip_OF:
        overflow_mask = (values < -max_value_allnorm) | (values > max_value_allnorm)
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


def fpany_absint_to_float_torch_allnorm(param_dict, sign=None, abs_int=None, expo=None, mant=None):

    """
    MARK: All values will be considered as Normal values.
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

    # values = 2.0**(expo-fp_bias) * (1 + (mant/mant_scale))

    zero_mask = (expo == 0) & (mant == 0)

    # MARK: All values are in normal form.
    values = torch.where(zero_mask, torch.tensor(0, dtype=torch.float32), 2.0**(expo-fp_bias) * (1 + (mant/mant_scale)))

    if sign is not None:
        sign = torch.as_tensor(sign)  # ensure it's a torch tensor
        values = values * sign

    return values




def quant_to_fp_any_vectorize_torch(arr, expo_width, mant_width, custom_bias=None, clip_OF=True, debug_mode=False):
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

    if debug_mode:
        print("\n sign =\n", sign)
        print("\n expo =\n", expo)
        print("\n mant =\n", mant)

    # * view as int -> view as fp
    fp_values = fpany_absint_to_float_torch(param_dict=param_dict, sign=sign, abs_int=None, expo=expo, mant=mant)

    return fp_values


def quant_to_fp_any_vectorize_torch_allnorm(arr, expo_width, mant_width, custom_bias=None, clip_OF=True):
    """
    MARK: All values will be considered as Normal values.
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
    expo, mant = float_to_fpany_absint_torch_allnorm(param_dict=param_dict, values=arr, clip_OF=clip_OF, return_extract=True)

    sign = torch.where(arr < 0, torch.tensor(-1, dtype=torch.float32), torch.tensor(1, dtype=torch.float32))

    # * view as int -> view as fp
    fp_values = fpany_absint_to_float_torch_allnorm(param_dict=param_dict, sign=sign, abs_int=None, expo=expo, mant=mant)

    return fp_values




def show_value_space(expo_width, mant_width, custom_bias, allnorm=False, show_style=1):

    pair_space = []
    for expo in range(2**expo_width):
        for mant in range(2**mant_width):
            pair_space.append([expo, mant])

    value_space = torch.tensor([ i for i in range(0, 2**(expo_width+mant_width))])
    param_dict = param_prepare(expo_width, mant_width, custom_bias=custom_bias, debug_mode=False)


    if allnorm:
        value_space_fp = fpany_absint_to_float_torch_allnorm(param_dict=param_dict, sign=None, abs_int=value_space, expo=None, mant=None)
        mode_str = "A-N"
    else:
        value_space_fp = fpany_absint_to_float_torch(param_dict=param_dict, sign=None, abs_int=value_space, expo=None, mant=None)
        mode_str = "S-N"


    if show_style == 0:
        pass
    elif show_style == 1:
        print(f"The value space of E{expo_width}M{mant_width}, bias={custom_bias} is: (in {mode_str} mode) \n", value_space_fp.numpy())
    elif show_style == 2:
        print(f"The value space of E{expo_width}M{mant_width}, bias={custom_bias} is: (in {mode_str} mode) \n")
        for i in range(len(value_space)):
            print(f"expo={pair_space[i][0]}, mant={pair_space[i][1]}, value={value_space_fp.numpy()[i]}")

    return value_space_fp





if __name__ == '__main__':

    expo_width = 3
    mant_width = 4
    default_bias = 2**(expo_width-1) - 1
    # custom_bias = default_bias
    custom_bias = 5
    param_dict = param_prepare(expo_width, mant_width, custom_bias=custom_bias, debug_mode=True)

    np.set_printoptions(suppress=True)
    # show_value_space(expo_width, mant_width, custom_bias, allnorm=False, show_style=1)
    show_value_space(expo_width, mant_width, custom_bias, allnorm=False, show_style=2)
    # show_value_space(expo_width, mant_width, custom_bias, allnorm=True, show_style=1)
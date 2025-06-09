import torch
import numpy as np
import timeit


def param_prepare(expo_width, mant_width, custom_bias=None, debug_mode=False):

    """
    MARK: In this version, we will not use Sub-Normal value.
    MARK: No matter Expo is 0 or not,  Value = (-1)^Sx * 2^(Expo-Bias) * (1+Mant)
    """

    # * Bias can be custom
    if custom_bias is not None:
        fp_bias = custom_bias
    else:
        fp_bias = int((2**(expo_width - 1)) - 1)

    # * Parameters preparing
    bias_double  = int(2 * fp_bias)
    max_expo     = int(2**expo_width - 1)
    max_mant     = int(2**mant_width - 1)
    mant_scale   = int(2**mant_width)
    max_value    = (2**(max_expo - fp_bias)) * (2 - 2**(-mant_width))
    min_value    = (2**(0 - fp_bias)) * (1 + 2**(-mant_width))
    resolution   = (2**(0 - fp_bias)) * 2**(-mant_width)
    max_norm_int = int(2**(expo_width+mant_width) - 1)
    OF_UF_mod    = int(2**(expo_width+mant_width))

    param_dict = {
        "expo_width"   : expo_width   ,
        "mant_width"   : mant_width   ,
        "fp_bias"      : fp_bias      , 
        "bias_double"  : bias_double  , 
        "max_expo"     : max_expo     , 
        "max_mant"     : max_mant     , 
        "mant_scale"   : mant_scale   , 
        "max_value"    : max_value    , 
        "min_value"    : min_value    , 
        "resolution"   : resolution   , 
        "max_norm_int" : max_norm_int , 
        "OF_UF_mod"    : OF_UF_mod    , 
    }

    if debug_mode:
        print(f"\n======== Parameters preparation for FP{1+expo_width+mant_width}_E{expo_width}M{mant_width} ========")
        for key, value in param_dict.items():
            print(f"{type(value)} : {key} = {value}")
        print("=====================================================\n")

    return param_dict








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
    max_value  = param_dict["max_value"]
    min_value  = param_dict["min_value"]
    resolution = param_dict["resolution"]
    max_expo   = param_dict["max_expo"]
    max_mant   = param_dict["max_mant"]
    mant_scale = param_dict["mant_scale"]

    # * Preprocess
    values = torch.as_tensor(values, dtype=torch.float32)    # Ensure it's a torch tensor with float32 dtype
    
    # * Open this if you want to consider values in [min_value/2, min_value) as min_value
    # But is the input values have already been quantized, than this one is useless
    # values = torch.round(values / resolution) * resolution

    # * Extracting
    # torch.frexp(): Decomposes input into mantissa and exponent tensors, such that input = mantissa ∈ (-1,1) x 2^exponent
    mant, expo = torch.frexp(values)

    # * Consider 0
    zero_mask = (values > -min_value) & (values < min_value)

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
        overflow_mask = (values < -max_value) | (values > max_value)
        expo = torch.where(overflow_mask, torch.tensor(max_expo, dtype=torch.int32), expo)
        mant = torch.where(overflow_mask, torch.tensor(max_mant, dtype=torch.int32), mant)

    if return_extract:
        return expo, mant
    else:
        return expo * torch.tensor(mant_scale, dtype=torch.int32) + mant





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





def show_value_space(expo_width, mant_width, custom_bias):

    value_space = torch.tensor([ i for i in range(0, 2**(expo_width+mant_width))])
    param_dict = param_prepare(expo_width, mant_width, custom_bias=custom_bias, debug_mode=False)
    value_space_fp = fpany_absint_to_float_torch_allnorm(param_dict=param_dict, sign=None, abs_int=value_space, expo=None, mant=None)

    print(f"The value space of E{expo_width}M{mant_width}, bias={custom_bias} is:\n", value_space_fp.numpy())

    return value_space_fp





if __name__ == "__main__":

    expo_width = 3
    mant_width = 4

    # if True:
    if False:
        # A = torch.tensor([0, 0.001, 0.004, 0.015, 0.025, 0.035, 0.045, 0.045])
        Iact_bias = 3
        # Iact = torch.tensor([0.5625])
        Iact = torch.tensor([0])
        param_dict_Iact = param_prepare(expo_width, mant_width, custom_bias=Iact_bias, debug_mode=True)

        Wght_bias = 5
        Wght = torch.tensor([-0.109375])
        param_dict_Wght = param_prepare(expo_width, mant_width, custom_bias=Wght_bias, debug_mode=True)


        Iact_int = float_to_fpany_absint_torch_allnorm(param_dict_Iact, Iact, clip_OF=False, return_extract=True)
        Wght_int = float_to_fpany_absint_torch_allnorm(param_dict_Wght, Wght, clip_OF=False, return_extract=True)
        
        print(Iact_int)
        # print(Wght_int)


        Oact_bias = 5
        Oact = torch.tensor([-0.0615234375])
        param_dict_Oact = param_prepare(expo_width, mant_width, custom_bias=Oact_bias, debug_mode=True)
        Oact_int = float_to_fpany_absint_torch_allnorm(param_dict_Oact, Oact, clip_OF=False, return_extract=True)
        # print(Oact_int)
    

    if True:
    # if False:
        custom_bias = 10
        value_space_fp = show_value_space(expo_width, mant_width, custom_bias)
        # value_space_fp = torch.tensor([0.0010376*0.25, 0.0010376*0.5, 0.0010376*0.75, 0.0010376])
        param_dict = param_prepare(expo_width, mant_width, custom_bias=custom_bias, debug_mode=False)
        expo, mant = float_to_fpany_absint_torch_allnorm(param_dict, value_space_fp, clip_OF=False, return_extract=True)
        print("expo =\n", expo)
        print("mant =\n", mant)
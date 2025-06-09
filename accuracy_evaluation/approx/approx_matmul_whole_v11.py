import torch
import numpy as np
import random

from approx.fpany_convert import param_prepare, show_value_space
from approx.fpany_convert import float_to_fpany_absint_torch, float_to_fpany_absint_torch_allnorm
from approx.fpany_convert import fpany_absint_to_float_torch, fpany_absint_to_float_torch_allnorm
from approx.fpany_convert import quant_to_fp_any_vectorize_torch, quant_to_fp_any_vectorize_torch_allnorm


# MARK: 这个版本要忠实量化


# 三个变量：
# 1. SubNormal view as normal ?
# 2. Compensation             ?
# 3. FlexBias                 ?

# (如果都搞不定再考虑 s2nn2s)


# Not considered:
    # OverFLow & Underflow
    # test_golden_quant_btw_mult_accu



def custom_matmul_vectorize(A, B, expo_width, mant_width,
                            
                            custom_bias_A, custom_bias_B, custom_bias_R,
                            comp_table_NN,

                            test_golden                                  = True,
                            test_golden_quant_btw_mult_accu              = True,         # Enable only when test_golden is on
                            test_golden_quant_btw_mult_accu_use_flexbias = True,         # Enable only when test_golden is on

                            test_baseline         = False,         # Only open one of them here
                            test_best_allnorm     = False,         # Only open one of them here
                            test_best_s2nn2s      = False,         # Only open one of them here

                            test_casestudy        = False,         # Case study will allow the below parameters
                            with_compensation     = False,         # Enable only when test_casestudy is on
                            with_view_all_as_norm = False,         # Enable only when test_casestudy is on
                            with_flexbias         = False,         # Enable only when test_casestudy is on
                            with_s2nn2s_opt       = False,         # Enable only when test_casestudy is on

                            # sim_hw_add_OFUF=False, with_OF_opt=False, with_UF_opt=False, golden_clip_OF=False,
                            
                            
                            debug_mode=False, self_check_mode=False
                            ):
                            
    assert A.shape[1] == B.shape[0]


    # * Parameters preparing
    A_param_dict = param_prepare(expo_width, mant_width, custom_bias=custom_bias_A, debug_mode=debug_mode)
    B_param_dict = param_prepare(expo_width, mant_width, custom_bias=custom_bias_B, debug_mode=debug_mode)
    R_param_dict = param_prepare(expo_width, mant_width, custom_bias=custom_bias_R, debug_mode=debug_mode)
    base_param_dict = param_prepare(expo_width, mant_width, custom_bias=None, debug_mode=debug_mode)           # No custom bias


    # **** Golden ****
    golden_result_3d = A.unsqueeze(2) * B.unsqueeze(0)
    golden_result_3d = golden_result_3d.float()          # float64 -> float32
    zero_mask_3d = (golden_result_3d == 0)               # remenber to check those zeros



    # MARK
    if test_golden:

        # * Quantization between mult & accumulate
        if test_golden_quant_btw_mult_accu:
            if test_golden_quant_btw_mult_accu_use_flexbias:
                golden_custom_bias = custom_bias_R
            else:
                golden_custom_bias = (2 ** (expo_width-1) - 1)

            golden_result_3d = quant_to_fp_any_vectorize_torch(golden_result_3d, expo_width, mant_width, custom_bias=golden_custom_bias, clip_OF=False)
            # print("\n quanted_golden_result_3d=\n", golden_result_3d.numpy())

        golden_result_2d = golden_result_3d.sum(dim=1)

        del golden_result_3d
        torch.cuda.empty_cache()     

        return golden_result_2d



    # MARK
    if test_baseline:

        # * View FP as Int
        A_expo_mant_int = float_to_fpany_absint_torch(base_param_dict, A, clip_OF=False, return_extract=False)
        B_expo_mant_int = float_to_fpany_absint_torch(base_param_dict, B, clip_OF=False, return_extract=False)

        # print("s-n\n", A_expo_mant_int)


        # * Bias should be fixed    
        B_neg = -((2 ** (expo_width-1) - 1) << mant_width)

        # * Approximate calculation: No Compensation
        temp_result_int_3d = approx_mult_baseline(
            A_expo_mant_int.unsqueeze(2), 
            B_expo_mant_int.unsqueeze(0), 
            B_neg
        )

        del A_expo_mant_int, B_expo_mant_int
        torch.cuda.empty_cache()


        # * Sign of the result        
        golden_result_sign_3d = torch.where(golden_result_3d < 0, torch.tensor(-1, dtype=torch.float32), torch.tensor(1, dtype=torch.float32))


        # * View FP as Int -> View FP as FP
        approx_result_fp_3d = fpany_absint_to_float_torch(param_dict=base_param_dict, sign=golden_result_sign_3d, abs_int=temp_result_int_3d, expo=None, mant=None)
        # print("approx_result_fp_3d =", approx_result_fp_3d)


        del golden_result_sign_3d, temp_result_int_3d
        torch.cuda.empty_cache()


        # * 3d -> 2d
        approx_result_2d = approx_result_fp_3d.sum(dim=1)

        del approx_result_fp_3d
        torch.cuda.empty_cache()

        return approx_result_2d



    # MARK
    if test_best_allnorm:

        # * View FP as Int
        A_expo, A_mant = float_to_fpany_absint_torch_allnorm(A_param_dict, A, clip_OF=False, return_extract=True)
        B_expo, B_mant = float_to_fpany_absint_torch_allnorm(B_param_dict, B, clip_OF=False, return_extract=True)


        mant_scale = 2**mant_width
        A_expo_mant_int = A_expo * mant_scale + A_mant
        B_expo_mant_int = B_expo * mant_scale + B_mant
        # print("all norm\n", A_expo_mant_int)

        del A_expo, B_expo
        torch.cuda.empty_cache()


        B_combine_neg = -((custom_bias_A + custom_bias_B - custom_bias_R) << mant_width)
        # print(B_combine_neg)


        # * Approximate calculation
        # TODO: Comp
        temp_result_int_3d = approx_mult_proposed(
            A_expo_mant_int.unsqueeze(2), 
            B_expo_mant_int.unsqueeze(0), 
            A_mant.unsqueeze(2), 
            B_mant.unsqueeze(0), 
            B_combine_neg, 
            comp_table_NN
        )

        del A_expo_mant_int, B_expo_mant_int, A_mant, B_mant
        torch.cuda.empty_cache()


        # * Sign of the result        
        golden_result_sign_3d = torch.where(golden_result_3d < 0, torch.tensor(-1, dtype=torch.float32), torch.tensor(1, dtype=torch.float32))


        # * View FP as Int -> View FP as FP
        approx_result_fp_3d = fpany_absint_to_float_torch_allnorm(param_dict=R_param_dict, sign=golden_result_sign_3d, abs_int=temp_result_int_3d, expo=None, mant=None)


        del golden_result_sign_3d, temp_result_int_3d
        torch.cuda.empty_cache()

        # print("approx_result_fp_3d =", approx_result_fp_3d)

        # * 3d -> 2d
        approx_result_2d = approx_result_fp_3d.sum(dim=1)

        del approx_result_fp_3d
        torch.cuda.empty_cache()

        return approx_result_2d



    # MARK
    if test_best_s2nn2s:
        pass






    # MARK
    if test_casestudy:

        if with_view_all_as_norm:
            if with_flexbias:
                # * View FP as Int
                A_expo, A_mant = float_to_fpany_absint_torch_allnorm(A_param_dict, A, clip_OF=False, return_extract=True)
                B_expo, B_mant = float_to_fpany_absint_torch_allnorm(B_param_dict, B, clip_OF=False, return_extract=True)
            else:
                # * View FP as Int
                A_expo, A_mant = float_to_fpany_absint_torch_allnorm(base_param_dict, A, clip_OF=False, return_extract=True)
                B_expo, B_mant = float_to_fpany_absint_torch_allnorm(base_param_dict, B, clip_OF=False, return_extract=True)

        else:
            if with_flexbias:
                # * View FP as Int
                A_expo, A_mant = float_to_fpany_absint_torch(A_param_dict, A, clip_OF=False, return_extract=True)
                B_expo, B_mant = float_to_fpany_absint_torch(B_param_dict, B, clip_OF=False, return_extract=True)
            else:
                # * View FP as Int
                A_expo, A_mant = float_to_fpany_absint_torch(base_param_dict, A, clip_OF=False, return_extract=True)
                B_expo, B_mant = float_to_fpany_absint_torch(base_param_dict, B, clip_OF=False, return_extract=True)


        mant_scale = 2**mant_width
        A_expo_mant_int = A_expo * mant_scale + A_mant
        B_expo_mant_int = B_expo * mant_scale + B_mant
        # print("all norm\n", A_expo_mant_int)

        del A_expo, B_expo
        torch.cuda.empty_cache()


        if with_flexbias:
            B_testcase_neg = -((custom_bias_A + custom_bias_B - custom_bias_R) << mant_width)
        else:
            B_testcase_neg = -((2 ** (expo_width-1) - 1) << mant_width)


        if with_compensation:
            # * Approximate calculation: With Compensation
            temp_result_int_3d = approx_mult_proposed(
                A_expo_mant_int.unsqueeze(2), 
                B_expo_mant_int.unsqueeze(0), 
                A_mant.unsqueeze(2), 
                B_mant.unsqueeze(0), 
                B_testcase_neg, 
                comp_table_NN
            )
        else:
            # * Approximate calculation: No Compensation
            temp_result_int_3d = approx_mult_baseline(
                A_expo_mant_int.unsqueeze(2), 
                B_expo_mant_int.unsqueeze(0), 
                B_testcase_neg
            )


        del A_expo_mant_int, B_expo_mant_int, A_mant, B_mant
        torch.cuda.empty_cache()


        # * Sign of the result        
        golden_result_sign_3d = torch.where(golden_result_3d < 0, torch.tensor(-1, dtype=torch.float32), torch.tensor(1, dtype=torch.float32))



        if with_view_all_as_norm:
            if with_flexbias:
                # * View FP as Int -> View FP as FP
                approx_result_fp_3d = fpany_absint_to_float_torch_allnorm(param_dict=R_param_dict, sign=golden_result_sign_3d, abs_int=temp_result_int_3d, expo=None, mant=None)
            else:
                # * View FP as Int -> View FP as FP
                approx_result_fp_3d = fpany_absint_to_float_torch_allnorm(param_dict=base_param_dict, sign=golden_result_sign_3d, abs_int=temp_result_int_3d, expo=None, mant=None)


        else:
            if with_flexbias:
                # * View FP as Int -> View FP as FP
                approx_result_fp_3d = fpany_absint_to_float_torch(param_dict=R_param_dict, sign=golden_result_sign_3d, abs_int=temp_result_int_3d, expo=None, mant=None)
            else:
                # * View FP as Int -> View FP as FP
                approx_result_fp_3d = fpany_absint_to_float_torch(param_dict=base_param_dict, sign=golden_result_sign_3d, abs_int=temp_result_int_3d, expo=None, mant=None)


        del golden_result_sign_3d, temp_result_int_3d
        torch.cuda.empty_cache()

        # print("approx_result_fp_3d =", approx_result_fp_3d)

        # * 3d -> 2d
        approx_result_2d = approx_result_fp_3d.sum(dim=1)

        del approx_result_fp_3d
        torch.cuda.empty_cache()

        return approx_result_2d





def approx_mult_baseline(x_int, y_int, B_neg):

    mult_result = x_int + y_int + B_neg
    
    return mult_result


def approx_mult_proposed(x_int, y_int, x_mant, y_mant, B_combine_neg, comp_table_NN):

    mult_result = x_int + y_int + B_combine_neg + comp_table_NN[x_mant.long(), y_mant.long()]
    
    return mult_result


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
def get_comp_table_NN(expo_width, mant_width, withComp, dnsmp_factor):


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

    def get_comp_table_NN_cpu(expo_width, mant_width, withComp, dnsmp_factor):

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
            ], dtype=torch.int32)

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
                ], dtype=torch.int32)
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
                ], dtype=torch.int32)


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
                ], dtype=torch.int32)
            
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
                ], dtype=torch.int32)

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
                ], dtype=torch.int32)

        else:
            comp_table_NN = torch.zeros((2**mant_width, 2**mant_width), dtype=torch.int32)

        return comp_table_NN




    def random_tensor_gen(expo_width, mant_width, custom_bias, row, col, seed=None):

        if seed is not None:
            np.random.seed(seed)
            random.seed(seed)

        value_space = show_value_space(expo_width, mant_width, custom_bias, allnorm=False, show_style=0)

        random_tensor = torch.tensor([[np.random.choice(value_space.numpy()) * random.choice([-1, 1]) for _ in range(col)] for _ in range(row)])

        return random_tensor






if __name__ == "__main__":

    expo_width = 3
    mant_width = 4


    dnsmp_factor = 3
    # dnsmp_factor = 4
    # dnsmp_factor = 5


    # * The withComp here is always true
    comp_table_NN = get_comp_table_NN_cpu(expo_width, mant_width, withComp=True, dnsmp_factor=dnsmp_factor)
    print("dnsmp_factor =", dnsmp_factor)
    print(comp_table_NN)


    param_prepare(expo_width, mant_width, custom_bias=None, debug_mode=False)


    Iact_bias = 3
    Wght_bias = 5
    Oact_bias = 5


    # * Random Tensor: Test case 1
    size = 16
    Iact_random = random_tensor_gen(expo_width, mant_width, custom_bias=Iact_bias, row=size, col=size, seed=8)
    Wght_random = random_tensor_gen(expo_width, mant_width, custom_bias=Wght_bias, row=size, col=size, seed=10)


    # print(Iact_random)
    # print(Wght_random)



    # * Example: Golden setting
    golden_result_2d = custom_matmul_vectorize(
        A                     = Iact_random, 
        B                     = Wght_random, 
        expo_width            = expo_width, 
        mant_width            = mant_width,                    
        custom_bias_A         = Iact_bias, 
        custom_bias_B         = Wght_bias, 
        custom_bias_R         = Oact_bias,
        comp_table_NN         = comp_table_NN,

        test_golden                                  = True,
        test_golden_quant_btw_mult_accu              = True,    # This will only effect the golden
        test_golden_quant_btw_mult_accu_use_flexbias = True,    # This will only effect the golden

        test_baseline         = False,
        test_best_allnorm     = False,
        test_best_s2nn2s      = False,

        test_casestudy        = False,
        with_compensation     = False,      # Enable only when test_casestudy is on
        with_view_all_as_norm = False,      # Enable only when test_casestudy is on
        with_flexbias         = False,      # Enable only when test_casestudy is on
        with_s2nn2s_opt       = False,      # Enable only when test_casestudy is on

        debug_mode            = False, 
        self_check_mode       = False
    )



    # * Example: Baseline setting
    baseline_result_2d = custom_matmul_vectorize(
        A                     = Iact_random, 
        B                     = Wght_random, 
        expo_width            = expo_width, 
        mant_width            = mant_width,                    
        custom_bias_A         = Iact_bias, 
        custom_bias_B         = Wght_bias, 
        custom_bias_R         = Oact_bias,
        comp_table_NN         = comp_table_NN,

        test_golden                                  = False,
        test_golden_quant_btw_mult_accu              = True,
        test_golden_quant_btw_mult_accu_use_flexbias = True,

        test_baseline         = True,
        test_best_allnorm     = False,
        test_best_s2nn2s      = False,

        test_casestudy        = False,
        with_compensation     = False,      # Enable only when test_casestudy is on
        with_view_all_as_norm = False,      # Enable only when test_casestudy is on
        with_flexbias         = False,      # Enable only when test_casestudy is on
        with_s2nn2s_opt       = False,      # Enable only when test_casestudy is on

        debug_mode            = False, 
        self_check_mode       = False
    )



    # * Example: Best setting: All-norm mode
    best_result_2d = custom_matmul_vectorize(
        A                     = Iact_random, 
        B                     = Wght_random, 
        expo_width            = expo_width, 
        mant_width            = mant_width,                    
        custom_bias_A         = Iact_bias, 
        custom_bias_B         = Wght_bias, 
        custom_bias_R         = Oact_bias,
        comp_table_NN         = comp_table_NN,

        test_golden                                  = False,
        test_golden_quant_btw_mult_accu              = True,
        test_golden_quant_btw_mult_accu_use_flexbias = True,

        test_baseline         = False,
        test_best_allnorm     = True,
        test_best_s2nn2s      = False,

        test_casestudy        = False,
        with_compensation     = False,      # Enable only when test_casestudy is on
        with_view_all_as_norm = False,      # Enable only when test_casestudy is on
        with_flexbias         = False,      # Enable only when test_casestudy is on
        with_s2nn2s_opt       = False,      # Enable only when test_casestudy is on

        debug_mode            = False, 
        self_check_mode       = False
    )



    # * Example: Case study
    casestudy_result_2d = custom_matmul_vectorize(
        A                     = Iact_random, 
        B                     = Wght_random, 
        expo_width            = expo_width, 
        mant_width            = mant_width,                    
        custom_bias_A         = Iact_bias, 
        custom_bias_B         = Wght_bias, 
        custom_bias_R         = Oact_bias,
        comp_table_NN         = comp_table_NN,

        test_golden                                  = False,
        test_golden_quant_btw_mult_accu              = True,
        test_golden_quant_btw_mult_accu_use_flexbias = True,

        test_baseline         = False,
        test_best_allnorm     = False,
        test_best_s2nn2s      = False,

        test_casestudy        = True,       # Case study will allow the below parameters
        with_compensation     = False,      # Enable only when test_casestudy is on
        with_view_all_as_norm = False,      # Enable only when test_casestudy is on
        with_flexbias         = False,      # Enable only when test_casestudy is on
        with_s2nn2s_opt       = False,      # Enable only when test_casestudy is on

        debug_mode            = False, 
        self_check_mode       = False
    )



    # print("\n golden_result_2d\n", golden_result_2d)
    # print("\n baseline_result_2d\n", baseline_result_2d)
    # print("\n best_result_2d\n", best_result_2d)


    print("RMSE of Baseline        :", torch.sqrt(torch.mean((golden_result_2d - baseline_result_2d) ** 2)).item())
    print("RMSE of Best (All-Norm) :", torch.sqrt(torch.mean((golden_result_2d - best_result_2d) ** 2)).item())
    print("RMSE of Case Study      :", torch.sqrt(torch.mean((golden_result_2d - casestudy_result_2d) ** 2)).item())
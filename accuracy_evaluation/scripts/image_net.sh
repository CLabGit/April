#!/bin/bash
device=0

seed=10

image_dir="/home/zou/data/ImageNet"
model_dir="/home/zou/codes/April/model_dir/mobilenet_v2.pth.tar"

architecture="mobilenet_v2_quantized"
# architecture="resnet18_quantized_approx"
# architecture="deit_quantized_approx"
# architecture="fastvit_t8_quantized_approx"

batch_size=64

n_bits=8

expo_width=3
mant_width=4
dnsmp_factor=3

approx_output_dir="/home/zou/codes/April/approx_output"

# v9
CUDA_VISIBLE_DEVICES=$device python image_net.py validate-quantized \
    --images-dir ${image_dir} \
    --architecture ${architecture} \
    --batch-size ${batch_size} \
    --seed ${seed} \
    --model-dir ${model_dir} \
    --n-bits ${n_bits}  \
    --cuda \
    --load-type fp32 \
    --quant-setup all \
    --qmethod fp_quantizer \
    --per-channel \
    --fp8-mantissa-bits=$mant_width \
    --fp8-set-maxval \
    --no-fp8-mse-include-mantissa-bits \
    --weight-quant-method=current_minmax \
    --act-quant-method=allminmax \
    --num-est-batches=1 \
    --quantize-input \
    --approx_flag \
    --no-quantize-after-mult-and-add \
    --res-quantizer-flag \
    --no-original-quantize-res \
    --expo-width ${expo_width} \
    --mant-width ${mant_width} \
    --dnsmp-factor ${dnsmp_factor} \
    --no-withComp \
    --with_approx \
    --with_s2nn2s_opt \
    --no-sim_hw_add_OFUF \
    --no-with_OF_opt \
    --no-with_UF_opt \
    --no-golden-clip-OF \
    --no-quant_btw_mult_accu \
    --no-debug-mode \
    --no-self-check-mode \
    --approx-output-dir ${approx_output_dir} \
    --test_casestudy \
    --no-with_flexbias 

# # v11
# CUDA_VISIBLE_DEVICES=$device python image_net.py validate-quantized \
#     --images-dir ${image_dir} \
#     --architecture ${architecture} \
#     --batch-size ${batch_size} \
#     --seed ${seed} \
#     --model-dir ${model_dir} \
#     --n-bits ${n_bits}  \
#     --cuda \
#     --load-type fp32 \
#     --quant-setup all \
#     --qmethod fp_quantizer \
#     --per-channel \
#     --fp8-mantissa-bits=$mant_width \
#     --fp8-set-maxval \
#     --no-fp8-mse-include-mantissa-bits \
#     --weight-quant-method=current_minmax \
#     --act-quant-method=allminmax \
#     --num-est-batches=1 \
#     --reestimate-bn-stats \
#     --quantize-input \
#     --no-approx_flag \
#     --no-quantize-after-mult-and-add \
#     --no-res-quantizer-flag \
#     --original-quantize-res \
#     --expo-width ${expo_width} \
#     --mant-width ${mant_width} \
#     --dnsmp-factor ${dnsmp_factor} \
#     --withComp \
#     --no-test_golden \
#     --no-test_golden_quant_btw_mult_accu \
#     --no-test_golden_quant_btw_mult_accu_use_flexbias \
#     --no-test_baseline \
#     --test_best_allnorm \
#     --test_best_s2nn2s \
#     --no-test_casestudy \
#     --with_compensation \
#     --with_view_all_as_norm \
#     --with_flexbias \
#     --with_s2nn2s_opt \
#     --no-debug-mode \
#     --no-self-check-mode \
#     --approx-output-dir ${approx_output_dir} 
#!/bin/bash

seed=10

image_dir="/home/zou/data/ImageNet"
model_dir="/home/zou/codes/FP8-quantization/model_dir/mobilenet_v2.pth.tar"

architecture="demo_quantized"

batch_size=64

n_bits=8


python image_net.py validate-quantized-demo \
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
    --fp8-mantissa-bits=5 \
    --fp8-set-maxval \
    --no-fp8-mse-include-mantissa-bits \
    --weight-quant-method=current_minmax \
    --act-quant-method=allminmax \
    --num-est-batches=1
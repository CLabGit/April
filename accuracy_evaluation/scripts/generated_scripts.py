import os
from itertools import product

base_script = """#!/bin/bash
device={device}
seed={seed}
image_dir={image_dir}
model_dir={model_dir}
architecture={architecture}
batch_size={batch_size}
n_bits={n_bits}
expo_width={expo_width}
mant_width={mant_width}
dnsmp_factor={dnsmp_factor}
approx_output_dir={approx_output_dir}

CUDA_VISIBLE_DEVICES=$device python image_net.py validate-quantized \\
    --images-dir ${{image_dir}} \\
    --architecture ${{architecture}} \\
    --batch-size ${{batch_size}} \\
    --seed ${{seed}} \\
    --model-dir ${{model_dir}} \\
    --n-bits ${{n_bits}}  \\
    --cuda \\
    --load-type fp32 \\
    --quant-setup all \\
    --qmethod fp_quantizer \\
    --per-channel \\
    --fp8-mantissa-bits=$mant_width \\
    --fp8-set-maxval \\
    --no-fp8-mse-include-mantissa-bits \\
    --weight-quant-method=current_minmax \\
    --act-quant-method=allminmax \\
    --num-est-batches=1 \\
    --quantize-input \\
    {approx_flag} \\
    {quantize_after_mult_and_add_flag} \\
    {res_quantizer_flag} \\
    --no-original-quantize-res \\
    --expo-width ${{expo_width}} \\
    --mant-width ${{mant_width}} \\
    --dnsmp-factor ${{dnsmp_factor}} \\
    {withComp_flag} \\
    {with_approx_flag} \\
    {with_s2nn2s_opt_flag} \\
    --no-sim_hw_add_OFUF \\
    --no-with_OF_opt \\
    --no-with_UF_opt \\
    --no-golden-clip-OF \\
    {quant_btw_mult_accu_flag} \\
    --no-debug-mode \\
    --no-self-check-mode \\
    --approx-output-dir ${{approx_output_dir}} 
"""
# 生成不同 flag 组合的 shell 脚本
device = 0
seed = 10
image_dir = "/home/zou/data/ImageNet"
model_dir = "/home/zou/codes/FP8-quantization/model_dir/mobilenet_v2.pth.tar"
architecture = "mobilenet_v2_quantized_approx"
batch_size = 16
n_bits = 8
expo_width = 3
mant_width = 4
# dnsmp_factor = 3
approx_output_dir = "/home/zou/codes/FP8-quantization/approx_output"


# run_method flags
approx_flag = ["--approx_flag", "--no-approx_flag"]
quantize_after_mult_and_add_flag = ["--quantize-after-mult-and-add", "--no-quantize-after-mult-and-add"]
res_quantizer_flag = ["--res-quantizer-flag", "--no-original-quantize-res"]

# approx_options flags
withComp_flag = ["--withComp", "--no-withComp"]
with_approx_flag = ["--with_approx", "--no-with_approx"]
with_s2nn2s_opt_flag = ["--with_s2nn2s_opt", "--no-with_s2nn2s_opt"]
quant_btw_mult_accu_flag = ["--quant_btw_mult_accu", "--no-quant_btw_mult_accu"]


# 创建输出目录
output_dir = "/home/zou/codes/FP8-quantization/scripts/generated_scripts"
os.makedirs(output_dir, exist_ok=True)
output_dir = os.path.join(output_dir, architecture)
os.makedirs(output_dir, exist_ok=True)
output_dir = os.path.join(output_dir, "E{}M{}".format(expo_width, mant_width))
os.makedirs(output_dir, exist_ok=True)

exit_loop = False

for i0, withComp in enumerate(withComp_flag):
    for i1, dnsmp_factor in enumerate(iter([3, 4, 5])):
        for i2, with_s2nn2s_opt in enumerate(with_s2nn2s_opt_flag):
            for i3, quant_btw_mult_accu in enumerate(quant_btw_mult_accu_flag):
                i = i0 * 3 * len(with_s2nn2s_opt_flag) * len(quant_btw_mult_accu_flag) + i1 * len(with_s2nn2s_opt_flag) * len(quant_btw_mult_accu_flag) + i2 * len(quant_btw_mult_accu_flag) + i3
                script_content = base_script.format(
                    device=device,
                    seed=seed,
                    image_dir=image_dir,
                    model_dir=model_dir,
                    architecture=architecture,
                    batch_size=batch_size,
                    n_bits=n_bits,
                    expo_width=expo_width,
                    mant_width=mant_width,
                    dnsmp_factor=dnsmp_factor,
                    approx_output_dir=approx_output_dir,
                    approx_flag=approx_flag[0],
                    quantize_after_mult_and_add_flag=quantize_after_mult_and_add_flag[1],
                    res_quantizer_flag=res_quantizer_flag[0],
                    withComp_flag=withComp,
                    with_approx_flag=with_approx_flag[0],
                    with_s2nn2s_opt_flag=with_s2nn2s_opt,
                    quant_btw_mult_accu_flag=quant_btw_mult_accu
                )
        
                # 生成文件名并写入脚本
                script_filename = os.path.join(output_dir, f"run_script_{i}.sh")
                with open(script_filename, 'w') as f:
                    f.write(script_content)
                
                if i0 == 1:
                    exit_loop = True
                    break
            if exit_loop:
                break
        if exit_loop:
            break
    if exit_loop:
        break
    # print(f"Generated: {script_filename}")

print("All scripts generated successfully in the 'generated_scripts' folder.")

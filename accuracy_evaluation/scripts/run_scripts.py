import os
import subprocess

# 定义包含所有 .sh 文件的目录
expo_width=3
mant_width=4
architecture="mobilenet_v2_quantized_approx"
script_dir = "/home/zou/codes/FP8-quantization/scripts/generated_scripts"
script_dir = os.path.join(script_dir, architecture)
script_dir = os.path.join(script_dir, "E{}M{}".format(expo_width, mant_width))

# 遍历目录并逐个执行 .sh 文件
for script_name in os.listdir(script_dir):  # 遍历 'generated_scripts' 文件夹中的所有文件
    script_path = os.path.join(script_dir, script_name)
    if script_path.endswith(".sh"):  # 确保文件以 .sh 结尾，即只对 .sh 文件进行操作
        print(f"Executing: {script_path}")
        
        # 使用 subprocess 执行 .sh 文件
        subprocess.run(["bash", script_path])
        
        print(f"Completed: {script_path}")
print("All scripts executed successfully.")

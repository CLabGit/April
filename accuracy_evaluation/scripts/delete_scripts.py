import os

# 定义包含所有 .sh 文件的目录
expo_width=3
mant_width=4
architecture="mobilenet_v2_quantized_approx"
script_dir = "/home/zou/codes/FP8-quantization/scripts/generated_scripts"
script_dir = os.path.join(script_dir, architecture)
script_dir = os.path.join(script_dir, "E{}M{}".format(expo_width, mant_width))

# 遍历目录并删除所有 .sh 文件
for script_name in os.listdir(script_dir):
    script_path = os.path.join(script_dir, script_name)
    if script_path.endswith(".sh"):
        os.remove(script_path)
        print(f"Deleted: {script_path}")
print("All scripts deleted successfully.")

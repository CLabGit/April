# April: Accuracy-Improved Floating-Point Approximation For Neural Network Accelerators


The FP8 evaluations are based on previous Qualcomm's work "FP8 Quantization: The Power of the Exponent".
fork from https://github.com/Qualcomm-AI-research/FP8-quantization

## How to install
Make sure to have Python ≥3.8 (tested with Python 3.8.10 and 3.9.19) and 
ensure the latest version of `pip` (tested with 21.3.1):
```bash
conda create -n fp8_quantization python=3.9
conda activate fp8_quantization
```

Next, install PyTorch >= 2.4.0 (tested with PyTorch 2.4.0 and 2.4.1) with the appropriate CUDA version (tested with CUDA 11.8):
```bash
conda install pytorch torchvision torchaudio pytorch-cuda=11.8 -c pytorch -c nvidia
```

Finally, install the remaining dependencies using pip:
```bash
pip install -r requirements.txt
```


## Running experiments
### ImageNet experiments
The main run file to reproduce the ImageNet experiments is `image_net.py`. It contains commands for validating models quantized with post-training quantization. You can see the full list of options for each command using `python image_net.py [COMMAND] --help`.

You should update proper path in `scripts/image_net.sh` and `models/*.py` before running experiments. Pre-trained weights are in `model_dir/`.
We provide the code to reproduce the floating-point multiplication approximation (FPMA) results for MobileNetV2, Fastvit-t12, Deit-tiny, and Mobileone pre-trained on ImageNet.

To reproduce the experiments run:
```bash
sh scripts/image_net.sh
 ```

where <ARCHITECTURE_NAME> can be mobilenet_v2_quantized_approx ,resnet_quantized_approx, fastvit_t12_quantized_approx, deit_quantized_approx and mobileone_quantized_approx. 

## Notice
We load ImageNet dataset as the following structure:  
ImageNet/  
|  ├── train/  
|  │     └── ...   
|  └── val/   
|        ├── 0/    
|        ├── 1/  
|        ├── 2/  
|        └── ...   
If you would like to use a structure like this:  
ImageNet/  
|  ├── train/  
|  │     └── ...  
|  └── val/  
|        ├── 000/  
|        ├── 001/  
|        ├── 002/  
|        └── ...  
You should:  
1. **Comment out lines 119-154** in `utils/imagenet_dataloaders.py`.
2. **Uncomment (de-annotate) lines 105-117** in the same file.

This adjustment will allow your dataset to load in the alternative directory structure.



import math

import torch
import torch.nn as nn
import torch.nn.functional as F

from quantization.autoquant_utils import quantize_model, Flattener, QuantizedActivationWrapper
from quantization.base_quantized_classes import QuantizedActivation, FP32Acts
from quantization.base_quantized_model import QuantizedModel


class DemoModel(nn.Module):
    def __init__(self):
        super(DemoModel, self).__init__()
        self.fc = nn.Linear(10, 10)
    def forward(self, x):
        x = self.fc(x)
        return x

class QuantizedDemoModel(QuantizedModel):
    def __init__(self, model_fp, quant_setup=None, **quant_params):
        super().__init__()
        specials = {}
        
        quantize_input = quant_setup and quant_setup == "LSQ_paper"
        self.fc = quantize_model(
            model_fp.fc,
            **quant_params,
        )
        
    def forward(self, x):
        x = self.fc(x)
        return x
    
def demo_quantized(pretrained=True, model_dir=None, load_type="fp32", **quant_params):
    fp_model = DemoModel()
    quant_model = QuantizedDemoModel(fp_model, **quant_params)
    return quant_model
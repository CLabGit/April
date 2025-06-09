import torch.nn as nn
from approx.approx_calculation import *
from models.resnet_quantized import *

from quantization.base_quantized_classes import QuantizedActivation

from quantization.autoquant_utils import QuantConv, QuantLinear
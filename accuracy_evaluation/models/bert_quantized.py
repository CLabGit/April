import os
import re
import torch
from collections import OrderedDict
from quantization.autoquant_utils import quantize_sequential, Flattener, quantize_model, BNQConv
from quantization.base_quantized_classes import QuantizedActivation, FP32Acts
from quantization.base_quantized_model import QuantizedModel

from transformers import BertTokenizer, BertModel



tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
model = BertModel.from_pretrained("bert-base-uncased")
text = "Replace me by any text you'd like."
encoded_input = tokenizer(text, return_tensors='pt')
output = model(**encoded_input)


def bert_quantized(pretrained=True, model_dir=None, load_type="fp32", **qparams):
    if pretrained and load_type == "fp32":
        tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
        model = BertModel.from_pretrained("bert-base-uncased")
        text = "Replace me by any text you'd like."
        encoded_input = tokenizer(text, return_tensors='pt')
        output = model(**encoded_input)
        print(output)

    return model
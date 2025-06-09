import math

import torch
import torch.nn as nn
import torch.nn.functional as F

import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter

__all__ = ["MobileNetV2"]


def conv_bn(inp, oup, stride):
    return nn.Sequential(
        nn.Conv2d(inp, oup, 3, stride, 1, bias=False), nn.BatchNorm2d(oup), nn.ReLU6(inplace=True)
    )


def conv_1x1_bn(inp, oup):
    return nn.Sequential(
        nn.Conv2d(inp, oup, 1, 1, 0, bias=False), nn.BatchNorm2d(oup), nn.ReLU6(inplace=True)
    )


class InvertedResidual(nn.Module):
    def __init__(self, inp, oup, stride, expand_ratio):
        super(InvertedResidual, self).__init__()
        self.stride = stride
        assert stride in [1, 2]

        hidden_dim = round(inp * expand_ratio)
        self.use_res_connect = self.stride == 1 and inp == oup

        if expand_ratio == 1:
            self.conv = nn.Sequential(
                # dw
                nn.Conv2d(hidden_dim, hidden_dim, 3, stride, 1, groups=hidden_dim, bias=False),
                nn.BatchNorm2d(hidden_dim),
                nn.ReLU6(inplace=True),
                # pw-linear
                nn.Conv2d(hidden_dim, oup, 1, 1, 0, bias=False),
                nn.BatchNorm2d(oup),
            )
        else:
            self.conv = nn.Sequential(
                # pw
                nn.Conv2d(inp, hidden_dim, 1, 1, 0, bias=False),
                nn.BatchNorm2d(hidden_dim),
                nn.ReLU6(inplace=True),
                # dw
                nn.Conv2d(hidden_dim, hidden_dim, 3, stride, 1, groups=hidden_dim, bias=False),
                nn.BatchNorm2d(hidden_dim),
                nn.ReLU6(inplace=True),
                # pw-linear
                nn.Conv2d(hidden_dim, oup, 1, 1, 0, bias=False),
                nn.BatchNorm2d(oup),
            )

    def forward(self, x):
        if self.use_res_connect:
            return x + self.conv(x)
        else:
            return self.conv(x)


class MobileNetV2(nn.Module):
    def __init__(self, n_class=1000, input_size=224, width_mult=1.0, dropout=0.0):
        super().__init__()
        block = InvertedResidual
        input_channel = 32
        last_channel = 1280
        inverted_residual_setting = [
            # t, c, n, s
            [1, 16, 1, 1],
            [6, 24, 2, 2],
            [6, 32, 3, 2],
            [6, 64, 4, 2],
            [6, 96, 3, 1],
            [6, 160, 3, 2],
            [6, 320, 1, 1],
        ]

        # building first layer
        assert input_size % 32 == 0
        input_channel = int(input_channel * width_mult)
        self.last_channel = int(last_channel * width_mult) if width_mult > 1.0 else last_channel
        features = [conv_bn(3, input_channel, 2)]
        # building inverted residual blocks
        for t, c, n, s in inverted_residual_setting:
            output_channel = int(c * width_mult)
            for i in range(n):
                if i == 0:
                    features.append(block(input_channel, output_channel, s, expand_ratio=t))
                else:
                    features.append(block(input_channel, output_channel, 1, expand_ratio=t))
                input_channel = output_channel
        # building last several layers
        features.append(conv_1x1_bn(input_channel, self.last_channel))
        features.append(nn.AvgPool2d(input_size // 32))
        # make it nn.Sequential
        self.features = nn.Sequential(*features)

        # building classifier
        self.classifier = nn.Sequential(
            nn.Dropout(dropout),
            nn.Linear(self.last_channel, n_class),
        )

        self._initialize_weights()

    def forward(self, x):
        x = self.features(x)
        x = F.adaptive_avg_pool2d(x, 1).squeeze()  # type: ignore[arg-type] # accepted slang
        x = self.classifier(x)
        return x

    def _initialize_weights(self):
        for idx, m in enumerate(self.modules()):
            # print(idx, m)
            if isinstance(m, nn.Conv2d):
                n = m.kernel_size[0] * m.kernel_size[1] * m.out_channels
                m.weight.data.normal_(0, math.sqrt(2.0 / n))
                if m.bias is not None:
                    m.bias.data.zero_()
            elif isinstance(m, nn.BatchNorm2d):
                m.weight.data.fill_(1)
                m.bias.data.zero_()
            elif isinstance(m, nn.Linear):
                n = m.weight.size(1)
                m.weight.data.normal_(0, 0.01)
                m.bias.data.zero_()
                

if __name__ == "__main__":
    model = MobileNetV2()
    state_dict = torch.load("/home/zou/codes/FP8-quantization/model_dir/mobilenet_v2.pth.tar")
    model.load_state_dict(state_dict)
    # a = nn.Linear(10, 10)
    # b = nn.Linear(10, 10)
    # c = nn.Sequential(a, b, a)
    # for idx, m in enumerate(c.modules()):
    #     print(idx, m)
    layer_info = []

    def traverse_model(model, prefix=''):
        for name, module in model.named_children():
            current_name = f'{prefix}.{name}' if prefix else name

            if isinstance(module, nn.Conv2d) or isinstance(module, nn.Linear):
                weight = module.weight.data
                layer_info.append({
                    'name': current_name,
                    'type': type(module).__name__,
                    'weight': weight,
                })
            elif isinstance(module, InvertedResidual):
                inverted_residual_name = current_name + '(InvertedResidual)'
                # print(f"Found InvertedResidual block: {inverted_residual_name}")
                for idx, sub_module in enumerate(module.conv):
                    sub_module_name = f'{inverted_residual_name}.conv.{idx}'
                    if isinstance(sub_module, nn.Conv2d):
                        weight = sub_module.weight.data
                        layer_info.append({
                            'name': sub_module_name,
                            'type': type(sub_module).__name__,
                            'weight': weight,
                        })
            else:
                traverse_model(module, current_name)

    # 开始遍历模型
    traverse_model(model)

    # 打印每个层的信息
    for idx, layer in enumerate(layer_info):
        print(f"Layer {idx + 1}:")
        print(f"  Name: {layer['name']}")
        print(f"  Type: {layer['type']}")
        print(f"  Weight shape: {layer['weight'].shape}")
        # print(f"  Weight data: {layer['weight']}")
        weight_flatten = layer['weight'].flatten()
        weight_flatten_np = weight_flatten.cpu().numpy()
        print(f"  Weight flatten shape: {weight_flatten.shape}")
        counts, bins, patches = plt.hist(weight_flatten_np, bins=30, alpha=0.7, color='blue')
        percentages = (counts / counts.sum()) * 100

        plt.clf()
        plt.bar(bins[:-1], percentages, width=(bins[1] - bins[0]), color='blue', alpha=0.7)

        # Should the y axis in percentage
        plt.gca().yaxis.set_major_formatter(FuncFormatter(lambda x, _: f'{x:.0f}%'))

        # Percentage tag on each bar
        for i in range(len(bins) - 1):
            plt.text(bins[i], percentages[i] + 0.2, f'{percentages[i]:.1f}%', ha='center', va='bottom')


        plt.xlabel('Value')
        plt.ylabel('Percentage')
        plt.title(f"Distribution of {layer['name']} Values")
        plt.show()
        print("-" * 50)
    
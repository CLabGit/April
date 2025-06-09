# Copyright (c) 2022 Qualcomm Technologies, Inc.
# All Rights Reserved.
import logging
import os

import click
from ignite.contrib.handlers import ProgressBar
from ignite.engine import create_supervised_evaluator, Events
from ignite.metrics import Accuracy, TopKCategoricalAccuracy, Loss
from torch.nn import CrossEntropyLoss
from torch.utils.data import DataLoader

from quantization.utils import pass_data_for_range_estimation
from utils import DotDict
from utils.click_options import (
    qat_options,
    quantization_options,
    fp8_options,
    quant_params_dict,
    base_options,
    approx_options,
    approx_params_dict,
    run_method_options,
    run_method_dict,
)
from utils.qat_utils import get_dataloaders_and_model, ReestimateBNStats, get_model

import torch
import matplotlib.pyplot as plt
import numpy as np
import math

from utils.CustomBatchSampler import CustomIterableDataset, get_mini_dataloader

from validate import validate

class Config(DotDict):
    pass


@click.group()  # create a group of commands, can be used to group multiple commands together
def fp8_cmd_group():
    logging.basicConfig(level=os.environ.get("LOGLEVEL", "INFO"))


pass_config = click.make_pass_decorator(Config, ensure=True)


@fp8_cmd_group.command()
@pass_config
@base_options
@fp8_options
@quantization_options
@qat_options
@click.option(
    "--load-type",
    type=click.Choice(["fp32", "quantized"]),
    default="quantized",
    help='Either "fp32", or "quantized". Specify weather to load a quantized or a FP ' "model.",
)
@approx_options
@run_method_options
def validate_quantized(config, load_type):
    """
    function for running validation on pre-trained quantized models
    """
    print("Setting up network and data loaders")
    qparams = quant_params_dict(config)
    approx_params = approx_params_dict(config)
    run_method = run_method_dict(config)
    qparams["custom_approx_params"] = approx_params
    qparams["run_method"] = run_method
    dataloaders, model = get_dataloaders_and_model(config=config, load_type=load_type, **qparams)
    
    # from approx.replace_operations_in_mobilenet_v2 import replace_operations_in_mobilenet_v2_quantized
    # replace_operations_in_mobilenet_v2_quantized(model, **qparams)
    # if config.base.cuda:
    #     model = model.cuda()
    # print("replace done")
    model.estimate_ranges()
    if load_type == "fp32":
        # Estimate ranges using training data
        pass_data_for_range_estimation(
            loader=dataloaders.train_loader,
            model=model,
            act_quant=config.quant.act_quant,
            weight_quant=config.quant.weight_quant,
            max_num_batches=config.quant.num_est_batches,
        )
        # Ensure we have the desired quant state
        model.set_quant_state(config.quant.weight_quant, config.quant.act_quant)


    # Fix ranges
    model.fix_ranges()
    # model.approx_calculation()
    # '''
    # test
    # '''
    # val_loader = dataloaders.val_loader
    # with torch.no_grad():
    #     for i, data in enumerate(val_loader):
    #         images, labels = data
    #         print(f"Batch {i}:")
    #         print("Image Batch Shape:", images.shape)  # e.g., [10, 3, 224, 224]
    #         print("Labels:", labels)  # e.g., tensor([0, 0, 0, ..., 0])
    #         output = model(data[0].to("cuda"))
    #         # print(output.logits.shape)
    #         # print(output.logits)
    #         print(output.max(-1))
    #         class_idx = labels[0].item()
    #         class_name = val_loader.dataset.classes[class_idx]
    #         image_path, _ = val_loader.dataset.samples[i * val_loader.batch_size]
    #         print(f"Image {i * val_loader.batch_size}: Path={image_path}, Label={class_idx} (Class={class_name})")

            
    #         images = data[0].cpu().numpy()
    #         image = images[0]
    #         image = np.transpose(image, (1, 2, 0))
    #         image = (image - image.min()) / (image.max() - image.min())
    #         image = (image * 255).astype(np.uint8)
            
    #         plt.imshow(image)
    #         plt.axis('off')  # 不显示坐标轴
    #         plt.show()
    #         if i == 300:

    #             break
    
    # return


    # Create evaluator
    loss_func = CrossEntropyLoss()
    metrics = {
        "top_1_accuracy": Accuracy(),
        "top_5_accuracy": TopKCategoricalAccuracy(),
        "loss": Loss(loss_func),
    }

    pbar = ProgressBar(persist=True)
    evaluator = create_supervised_evaluator(
        model=model, metrics=metrics, device="cuda" if config.base.cuda else "cpu"
    )
    pbar.attach(evaluator)
    print("Model with the ranges estimated:\n{}".format(model))
    # BN Re-estimation
    if config.qat.reestimate_bn_stats:
        ReestimateBNStats(
            model, dataloaders.train_loader, num_batches=int(0.01 * len(dataloaders.train_loader)) # use 0.0002 * len(dataloaders.train_loader on the serve
        )(None)

    print("Start quantized validation")
    num_batches = 10
    step = len(dataloaders.val_loader)/num_batches
    
    # print(len(mini_dataloader))
    # evaluator.run(dataloaders.val_loader)
    # evaluate_param = "full_test"
    evaluate_param = "mini_test"
    print(f"run_method: {run_method}")
    print(f"approx_params: {approx_params}")
    print(f"evaluate_param: {evaluate_param}")
    with torch.inference_mode():
        if evaluate_param == "full_test":
            # evaluator.run(dataloaders.val_loader)
            loss, top1k_acc, top5k_acc = validate(val_loader=dataloaders.val_loader, model=model, criterion=loss_func, device="cuda" if config.base.cuda else "cpu")
        elif evaluate_param == "mini_test":
            # evaluator.run(random_sampler)
            loss, top1k_acc, top5k_acc = validate(val_loader=dataloaders.mini_val_loader, model=model, criterion=loss_func, device="cuda" if config.base.cuda else "cpu")
    # final_metrics = evaluator.state.metrics
    # print(final_metrics)
    
    
    if config.approx.approx_output_dir is not None:
        import datetime
        output_dir = config.approx.approx_output_dir
        arch = str(config.base.architecture)
        output_dir = os.path.join(output_dir, arch)
        os.makedirs(output_dir, exist_ok=True)
        expo_width = approx_params["expo_width"]
        mant_width = approx_params["mant_width"]
        dnsmp_factor = approx_params["dnsmp_factor"]
        current_time = datetime.datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
        output_dir = os.path.join(output_dir, "E{}M{}D{}".format(expo_width, mant_width, dnsmp_factor))
        os.makedirs(output_dir, exist_ok=True)
        output_file_path = output_dir + f"/D{dnsmp_factor}_{current_time}.txt"
        with open(output_file_path, "w") as f:
            f.write(f"evaluate_param: {evaluate_param}\n")
            f.write(f"run_method: {run_method}\n")
            f.write(f"approx_params: {approx_params}\n")
            f.write(f"loss: {loss}, top1k_acc: {top1k_acc}, top5k_acc: {top5k_acc}")
            # f.write(f"final_metrics: {final_metrics}\n")


@fp8_cmd_group.command()
@pass_config
@base_options
@fp8_options
@quantization_options
@qat_options
@click.option(
    "--load-type",
    type=click.Choice(["fp32", "quantized"]),
    default="quantized",
    help='Either "fp32", or "quantized". Specify weather to load a quantized or a FP ' "model.",
)
def validate_quantized_demo(config, load_type):
    """
    function for demo test fp8 quantization
    """
    model = get_model(config, load_type)
    input_tensor = torch.rand(10, 10)
    print(input_tensor)
    pass

if __name__ == "__main__":
    fp8_cmd_group()
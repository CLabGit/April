#!/usr/bin/env python
# Copyright (c) 2021 Qualcomm Technologies, Inc.
# All Rights Reserved.

import os

import torchvision
import torch.utils.data as torch_data
from torchvision import transforms
from utils import BaseEnumOptions


class ImageInterpolation(BaseEnumOptions):
    nearest = transforms.InterpolationMode.NEAREST
    box = transforms.InterpolationMode.BOX
    bilinear = transforms.InterpolationMode.BILINEAR
    hamming = transforms.InterpolationMode.HAMMING
    bicubic = transforms.InterpolationMode.BICUBIC
    lanczos = transforms.InterpolationMode.LANCZOS


class ImageNetDataLoaders(object):
    """
    Data loader provider for ImageNet images, providing a train and a validation loader.
    It assumes that the structure of the images is
        images_dir
            - train
                - label1
                - label2
                - ...
            - val
                - label1
                - label2
                - ...
    """

    def __init__(
        self,
        images_dir: str,
        image_size: int,
        batch_size: int,
        num_workers: int,
        interpolation: transforms.InterpolationMode,
    ):
        """
        Parameters
        ----------
        images_dir: str
            Root image directory
        image_size: int
            Number of pixels the image will be re-sized to (square)
        batch_size: int
            Batch size of both the training and validation loaders
        num_workers
            Number of parallel workers loading the images
        interpolation: transforms.InterpolationMode
            Desired interpolation to use for resizing.
        """

        self.images_dir = images_dir
        self.batch_size = batch_size
        self.num_workers = num_workers

        # For normalization, mean and std dev values are calculated per channel
        # and can be found on the web.
        # normalize = transforms.Normalize(mean=[0.485, 0.456, 0.406], std=[0.229, 0.224, 0.225])
        normalize = transforms.Normalize(mean=[0.5, 0.5, 0.5], std=[0.5, 0.5, 0.5])

        self.train_transforms = transforms.Compose(
            [
                transforms.RandomResizedCrop(image_size, interpolation=interpolation.value),
                transforms.RandomHorizontalFlip(),
                transforms.ToTensor(),
                normalize,
            ]
        )

        self.val_transforms = transforms.Compose(
            [
                transforms.Resize(image_size + 24, interpolation=interpolation.value),
                transforms.CenterCrop(image_size),
                transforms.ToTensor(),
                normalize,
            ]
        )

        self._train_loader = None
        self._val_loader = None
        self._mini_val_loader = None

    @property
    def train_loader(self) -> torch_data.DataLoader:
        if not self._train_loader:
            root = os.path.join(self.images_dir, "train")
            train_set = torchvision.datasets.ImageFolder(root, transform=self.train_transforms)
            self._train_loader = torch_data.DataLoader(
                train_set,
                batch_size=self.batch_size,
                shuffle=True,
                num_workers=self.num_workers,
                pin_memory=True,
            )
        return self._train_loader

    # @property
    # def val_loader(self) -> torch_data.DataLoader:
    #     if not self._val_loader:
    #         root = os.path.join(self.images_dir, "val")
    #         val_set = torchvision.datasets.ImageFolder(root, transform=self.val_transforms)
    #         self._val_loader = torch_data.DataLoader(
    #             val_set,
    #             batch_size=self.batch_size,
    #             shuffle=False,
    #             num_workers=self.num_workers,
    #             pin_memory=True,
    #         )
    #     return self._val_loader
    
    @property
    def val_loader(self) -> torch_data.DataLoader:
        if not self._val_loader:
            root = os.path.join(self.images_dir, "val")
            val_set = torchvision.datasets.ImageFolder(root, transform=self.val_transforms)
            
            # 1. 按数字排序类名
            try:
                sorted_classes = sorted(val_set.classes, key=lambda x: int(x))
            except ValueError:
                raise ValueError("所有类名必须是可以转换为整数的字符串，例如 '0', '1', '2', ...")
            
            # # 2. 创建新的 class_to_idx 映射，将类名直接映射为其整数值
            new_class_to_idx = {cls_name: int(cls_name) for cls_name in sorted_classes}
            
            # # 3. 更新数据集的 class_to_idx
            val_set.class_to_idx = new_class_to_idx
            # print(val_set.samples[:150])
            # 4. 重新映射样本的类索引
            val_set.samples = [
                (path, val_set.class_to_idx[val_set.classes[original_class_idx]])
                for path, original_class_idx in val_set.samples
            ]
            
            # 5. 更新 targets（如果存在）
            if hasattr(val_set, 'targets'):
                val_set.targets = [s[1] for s in val_set.samples]
                
            self._val_loader = torch_data.DataLoader(
                val_set,
                batch_size=self.batch_size,
                shuffle=False,
                num_workers=self.num_workers,
                pin_memory=True,
            )
        return self._val_loader

    @property
    def mini_val_loader(self) -> torch_data.DataLoader:
        if not self._mini_val_loader:
            root = os.path.join(self.images_dir, "val")
            val_set = torchvision.datasets.ImageFolder(root, transform=self.val_transforms)

            # 1. 按数字排序类名
            try:
                sorted_classes = sorted(val_set.classes, key=lambda x: int(x))
            except ValueError:
                raise ValueError("所有类名必须是可以转换为整数的字符串，例如 '0', '1', '2', ...")

            # 2. 创建新的 class_to_idx 映射，将类名直接映射为其整数值
            new_class_to_idx = {cls_name: int(cls_name) for cls_name in sorted_classes}
            val_set.class_to_idx = new_class_to_idx

            # 3. 收集每个类别中的样本，限制为 10 个
            samples_per_class = {}
            for path, original_class_idx in val_set.samples:
                class_name = val_set.classes[original_class_idx]
                class_idx = val_set.class_to_idx[class_name]

                if class_idx not in samples_per_class:
                    samples_per_class[class_idx] = []

                # 只保留每个类别前 10 个样本
                if len(samples_per_class[class_idx]) < 10:
                    samples_per_class[class_idx].append((path, class_idx))

            # 4. 合并所有类别的样本
            new_samples = []
            for class_idx, samples in samples_per_class.items():
                new_samples.extend(samples)

            # 5. 更新 val_set 的 samples 和 targets
            val_set.samples = new_samples
            if hasattr(val_set, 'targets'):
                val_set.targets = [s[1] for s in new_samples]

            # 6. 创建 DataLoader
            self._mini_val_loader = torch_data.DataLoader(
                val_set,
                batch_size=self.batch_size,
                shuffle=False,
                num_workers=self.num_workers,
                pin_memory=True,
            )
        return self._mini_val_loader
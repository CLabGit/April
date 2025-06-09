import random
from torch.utils.data import IterableDataset, DataLoader, Subset
import torch
        
class CustomIterableDataset(IterableDataset):
    def __init__(self, dataloader, num_batches, random_sample=False, start_index=0, step=1):
        self.dataloader = dataloader
        self.num_batches = num_batches
        self.random_sample = random_sample
        self.start_index = start_index
        self.step = int(step)
        self.total_batches = len(dataloader)
        self.indices = self._get_indices()

    def _get_indices(self):
        if self.random_sample:
            return sorted(random.sample(range(self.total_batches), min(self.num_batches, self.total_batches)))
        else:
            return list(range(self.start_index, self.total_batches, self.step))[:self.num_batches]

    def __iter__(self):
        for i, batch in enumerate(self.dataloader):
            if i in self.indices:
                yield batch
            if len(self.indices) == 0:
                break
    
    
class CustomIterableDataset(IterableDataset):
    def __init__(self, dataloader, num_batches, random_sample=False, start_index=0, step=1):
        self.sampler = CustomIterableDataset(dataloader, num_batches, random_sample, start_index, step)

    def __iter__(self):
        yield from self.sampler
        
def get_mini_dataloader(dataloader, num_batches, random_sample=False, start_index=0, step=1):
    new_dataloader = dataloader
    selected_batches = []
    selected_num_batches = 0
    for i, batch in enumerate(new_dataloader):
        if selected_num_batches >= num_batches:
            break
        elif i == start_index or i % step == 0:
            selected_batches.append(batch)
            selected_num_batches += 1
    inputs_list, labels_list = zip(*selected_batches)
    inputs = torch.cat(inputs_list)
    labels = torch.cat(labels_list)
    
    from torch.utils.data import TensorDataset
    subset_dataset = TensorDataset(inputs, labels)
    new_val_loader = DataLoader(
        subset_dataset,
        batch_size=dataloader.batch_size,
        shuffle=False,  # 通常验证集不需要打乱
        num_workers=dataloader.num_workers
    )
    return new_val_loader

class CustomDataset:
    def __init__(self, dataloader, num_batches, random_sample=False, start_index=0, step=1):
        self.dataloader = dataloader
        self.num_batches = num_batches
        self.random_sample = random_sample
        self.start_index = start_index
        self.step = int(step)
        self.total_batches = len(dataloader)
        self.indices = self._get_indices()

    def _get_indices(self):
        if self.random_sample:
            return sorted(random.sample(range(self.total_batches), min(self.num_batches, self.total_batches)))
        else:
            return list(range(self.start_index, self.total_batches, self.step))[:self.num_batches]

    def __iter__(self):
        for i, batch in enumerate(self.dataloader):
            if i in self.indices:
                yield batch
            if len(self.indices) == 0:
                break
            

import torch
import torch.nn as nn
import torchvision.datasets as datasets
import torchvision.transforms as transforms
import torch.distributed as dist
import time
import torchvision.transforms.functional as tf
import torch.nn.functional as F
from tqdm import tqdm

def validate(val_loader, model, criterion, device):
    batch_time = AverageMeter()
    losses = AverageMeter()
    top1 = AverageMeter()
    top5 = AverageMeter()

    # switch to evaluate mode
    model.eval()

    val_start_time = end = time.time()
    loop = tqdm(enumerate(val_loader), leave=True, total=len(val_loader))
    for i, (data, target) in loop:
        data = data.to(device)
        target = target.to(device)

        with torch.no_grad():
            output = model(data)
            # data_cpu = data.cpu()
            # imshow(data_cpu)
            
        loss = criterion(output, target)

        # measure accuracy and record loss
        prec1, prec5 = accuracy(output.data, target, topk=(1, 5))
        # print(f'acc1: {prec1}  acc5: {prec5}  loss: {loss}')
        # break
        losses.update(loss.data.item(), data.size(0))
        top1.update(prec1.data.item(), data.size(0))
        top5.update(prec5.data.item(), data.size(0))
        # print(f'top1: {top1.val:.3f} ({top1.avg:.3f})  top5: {top5.val:.3f} ({top5.avg:.3f}')

        # measure elapsed time
        batch_time.update(time.time() - end)
        end = time.time()

        loop.set_description('Test ')
        loop.set_postfix_str('Time {batch_time.val:.3f} ({batch_time.avg:.3f})   '
                            'Loss {loss.val:.4f} ({loss.avg:.4f})   '
                            'Prec@1 {top1.val:.3f} ({top1.avg:.3f})   '
                            'Prec@5 {top5.val:.3f} ({top5.avg:.3f})'.format(
                                batch_time=batch_time,
                                loss=losses,
                                top1=top1,
                                top5=top5,
                            ))
        # if i % args.print_freq == 0:
        #     print('Test: [{0}/{1}]\t'
        #           'Time {batch_time.val:.3f} ({batch_time.avg:.3f})\t'
        #           'Loss {loss.val:.4f} ({loss.avg:.4f})\t'
        #           'Prec@1 {top1.val:.3f} ({top1.avg:.3f})\t'
        #           'Prec@5 {top5.val:.3f} ({top5.avg:.3f})'.format(
        #               i,
        #               len(val_loader),
        #               batch_time=batch_time,
        #               loss=losses,
        #               top1=top1,
        #               top5=top5,
        #           ))
    val_end_time = time.time()
    print(' * Prec@1 {top1.avg:.3f} Prec@5 {top5.avg:.3f} Time {time:.3f}'.
          format(top1=top1, top5=top5, time=val_end_time - val_start_time))

    return all_reduce_mean(losses.avg), all_reduce_mean(top1.avg), all_reduce_mean(top5.avg)


class AverageMeter(object):
    """Computes and stores the average and current value"""

    def __init__(self):
        self.reset()

    def reset(self):
        self.val = 0
        self.avg = 0
        self.sum = 0
        self.count = 0

    def update(self, val, n=1):
        self.val = val
        self.sum += val * n
        self.count += n
        self.avg = self.sum / self.count


def accuracy(output, target, topk=(1, )):
    """Computes the precision@k for the specified values of k"""
    maxk = max(topk)
    batch_size = target.size(0)

    _, pred = output.topk(maxk, 1, True, True)
    # print(pred, target)
    pred = pred.t()
    correct = pred.eq(target.reshape(1, -1).expand_as(pred))

    res = []
    for k in topk:
        correct_k = correct[:k].reshape(-1).float().sum(0)
        res.append(correct_k.mul_(100.0 / batch_size))
    return res


def is_dist_avail_and_initialized():
    if not dist.is_available():
        return False
    if not dist.is_initialized():
        return False
    return True

def get_world_size():
    if not is_dist_avail_and_initialized():
        return 1
    return dist.get_world_size()


def all_reduce_mean(x):
    world_size = get_world_size()
    if world_size > 1:
        x_reduce = torch.tensor(x).cuda()
        dist.all_reduce(x_reduce)
        x_reduce /= world_size
        return x_reduce.item()
    else:
        return x
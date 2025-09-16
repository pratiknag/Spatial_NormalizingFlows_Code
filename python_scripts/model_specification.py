#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Tuesday August  29 23:33:04 2023

@author: Pratik
"""

import numpy as np
import torch
import torch.utils.data as data
import torch.nn as nn
#import torch.optim as optim
from torchkit import optim
from torch.autograd import Variable
from torchkit import nn as nn_, flows, utils
from torchkit.transforms import from_numpy, binarize
# from torchvision.transforms import transforms
from ops import load_maf_data,transform_Data
from ops import DatasetWrapper
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")



import time 
import json
import argparse, os

m = nn.Tanh()
m1 = nn.Sigmoid()
class MAF(object):
    
    def __init__(self, args, p):

        self.args = args        
        self.__dict__.update(args.__dict__)
        self.p = p
        
        dim = p
        dimc = 1
        dimh = args.dimh
        flowtype = args.flowtype
        num_flow_layers = args.num_flow_layers
        num_ds_dim = args.num_ds_dim
        num_ds_layers = args.num_ds_layers
        fixed_order = args.fixed_order
                 
        act = nn.ELU()
        if flowtype == 'affine':
            flow = flows.IAF
        elif flowtype == 'dsf':
            flow = lambda **kwargs:flows.IAF_DSF(num_ds_dim=num_ds_dim,
                                                 num_ds_layers=num_ds_layers,
                                                 **kwargs)
        elif flowtype == 'ddsf':
            flow = lambda **kwargs:flows.IAF_DDSF(num_ds_dim=num_ds_dim,
                                                  num_ds_layers=num_ds_layers,
                                                  **kwargs)
        
        
        sequels = [nn_.SequentialFlow(
            flow(dim=dim,
                 hid_dim=dimh,
                 context_dim=dimc,
                 num_layers=args.num_hid_layers+1,
                 activation=act,
                 fixed_order=fixed_order)) for i in range(num_flow_layers)] + \
                  [flows.LinearFlow(dim, dimc),]
                
                
        self.flow = nn.Sequential(
                *sequels)
        
        
        
        if self.cuda:
            self.flow = self.flow.cuda()
        
        
        
        
    def density(self, spl1, spl2,params):
        
        n = spl1.size(0)
        context = Variable(torch.FloatTensor(n, 1).zero_()) 
        lgd = Variable(torch.FloatTensor(n).zero_())
        zeros = Variable(torch.FloatTensor(n, self.p).zero_())
        if self.cuda:
            context = context.cuda()
            lgd = lgd.cuda()
            zeros = zeros.cuda()
            
        z, logdet, _ = self.flow((spl1, lgd, context))
#         print("################ OUTSIDE SAMPLE")
#         print(z[0])
#         print("################ OUTSIDE SAMPLE")
#         z = nn_.sigmoid_(z)
        # z = 0.5 * m(z)
        if self.args.dataset == "TWIST_2D":
            z = 0.5 * m(z)
            r = utils.mult_normal(z, spl2, params)
        elif self.args.dataset == "argo3D":
            z = m1(z)
            r = utils.mult_normal(z, spl2, params)
        else:
            z = 0.5 * m(z)
            r = utils.mult_normal_exp(z, spl2, params)
#         logdet = logdet + nn_.logsigmoid(z) + nn_.logsigmoid(-z)
        
#         print("########### "+str(r))
#         losses = - r - logdet
        losses = - r 

#         losses =  torch.abs(logdet)
        return losses

    def density1(self, spl1, spl2,params):
        
        n = spl1.size(0)
        context = Variable(torch.FloatTensor(n, 1).zero_()) 
        lgd = Variable(torch.FloatTensor(n).zero_())
        zeros = Variable(torch.FloatTensor(n, self.p).zero_())
        if self.cuda:
            context = context.cuda()
            lgd = lgd.cuda()
            zeros = zeros.cuda()
            
        z, logdet, _ = self.flow((spl1, lgd, context))
#         print("################ OUTSIDE SAMPLE")
#         print(z[0])
#         print("################ OUTSIDE SAMPLE")
#         z = nn_.sigmoid_(z)
        # z = 0.5 * m(z)
        if self.args.dataset == "TWIST_2D":
            z = 0.5 * m(z)
            r = utils.mult_normal(spl1, spl2, params)
        elif self.args.dataset == "argo3D":
            z = m1(z)
            r = utils.mult_normal(z, spl2, params)
        else:
            z = 0.5 * m(z)
            r = utils.mult_normal_exp(z, spl2, params)
#         logdet = logdet + nn_.logsigmoid(z) + nn_.logsigmoid(-z)
        
#         print("########### "+str(r))
#         losses = - r - logdet
        losses = - r 
    
#         losses =  torch.abs(logdet)
        return losses

    def loss(self, x, y, params):
        return self.density(x,y, params)
    def loss1(self, x, y, params):
        return self.density1(x,y, params)
    
    def sample(self, spl):
        n = spl.size(0)
        context = Variable(torch.FloatTensor(n, 1).zero_()) 
        lgd = Variable(torch.FloatTensor(n).zero_())
        zeros = Variable(torch.FloatTensor(n, self.p).zero_())
        if self.cuda:
            context = context.cuda()
            lgd = lgd.cuda()
            zeros = zeros.cuda()
            
        z, logdet, _ = self.flow((spl, lgd, context))
#         losses = - utils.log_normal(z, zeros, zeros+1.0).sum(1) - logdet
#         z = nn_.sigmoid_(z)
        # z = 0.5 * m(z)
        if self.args.dataset == "argo3D":
            z = m1(z)
            return z
        else:
            z = 0.5 * m(z)
            return z
        
    def state_dict(self):
        return self.flow.state_dict()

    def load_state_dict(self, states):
        self.flow.load_state_dict(states)
         
    def clip_grad_norm(self):
        nn.utils.clip_grad_norm(self.flow.parameters(),
                                self.clip)



class model(object):
    
    patience = 2000
    
    def __init__(self, args, filename):
        
        self.__dict__.update(args.__dict__)

        self.filename = filename
        self.args = args        
        if args.dataset == 'step1D':
            p = 1
            df = transform_Data('raw_datasets/step1D.csv')
            np.save("raw_datasets/step1D.npy", df)
            path='step1D.npy'
            df = transform_Data('raw_datasets/test_locs1D.csv')
            np.save("raw_datasets/test_locs.npy", df)
            path1 = 'test_locs.npy'
            D = load_maf_data(path,path1)
        elif args.dataset == 'Monterrubio1D':
            p = 1
            df = transform_Data('raw_datasets/Monterrubio1D.csv')
            np.save("raw_datasets/Monterrubio1D.npy", df)
            path='Monterrubio1D.npy'
            df = transform_Data('raw_datasets/test_locs1D.csv')
            np.save("raw_datasets/test_locs.npy", df)
            path1 = 'test_locs.npy'
            D = load_maf_data(path,path1)
        elif args.dataset == 'AWU_RBF_2D':
            p = 2
            df = transform_Data('raw_datasets/AWU_RBF_2D.csv')
            np.save("raw_datasets/AWU_RBF_2D.npy", df)
            path='AWU_RBF_2D.npy'
            df = transform_Data('raw_datasets/test_locs2D.csv')
            np.save("raw_datasets/test_locs.npy", df)
            path1 = 'test_locs.npy'
            D = load_maf_data(path,path1)
        elif args.dataset == 'TWIST_2D':
            p = 2
            df = transform_Data('raw_datasets/TWIST_2D.csv')
            np.save("raw_datasets/TWIST_2D.npy", df)
            path='TWIST_2D.npy'
            df = transform_Data('raw_datasets/test_locs2D.csv')
            np.save("raw_datasets/test_locs.npy", df)
            path1 = 'test_locs.npy'
            D = load_maf_data(path,path1)
        elif args.dataset == 'AWU_RBF_LFT_2D':
            p = 2
            df = transform_Data('raw_datasets/AWU_RBF_LFT_2D.csv')
            np.save("raw_datasets/AWU_RBF_LFT_2D.npy", df)
            path='AWU_RBF_LFT_2D.npy'
            df = transform_Data('raw_datasets/test_locs2D.csv')
            np.save("raw_datasets/test_locs.npy", df)
            path1 = 'test_locs.npy'
            D = load_maf_data(path,path1)
        elif args.dataset == 'argo2D':
            p = 2
            df = transform_Data('raw_datasets/argo2D.csv')
            np.save("raw_datasets/argo2D.npy", df)
            path='argo2D.npy'
            df = transform_Data('raw_datasets/test_locs.csv')
            np.save("raw_datasets/test_locs.npy", df)
            path1 = 'test_locs.npy'
            D = load_maf_data(path,path1)
        elif args.dataset == 'argo3D':
            p = 3
            df = transform_Data('raw_datasets/argo3D.csv')
            np.save("raw_datasets/argo3D.npy", df)
            path='argo3D.npy'
            if args.test_dataset_argo3D == "sinloc":
                df = transform_Data('raw_datasets/test_locs_single-coordinate.csv')
            else:
                df = transform_Data('raw_datasets/test_locs_single-pres.csv')
            np.save("raw_datasets/test_locs_argo.npy", df)
            path1 = 'test_locs_argo.npy'
            D = load_maf_data(path,path1)
        
        tr, va, te, fd, fd_test = D.trn.x, D.val.x, D.tst.x, D.full_data.x, D.tst_data.x
            
            
        self.train_loader = data.DataLoader(tr, 
                                            batch_size=args.batch_size,
                                            shuffle=True)
        self.valid_loader = data.DataLoader(va, 
                                            batch_size=args.batch_size,
                                            shuffle=False)
        self.test_loader = data.DataLoader(te, 
                                           batch_size=args.batch_size,
                                           shuffle=False)
        self.Fdata_loader = data.DataLoader(fd, 
                                           batch_size=1000000,
                                           shuffle=False)
        self.fd_test_loader = data.DataLoader(fd_test, 
                                           batch_size=1000000,
                                           shuffle=False)
        
        self.maf = MAF(args, p)
        
        # optim
        amsgrad = bool(args.amsgrad)
        polyak = args.polyak
        self.optim = optim.Adam(self.maf.flow.parameters(),
                                lr=args.lr, 
                                betas=(args.beta1, args.beta2),
                                amsgrad=amsgrad,
                                polyak=polyak)
        
        
        # initialize checkpoint
        self.checkpoint = dict()
        self.checkpoint['best_val'] = float('inf')
        self.checkpoint['best_val_epoch'] = 0
        self.checkpoint['e'] = 0
        self.p = p
     
    def train(self, epoch, params):
        optim = self.optim
        t = 0 
        
        LOSSES = 0
        counter = 0
#         LOSSES1 = 0
#         counter1 = 0
        self.checkpoint['e'] = 0
        #for e in range(epoch):
        while self.checkpoint['e'] < epoch:
            for x1 in self.train_loader:
                x1 = x1.to(device)
                ### 1-d
                if self.p == 1:
                    x = torch.reshape(x1[:,0],(x1[:,0].shape[0],1))  
                    y = torch.reshape(x1[:,1],(x1[:,1].shape[0],1))
                ### 2-d
                elif self.p == 2:
                    x = torch.reshape(x1[:,:2],(x1[:,:2].shape[0],2))
                    y = torch.reshape(x1[:,2],(x1[:,2].shape[0],1))
                ### 3-d
                elif self.p == 3:
                    x = torch.reshape(x1[:,:3],(x1[:,:3].shape[0],3))
                    y = torch.reshape(x1[:,3],(x1[:,3].shape[0],1))
                    
                optim.zero_grad()
                x = Variable(x)
               
                if self.cuda:
                    x = x.cuda()
               
                    
                losses = self.maf.loss(x,y,params)
#                 losses1 = self.maf.loss1(x,y,params)
                loss = losses.mean()
#                 loss1 = losses1.mean()
                LOSSES += losses.sum().data.cpu().numpy()
                counter += 1
#                 LOSSES1 += losses1.sum().data.cpu().numpy()
#                 counter1 += losses1.size(0)
                loss.backward()
                self.maf.clip_grad_norm()
                optim.step()
                t += 1
                
                

            if self.checkpoint['e']%1 == 0:      
                optim.swap()
                loss_val = self.evaluate(self.valid_loader, params)
                loss_tst = self.evaluate(self.test_loader, params)       
                print('Epoch: [%4d/%4d] train <= %.2f '\
                      'valid: %.3f test: %.3f' % \
                (self.checkpoint['e']+1, epoch, LOSSES/float(counter), 
                 loss_val,
                 loss_tst))
#                 print('True density: [%4d/%4d] train <= %.2f '\
#                       'valid: %.3f test: %.3f' % \
#                 (self.checkpoint['e']+1, epoch, LOSSES1/float(counter1), 
#                  loss_val,
#                  loss_tst))
                if loss_tst < self.checkpoint['best_val']:
                    print(' [^] Best validation loss [^] ... [saving]')
                    self.save(self.save_dir+'/'+self.filename+'_best')
                    self.checkpoint['best_val'] = loss_tst
                    self.checkpoint['best_val_epoch'] = self.checkpoint['e']+1
                    
                LOSSES = 0
                counter = 0
                optim.swap()
                
            self.checkpoint['e'] += 1
            if (self.checkpoint['e'])%5 == 0:
                self.save(self.save_dir+'/'+self.filename+'_last')
            
            if self.impatient():
                print('Terminating due to impatience ... \n')
                break 
            
        # loading best valid model (early stopping)
        self.load(self.save_dir+'/'+self.filename+'_best')

    def impatient(self):
        current_epoch = self.checkpoint['e']
        bestv_epoch = self.checkpoint['best_val_epoch']
        return current_epoch - bestv_epoch > self.patience
        
        
    def evaluate(self, dataloader,params):
        LOSSES = 0 
        c = 0
        for x1 in dataloader:
            x1 = x1.to(device)
            ### 1-d
            if self.p == 1:
                x = torch.reshape(x1[:,0],(x1[:,0].shape[0],1))  
                y = torch.reshape(x1[:,1],(x1[:,1].shape[0],1))
            ### 2-d
            elif self.p == 2:
                x = torch.reshape(x1[:,:2],(x1[:,:2].shape[0],2))
                y = torch.reshape(x1[:,2],(x1[:,2].shape[0],1))
            ### 3-d
            elif self.p == 3:
                x = torch.reshape(x1[:,:3],(x1[:,:3].shape[0],3))
                y = torch.reshape(x1[:,3],(x1[:,3].shape[0],1))
               
            x = Variable(x)
            if self.cuda:
                x = x.cuda()
                
            losses = self.maf.loss(x,y,params).data.cpu().numpy()
            LOSSES += losses.sum()
            c += 1
        return LOSSES / float(c)


    def save(self, fn):        
        torch.save(self.maf.state_dict(), fn+'_model.pt')
        torch.save(self.optim.state_dict(), fn+'_optim.pt')
        with open(fn+'_args.txt','w') as out:
            out.write(json.dumps(self.args.__dict__,indent=4))
        with open(fn+'_checkpoint.txt','w') as out:     
            out.write(json.dumps(self.checkpoint,indent=4))

    def load(self, fn):
        self.maf.load_state_dict(torch.load(fn+'_model.pt'))
        self.optim.load_state_dict(torch.load(fn+'_optim.pt'))
    
    
    def resume(self, fn):
        self.load(fn)
        self.checkpoint.update(
            json.loads(open(fn+'_checkpoint.txt','r').read()))
    
    def get_warping(self, dataloader, test_locs):
    

        if test_locs:
            Z_sample = []
            for x1 in dataloader:
                x1 = x1.to(device)

                if self.p == 1:
                    x = x1[:, 0].reshape(-1, 1)
                elif self.p == 2:
                    x = x1[:, :2].reshape(-1, 2)
                elif self.p == 3:
                    x = x1[:, :3].reshape(-1, 3)

                x = Variable(x).to(device)
                zi = self.maf.sample(x)   # stays as torch.Tensor on device
                Z_sample.append(zi)

            Z_sample = torch.cat(Z_sample, dim=0)
            return Z_sample

        else:
            Z_sample, Y, x_sample = [], [], []
            for x1 in dataloader:
                if self.p == 1:
                    x = x1[:, 0].reshape(-1, 1)
                    y = x1[:, 1].reshape(-1, 1)
                elif self.p == 2:
                    x = x1[:, :2].reshape(-1, 2)
                    y = x1[:, 2].reshape(-1, 1)
                elif self.p == 3:
                    x = x1[:, :3].reshape(-1, 3)
                    y = x1[:, 3].reshape(-1, 1)

                x = Variable(x).to(device)
                y = y.to(device)

                zi = self.maf.sample(x)

                Z_sample.append(zi)
                Y.append(y)
                x_sample.append(x)

            Z_sample = torch.cat(Z_sample, dim=0)
            Y = torch.cat(Y, dim=0)
            x_sample = torch.cat(x_sample, dim=0)

            return Z_sample, Y, x_sample
            
    

# =============================================================================
# main
# =============================================================================


"""parsing and configuration"""
def parse_args():
    desc = "MAF"
    parser = argparse.ArgumentParser(description=desc)

    parser.add_argument('--dataset', type=str, default='AWU_RBF_2D', 
                        choices=['step1D',
                                 'Monterrubio1D',
                                 'AWU_RBF_2D',
                                 'AWU_RBF_LFT_2D',
                                 'TWIST_2D',
                                 'argo2D',
                                 'argo3D'])
    parser.add_argument('--test_dataset_argo3D', type=str, default='sinloc', 
                        choices=['sinloc',
                                 'sinpress'])
    parser.add_argument('--epoch', type=int, default=500, 
                        help='The number of epochs to run')
    parser.add_argument('--batch_size', type=int, default= 5000, 
                        help='The size of batch')
    parser.add_argument('--save_dir', type=str, default='models',
                        help='Directory name to save the model')
    parser.add_argument('--result_dir', type=str, default='results',
                        help='Directory name to save the generated images')
    parser.add_argument('--log_dir', type=str, default='logs',
                        help='Directory name to save training logs')
    parser.add_argument('--seed', type=int, default=1997,
                        help='Random seed')
    parser.add_argument('--fn', type=str, default='0',
                        help='Filename of model to be loaded')
    parser.add_argument('--to_train', type=int, default=1,
                        help='1 if to train 0 if not')
    parser.add_argument('--lr', type=float, default=0.0001)  # 0.001
    parser.add_argument('--clip', type=float, default=5.0)
    parser.add_argument('--beta1', type=float, default=0.9)
    parser.add_argument('--beta2', type=float, default=0.999)
    parser.add_argument('--amsgrad', type=int, default=0)
    parser.add_argument('--polyak', type=float, default=0.0)
    parser.add_argument('--cuda', type=bool, default=False)
    parser.add_argument('--step_size_GP', type=float, default=0.0001)
    parser.add_argument('--n_iter_GP', type=int, default=100)
    parser.add_argument('--init_sigma_sq', type=float, default=0.8)
    parser.add_argument('--init_range', type=float, default=0.1)
    parser.add_argument('--init_nugget', type=float, default=0.001)
    parser.add_argument('--max_iter', type=int, default=10)
    
    parser.add_argument('--dimh', type=int, default=100)
    parser.add_argument('--flowtype', type=str, default='ddsf')
    parser.add_argument('--num_flow_layers', type=int, default=5) # 1
    parser.add_argument('--num_hid_layers', type=int, default=5) # 10
    parser.add_argument('--num_ds_dim', type=int, default=16) # 10
    parser.add_argument('--num_ds_layers', type=int, default=5) # 5 7 is opt
    parser.add_argument('--fixed_order', type=bool, default=True,
                        help='Fix the made ordering to be the given order')
    
    
    return check_args(parser.parse_args())

"""checking arguments"""
def check_args(args):
    # --save_dir
    if not os.path.exists(args.save_dir):
        os.makedirs(args.save_dir)

    # --result_dir
    if not os.path.exists(args.result_dir + '_' + args.dataset):
        os.makedirs(args.result_dir + '_' + args.dataset)

    # --result_dir
    if not os.path.exists(args.log_dir):
        os.makedirs(args.log_dir)

    # --epoch
    try:
        assert args.epoch >= 1
    except:
        print('number of epochs must be larger than or equal to one')

    # --batch_size
    try:
        assert args.batch_size >= 1
    except:
        print('batch size must be larger than or equal to one')

    return args
   

def args2fn(args):
    
    prefix_key_pairs = [
        ('', 'dataset'),
        ('e', 'epoch'),
        ('s', 'seed'),
        ('p', 'polyak'),
        ('h', 'dimh'),
        ('f', 'flowtype'),
        ('fl', 'num_flow_layers'),
        ('l', 'num_hid_layers'),
        ('dsdim','num_ds_dim'),
        ('dsl', 'num_ds_layers'),
    ]
    
    return '_'.join([p+str(args.__dict__[k]) for p, k in prefix_key_pairs])
    
    

    
    

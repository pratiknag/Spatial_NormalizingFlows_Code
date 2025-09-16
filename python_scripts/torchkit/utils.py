#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 11 13:58:12 2017

@author: CW & Pratik
"""


import numpy as np

import torch
from scipy.spatial import distance_matrix
from scipy.special import kv, gamma
from torch.distributions import MultivariateNormal
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
delta = 1e-7
sigmoid = lambda x:torch.nn.functional.sigmoid(x) * (1-delta) + 0.5 * delta

c = - 0.5 * np.log(2*np.pi)
def log_normal(x, mean, log_var, eps=0.00001):
    return - (x-mean) ** 2 / (2. * torch.exp(log_var) + eps) - log_var/2. + c

def log_laplace(x, mean, log_scale, eps=0.00001):
    return - torch.abs(x-mean) / (torch.exp(log_scale) + eps) - log_scale - np.log(2)


def bceloss(pi, x):
    return - (x * torch.log(pi) + (1-x) * torch.log(1-pi))

def matern32_cov(x1, x2, params):
    """
    Matérn 3/2 covariance function.
    
    Args:
        x1 (torch.Tensor): shape [N, d]
        x2 (torch.Tensor): shape [M, d]
        params (torch.Tensor): [sigma2, lengthscale, nugget]

    Returns:
        torch.Tensor: [N, M] covariance matrix
    """
    sigma2, ell, nugget = params[0], params[1], params[2]
    dist = torch.cdist(x1, x2, p=2)  # pairwise Euclidean distances
    sqrt3 = np.sqrt(3.0)

    cov = sigma2 * (1.0 + sqrt3 * dist / ell) * torch.exp(-sqrt3 * dist / ell)

    # add nugget on the diagonal if square
    if x1.shape[0] == x2.shape[0]:
        cov = cov + nugget * torch.eye(x1.shape[0], device=x1.device)
    
    return cov

def matern12_cov(x1, x2, params):
    """
    Matérn 1/2 (Exponential) covariance function.
    
    Args:
        x1 (torch.Tensor): shape [N, d]
        x2 (torch.Tensor): shape [M, d]
        params (torch.Tensor): [sigma2, lengthscale, nugget]

    Returns:
        torch.Tensor: [N, M] covariance matrix
    """
    sigma2, ell, nugget = params[0], params[1], params[2]
    dist = torch.cdist(x1, x2, p=2)  # pairwise Euclidean distances

    cov = sigma2 * torch.exp(-dist / ell)

    # add nugget on the diagonal if square
    if x1.shape[0] == x2.shape[0]:
        cov = cov + nugget * torch.eye(x1.shape[0], device=x1.device)
    
    return cov

def mult_normal_exp(x,y, params):
    K = matern12_cov(x, x, params)  # [n, n]
    jitter = 1e-4
    K = K + jitter * torch.eye(x.shape[0], device=x.device)  # stabilize

    y = y.reshape(-1, 1)  # [n, 1]

    # Direct inverse
    K_inv = torch.linalg.inv(K)

    term1 = -0.5 * (y.T @ K_inv @ y).squeeze()
    sign, logdet = torch.slogdet(K)  # stable log determinant
    term2 = -0.5 * logdet
    term3 = -0.5 * x.shape[0] * torch.log(torch.tensor(2.0 * torch.pi, device=x.device))

    return term1 + term2 + term3

def mult_normal(x,y, params):
    K = matern32_cov(x, x, params)  # [n, n]
    jitter = 1e-5  # start small, increase if needed
    K = K + jitter * torch.eye(x.shape[0], device=x.device)
    L = torch.linalg.cholesky(K)    # [n, n]

    y = y.reshape(-1, 1)            # ensure [n, 1]

    # alpha = K^{-1} y, computed via Cholesky
    alpha = torch.cholesky_solve(y, L)  # [n, 1]

    term1 = -0.5 * (y.T @ alpha).squeeze()
    term2 = -torch.sum(torch.log(torch.diag(L)))
    term3 = -0.5 * x.shape[0] * torch.log(torch.tensor(2 * torch.pi, device=x.device))

    return term1 + term2 + term3

def categorical_kl(q, p, logq=None, logp=None):
    """ 
        compute the kl divergence KL(q||p) for categorical distributions q, p
        q, p : (batch_size, num_classes)
    """
    if logq is None:
        logq = torch.log(q)
    if logp is None:
        logp = torch.log(p)
    
    return (q * (logq - logp)).sum(1)


def factorial_gaussian_crossentropy(mean_q, log_var_q, mean_p, log_var_p, 
                                    eps=0.00001):
    """
    - E_q(log p)
    """
    return (
        log_var_p + (mean_q**2 +
                     mean_p**2 -
                     mean_q*mean_p*2 + 
                     torch.exp(log_var_q)) / (torch.exp(log_var_p) + eps) 
    )/2. - c



def factorial_gaussian_entropy(log_var_q):
    """
    - E_q(log q)
    """
    return (1+log_var_q)/2. - c
    

def varify(x):
    return torch.autograd.Variable(torch.from_numpy(x))

def oper(array,oper,axis=-1,keepdims=False):
    a_oper = oper(array)
    if keepdims:
        shape = []
        for j,s in enumerate(array.size()):
            shape.append(s)
        shape[axis] = -1
        a_oper = a_oper.view(*shape)
    return a_oper

def log_sum_exp(A, axis=-1, sum_op=torch.sum):    
    maximum = lambda x: x.max(axis)[0]    
    A_max = oper(A,maximum,axis,True)
    summation = lambda x: sum_op(torch.exp(x-A_max), axis)
    B = torch.log(oper(A,summation,axis,True)) + A_max    
    return B

def log_mean_exp(A, axis=-1):
    return log_sum_exp(A, axis, sum_op=torch.mean)

def log_sum_exp_np(A, axis=-1, sum_op=np.sum):    
    A_max = np.max(A, axis, keepdims=True)
    B = np.log(sum_op(np.exp(A-A_max),axis,keepdims=True)) + A_max    
    return B

def log_mean_exp_np(A, axis=-1):
    return log_sum_exp_np(A, axis, sum_op=np.mean)
    
    
def mul_grad_value(parameters, mul):
    if isinstance(parameters, torch.Tensor):
        parameters = [parameters]
    for p in parameters:
        p.grad.data.mul_(mul)

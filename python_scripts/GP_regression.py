#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Tuesday August  29 23:33:04 2023

@author: Pratik
"""

import scipy
from torchkit import nn as nn_, flows, utils 
import numpy as np
import torch


class GP_regression:
    
    def __init__(self, z, spl2):
        self.z = z
        self.spl2 = spl2

    def log_likelihood(self,params, data="TWIST_2D"):
        if (data == "TWIST_2D"):
            return (- utils.mult_normal(self.z, self.spl2, params))
        else:
            return (- utils.mult_normal_exp(self.z, self.spl2, params))  #.data.cpu().numpy().reshape(1)



    def GP_post(self, X1, y1, X2, params):
        """
        Calculate the posterior mean and covariance matrix for y2
        based on the corresponding input X2, the observations (y1, X1), 
        and the prior kernel function.
        """
        X1 = torch.tensor(X1)
        X2 = torch.tensor(X2)
        y1 = torch.tensor(y1)
        # Kernel of the observations
        sigma_train = utils.exponential_cov(X1,X1, params)
        # Kernel of observations vs to-predict
        sigma_test_train = utils.exponential_cov(X2,X1, params)
        sigma_train_test = torch.transpose(sigma_test_train, 0, 1)
        # Solve
        solved = torch.inverse(sigma_train)
#         solved = np.array(solved)
        # Compute posterior mean
        term1 = torch.matmul(sigma_test_train.float(),solved.float())
        mu2 = np.array(torch.matmul(term1.float(),y1.float()))
        # Compute the posterior covariance
        sigma_test = utils.exponential_cov(X2,X2, params)
        sigma2 = np.array(sigma_test - torch.matmul(term1.float(),sigma_train_test.float()))
        return mu2, sigma2  # mean, covariance
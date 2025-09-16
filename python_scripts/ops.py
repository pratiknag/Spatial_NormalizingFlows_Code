# -*- coding: utf-8 -*-
"""
Created on Wed Feb 21 16:47:13 2024
@author: Pratik Nag
"""

PARAM_EXTENSION = 'params'

import pickle
import numpy as np
from sklearn.preprocessing import OneHotEncoder
floatX = 'float32'

import scipy.io
from torch.utils.data import Dataset as Dataset

import external_maf as maf
import pandas as pd


class DatasetWrapper(Dataset):

    def __init__(self, dataset, transform=None):

        self.dataset = dataset
        self.transform = transform
    
    def __len__(self):
        return len(self.dataset)

    def __getitem__(self, ind):
        
        sample = self.dataset[ind]
        if self.transform:
            sample = self.transform(sample)
        
        return sample

class InputOnly(Dataset):

    def __init__(self, dataset):

        self.dataset = dataset
    
    def __len__(self):
        return len(self.dataset)

    def __getitem__(self, ind):
        return self.dataset[ind][0]

def transform_Data(path):
    df = pd.read_csv(path).to_numpy()
    return(df)



def load_maf_data(path,path1):
    return maf.DATA(path,path1)

    raise ValueError('Unknown dataset')














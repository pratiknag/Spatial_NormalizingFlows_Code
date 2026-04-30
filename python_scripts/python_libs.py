#!/usr/bin/env python
# coding: utf-8

"""
Copied from Bivariate DeepKriging https://github.com/pratiknag/Bivariate_DeepKriging/

Created on Tuesday August  29 23:33:04 2023

@author: Pratik
"""

import os
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'
import pandas as pd
import keras
from keras.models import Sequential,Model
from keras.layers import Dense, Dropout, BatchNormalization,Input
# from keras.wrappers.scikit_learn import KerasRegressor
from keras.callbacks import EarlyStopping, ModelCheckpoint
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import KFold
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
import numpy as np
from numpy import exp
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
import pylab 
import time
from tqdm import tqdm


# number of simulations
num_sim = 5

def mse(y_pred,y_true):
    mse = np.mean((y_pred-y_true)**2)
    return mse

def mae(y_pred,y_true):
    mae = np.mean(np.absolute(y_pred-y_true))
    return mae
# define and fit the model
def fit_model(X_train, y_train,base_model1):
    # DeepKriging model for continuous data
    ensemble_model = Sequential()

    ensemble_model.add(base_model1)
#     ensemble_model.add(Dense(50, activation = "relu"))
    ensemble_model.add(Dense(50, activation = "relu"))
    ensemble_model.add(Dense(1, activation='linear'))
    ensemble_model.layers[-3].trainable = False
    optimizer = keras.optimizers.Adam(learning_rate=0.01)
    ensemble_model.compile(optimizer=optimizer, loss='mse', metrics=['mse','mae'])
    ensemble_model.fit(X_train, y_train,
                       epochs = 200, batch_size = 128, verbose = 0)
    return ensemble_model

# fit an ensemble of models
def fit_ensemble(n_members, X_train, Y_train,base_model1):
    ensemble = list()
    for i in tqdm(range(n_members)):
#         x_train, x_test,y_train,y_test = train_test_split(X_train,Y_train,test_size = 0.2)
        # define and fit the model on the training set
        model = fit_model(X_train, Y_train,base_model1)
        # evaluate model on the test set
#         yhat = model.predict(x_test, verbose=0)
#         mae1 = mae(yhat, y_test)
#         print('>%d, MAE: %.3f' % (i+1, mae1))
        # store the model
        ensemble.append(model)
    return ensemble
def y_list_uni(yhat,i):
    the_list = list()
    for j in range(len(yhat)):
        the_list.append(yhat[j][0][i])
    return the_list
# make predictions with the ensemble and calculate a prediction interval
def variance(data):
    # Number of observations
    n = len(data)
    # Mean of the data
    mean = sum(data) / n
    # Square deviations
    deviations = [(x - mean) ** 2 for x in data]
    # Variance
    variance = sum(deviations) / (n-1)
    return variance
def predict_with_pi(ensemble, X):
    mean_vec1 = list()
    var_vec1 = list()
    # make predictions
    yhat = [model.predict(X, verbose=0) for model in ensemble]
    for data in tqdm(range(X.shape[0])):
        yhat1 = np.asarray(np.asarray(yhat)[:,data,:])
        var1_pred = yhat1[:,0]
        mean1 = var1_pred.mean()
        var1 = variance(var1_pred)
        mean_vec1.append(mean1)
        var_vec1.append(var1)
    return mean_vec1, var_vec1

def calc_distance(s1,s2):
    return(np.sqrt((s1[0] - s2[0])**2 + (s1[1] - s2[1])**2))

def get_nearest_data(s_train,s_test,r,k):
    dist_mat = np.zeros((len(s_test),len(s_train)))
    nearest_var = np.zeros((len(s_test)))
    for i in range(len(s_train)):
        for j in range(len(s_test)):
            dist_mat[j][i] = calc_distance(s_train[i],s_test[j])
    for i in range(len(s_test)):
        a = dist_mat[i]
        b = np.argpartition(a,k)[:k]
        y_val = []
        for index in b:
            y_val.append(r[index])
        nearest_var[i] = (np.mean(y_val))
    return nearest_var
#!/usr/bin/env python
# coding: utf-8

"""
Modified from Bivariate DeepKriging to 1D response
Column-index based access
"""

import os
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'

import pandas as pd
import keras
from keras.models import Model
from keras.layers import Dense, Input
import numpy as np
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import train_test_split
import sys
from python_libs import *


def main():

    mpiw_list = []
    picp_list = []
    mae_list  = []
    mse_list  = []

    data_name = ["AWU_RBF_LFT_2D", "TWIST_2D"]

    for sim in range(3):

        df = pd.read_csv(f"raw_datasets/{data_name[sim]}.csv")
        df_test1 = pd.read_csv("raw_datasets/test_locs2D.csv")

        # ===============================
        # Train / internal test split
        # ===============================
        df_train, df_test = train_test_split(df, train_size=0.9, random_state=42)

        # locations
        s_train = df_train.iloc[:, [0,1]].values
        s_test1 = df_test1.iloc[:, [0,1]].values
        s_test  = df_test.iloc[:, [0,1]].values

        # response (1D)
        y_train = df_train.iloc[:, 2].values.reshape(-1,1)
        # y_test1 = df_test1.iloc[:, 2].values.reshape(-1,1)
        y_test  = df_test.iloc[:, 2].values.reshape(-1,1)

        # # standardize response
        # y_mean = y_train.mean()
        # y_var  = y_train.var()

        # y_train = (y_train - y_mean) / np.sqrt(y_var)

        

        # ===============================
        # Basis construction
        # ===============================
        num_basis = [3**2, 7**2, 11**2]
        knots_1d = [np.linspace(0,1,int(np.sqrt(i))) for i in num_basis]

        def build_phi(s):
            N = s.shape[0]
            phi = np.zeros((N, sum(num_basis)))
            K = 0
            for res in range(len(num_basis)):
                theta = 2.5 / np.sqrt(num_basis[res])
                k1, k2 = np.meshgrid(knots_1d[res], knots_1d[res])
                knots = np.column_stack((k1.ravel(), k2.ravel()))
                for i in range(num_basis[res]):
                    d = np.linalg.norm(s - knots[i], axis=1) / theta
                    mask = (d >= 0) & (d <= 1)
                    phi[mask, i+K] = (1-d[mask])**6 * (35*d[mask]**2 + 18*d[mask] + 3)/3
                K += num_basis[res]
            return phi

        phi_train = build_phi(s_train)
        phi_test1 = build_phi(s_test1)
        phi_test  = build_phi(s_test)

        # ===============================
        # Split for ensemble / MSE
        # ===============================
        s_e, s_m, X_e, X_m, y_e, y_m = train_test_split(
            s_train, phi_train, y_train, test_size=0.1, random_state=42
        )

        # ===============================
        # Base model
        # ===============================
        inp = Input(shape=(phi_train.shape[1],))
        x = Dense(100, activation='relu')(inp)
        x = Dense(100, activation='relu')(x)
        x = Dense(100, activation='relu')(x)
        x = Dense(50, activation='relu')(x)
        x = Dense(50, activation='relu')(x)
        out = Dense(1, activation='linear')(x)

        base_model = Model(inp, out)
        base_model.compile(optimizer=keras.optimizers.Adam(0.001), loss='mae')

        base_model.fit(X_e, y_e, epochs=500, batch_size=128,
                       validation_split=0.1, verbose=0)

        feature_model = Model(inp, x)

        # ===============================
        # Ensemble
        # ===============================
        ensemble = fit_ensemble(20, X_e, y_e, feature_model)

        # ===============================
        # Nugget estimation
        # ===============================
        mean_vec, var_vec = predict_with_pi(ensemble, X_m)
        r = (y_m.flatten() - mean_vec)**2 - var_vec
        r[r < 0] = 0.0

        r_pred = get_nearest_data(s_m, s_test, r, 30)

        # ===============================
        # Prediction on external test
        # ===============================
        mean_vec, var_vec = predict_with_pi(ensemble, phi_test)
        mean_vec = np.array(mean_vec)
        var_vec = np.array(var_vec)
        
        
        sd_vec   = np.sqrt(var_vec + r_pred) 

        lower = mean_vec - 1.96 * sd_vec
        upper = mean_vec + 1.96 * sd_vec

        # metrics
        mae = np.mean(np.abs(y_test.flatten() - mean_vec))
        mse = np.mean((y_test.flatten() - mean_vec)**2)
        picp = np.mean((y_test.flatten() > lower) & (y_test.flatten() < upper))
        mpiw = np.mean(upper - lower)

        mae_list.append(mae)
        mse_list.append(mse)
        picp_list.append(picp)
        mpiw_list.append(mpiw)

        # ===============================
        # Save predictions for df_test1
        # ===============================
        mean_t1, var_t1 = predict_with_pi(ensemble, phi_test1)
        mean_t1 = np.array(mean_t1)
        var_t1 = np.array(var_t1)
        
        df_test1["prediction"] = mean_t1
        df_test1["se"] = np.sqrt(var_t1)
        df_test1.to_csv(f"raw_datasets/{data_name[sim]}_pred_DeepKriging.csv", index=False)

        print(f"Sim {sim+1}: MAE={mae:.4f}, MSE={mse:.4f}, PICP={picp:.4f}, MPIW={mpiw:.4f}")

    # ===============================
    # Save summary
    # ===============================
    df_out = pd.DataFrame({
        "MAE": mae_list,
        "MSE": mse_list,
        "PICP": picp_list,
        "MPIW": mpiw_list
    })

    df_out.to_csv("raw_datasets/DeepKriging_1D_results.csv", index=False)


if __name__ == "__main__":
    main()
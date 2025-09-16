#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Tuesday August  29 23:33:04 2023

@author: Pratik
"""

import model_specification
from GP_regression import GP_regression
# import time 
import json
import argparse, os
import numpy as np
import torch
import torch.nn as nn
import subprocess
from scipy.optimize import minimize
from scipy.optimize import Bounds
from scipy.optimize import SR1
from scipy import optimize
import pandas as pd



"""main"""
def main():
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    # preprocess datasets 
    # cwd = os.getcwd()
    # subprocess.run("Rscript "+cwd+"/r_scripts/data_preprocessing.R "+cwd,shell = True, executable="/bin/bash")
    # parse arguments
    print("=========================================================")
    print("Normalizing flow models for warping functions ... ")
    print("=========================================================")
    args = model_specification.parse_args()
    if args is None:
        exit()
    params = np.array([args.init_sigma_sq,args.init_range,args.init_nugget])
    
    np.random.seed(args.seed)
    torch.manual_seed(args.seed+10000)

    #fn = str(time.time()).replace('.','')
    fn = model_specification.args2fn(args)
    print(args)
    print('\nfilename: ', fn)

    print(" [*] Building model!")    
    if args.fn != '0':
        # overwrite
        # args.fn ends with ``_last'' or ``_best''
        old_fn = args.fn
        overwrite_args = True
        print('MANUALLY RESUMING')
    else:
        # automatic resuming the last model
        # (of the same args) if it exists
        old_fn = fn + '_best'
        overwrite_args = False
        print('AUTOMATICALLY RESUMING')
        
    old_args = args.save_dir+'/'+old_fn+'_args.txt'
    old_path = args.save_dir+'/'+old_fn
    if os.path.isfile(old_args):
        def without_keys(d, keys):
            return {x: d[x] for x in d if x not in keys}
        d = without_keys(json.loads(open(old_args,'r').read()),
                         ['to_train','epoch'])
        # args.__dict__.update(d)
        # if overwrite_args:
        #     fn = model_specification.args2fn(args)
        print(" New args:" )
        print(args)
        print('\nfilename: ', fn)
        mdl = model_specification.model(args, fn)
        print(" [*] Loading model!")
        mdl.resume(old_path)
    else:
        mdl = model_specification.model(args, fn)
    
    #launch the graph in a session
    iteration = 0 
    param_diff = 100000.0
    
    if args.to_train == 1:
        print(" [*] Training started!")
        while param_diff > 0.0001 and iteration < args.max_iter:
            
                
            mdl.train(args.epoch, torch.tensor(params).to(device))

            ## Second stage optimization -- GP regression
            test_locs = False
            F_warped_loc,Y,orig_loc = mdl.get_warping(mdl.Fdata_loader, test_locs)
            regress1 = GP_regression(F_warped_loc,Y)
        #     print(regress.log_likelihood(params0))
            # device = F_warped_loc.device
            dtype  = F_warped_loc.dtype
            params0 = torch.nn.Parameter(torch.tensor(params, dtype=dtype, device=device))

            optimizer = torch.optim.Adam([params0], lr=args.step_size_GP)  # or Adam(..., maximize=True) if torch>=2.0

            print(f" [*] Optimization started for GP regression with warped locs at iteration {iteration}!")
            for i in range(args.n_iter_GP):
                optimizer.zero_grad()

                # If your log_likelihood returns a value to MAXIMIZE, negate it:
                nll = regress1.log_likelihood(params0, args.dataset)   # <-- key fix (use nll for a minimizer)
                nll.backward(retain_graph=True)
                optimizer.step()

                with torch.no_grad():
                    # only enforce positivity; don't cap at 1.2 (too restrictive)
                    params0.clamp_(min=1e-6)

                if i % 50 == 0:
                    print(
                            f"{i}\tlogL={(-nll).item():.6f}\t"
                            f"param0={params0[0].detach().item():.4f}, "
                            f"{params0[1].detach().item():.4f}, "
                            f"{params0[2].detach().item():.4f}"
                        )

            print(" [*] Optimization finished for GP regression with warped locs!")

            estim_params = params0.detach().cpu().numpy()

            # Use absolute difference (or a norm) for the stopping rule
            param_diff = float(np.mean(np.abs(estim_params - params)))

            print("---- Parameter estimates for Matern covariance function with fixed smoothness for warped locations ----")
            ll_val = regress1.log_likelihood(torch.tensor(estim_params, dtype=dtype, device=device)).item()
            print('{}, \t{}, \t{}, \t{}'.format(estim_params[0], estim_params[1], estim_params[2], ll_val))

            params = estim_params
            iteration += 1
            args.epoch = 600
        print(" [*] Training finished!")
    else:
        pass
    
    old_fn = fn + '_best'
    old_path = args.save_dir+'/'+old_fn
    mdl.resume(old_path)
    #     warped_loc,y,x_tr = mdl.sample(mdl.train_loader)
        

    print(" [**] Valid: %.4f" % mdl.evaluate(mdl.valid_loader, params))
    print(" [**] Test: %.4f" % mdl.evaluate(mdl.test_loader, params))

    print(" [*] Testing finished!")
        
        
        
    
    test_locs = True
    Ft_warped_loc = mdl.get_warping(mdl.fd_test_loader, test_locs)
    Ft_warped_loc = Ft_warped_loc.detach().cpu().numpy()
    test_locs = False
    F_warped_loc,Y,_ = mdl.get_warping(mdl.Fdata_loader, test_locs)
    F_warped_loc = F_warped_loc.detach().cpu().numpy()
    Y = Y.detach().cpu().numpy()
    result_dir = args.result_dir + '_' + args.dataset


#### 1D case 
    # df1 = pd.DataFrame({'warped_loc': F_warped_loc[:, 0], 'y': Y[:, 0]})
    # df2 = pd.DataFrame({'test_loc': Ft_warped_loc[:,0]})
    if args.dataset == "AWU_RBF_2D" or args.dataset == "AWU_RBF_LFT_2D" or args.dataset == "TWIST_2D":
        #### 2D case 
        df1 = pd.DataFrame({
            's1': F_warped_loc[:, 0],
            's2': F_warped_loc[:, 1],
            'y': Y[:, 0]
        })
        df2 = pd.DataFrame({
            's1': Ft_warped_loc[:, 0],
            's2': Ft_warped_loc[:, 1]
        })
        df1.to_csv(result_dir+"/warped_train_data.csv",
                            index = False)
        df2.to_csv(result_dir+"/warped_test_data.csv",
                            index = False)
        np.save(result_dir+"/param_estimates_python.npy", params)
    elif args.dataset == "argo3D":
        ### 3D case 
        df1 = pd.DataFrame({
            's1': F_warped_loc[:, 0],
            's2': F_warped_loc[:, 1],
            's3': F_warped_loc[:, 2],
            'y': Y[:, 0]
        })
        df2 = pd.DataFrame({
                's1': Ft_warped_loc[:, 0],
                's2': Ft_warped_loc[:, 1],
                's3': Ft_warped_loc[:, 2]
            })
        if args.test_dataset_argo3D == "sinloc":
            df2.to_csv(result_dir+"/warped_test_data-sinLoc.csv",
                            index = False)
        else:
            df2.to_csv(result_dir+"/warped_test_data-sinPres.csv",
                            index = False)
            np.save(result_dir+"/param_estimates_python.npy", params)
            df1.to_csv(result_dir+"/warped_train_data.csv",
                                index = False)
        
        

    print("=========================================================")
    print("Warping functions are trained ... ")
    print("=========================================================")

if __name__ == '__main__':
    main()


    #### training argo command python main_script.py --dataset="argo3D" --batch_size=5000 --epoch=100
    #### training 2D data python main_script.py --dataset='AWU_RBF_LFT_2D' --lr=0.0001
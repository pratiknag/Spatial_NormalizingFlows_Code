#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Tuesday August  29 23:33:04 2023

@author: Pratik
"""

import model_validation
from GP_regression import GP_regression
import time 
import json
import argparse, os
import numpy as np
import torch
from scipy.optimize import minimize
from scipy.optimize import Bounds
from scipy.optimize import SR1
from scipy import optimize


"""main"""
def main():
    # parse arguments
    args = model_validation.parse_args()
    if args is None:
        exit()
#     params = np.array([0.3,0.2])
    np.random.seed(args.seed)
    torch.manual_seed(args.seed+10000)

    #fn = str(time.time()).replace('.','')
    fn = model_validation.args2fn(args)
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
        old_fn = fn + '_last'
        overwrite_args = False
        print('AUTOMATICALLY RESUMING')
        
    old_args = args.save_dir+'/'+old_fn+'_args.txt'
    old_path = args.save_dir+'/'+old_fn
    if os.path.isfile(old_args):
        def without_keys(d, keys):
            return {x: d[x] for x in d if x not in keys}
        d = without_keys(json.loads(open(old_args,'r').read()),
                         ['to_train','epoch'])
        args.__dict__.update(d)
        if overwrite_args:
            fn = model_validation.args2fn(args)
        print(" New args:" )
        print(args)
        print('\nfilename: ', fn)
        mdl = model_validation.model(args, fn)
        print(" [*] Loading model!")
        mdl.resume(old_path)
    else:
        mdl = model_validation.model(args, fn)
        print(" [*] New model!")
#     #launch the graph in a session
#     if args.to_train:
#         print(" [*] Training started!")
#         mdl.train(args.epoch, params)
#         print(" [*] Training finished!")

#     print(" [**] Valid: %.4f" % mdl.evaluate(mdl.valid_loader, params))
#     print(" [**] Test: %.4f" % mdl.evaluate(mdl.test_loader, params))

#     print(" [*] Testing finished!")
    
    ## Second stage optimization -- GP regression
    warped_loc,y,x_tr = mdl.sample(mdl.Fdata_loader)
    
    np.save("generated_warping.npy", warped_loc)
    np.save("original_loc.npy",x_tr)

if __name__ == '__main__':
    main()
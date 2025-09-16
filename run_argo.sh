#!/bin/bash

echo -e "\033[1;36m==============================================================\033[0m"
echo -e "\033[1;36m###   \033[1;33mRunning scripts for the argo dataset           \033[1;36m###\033[0m"
echo -e "\033[1;36m==============================================================\033[0m"
echo ""
echo -e "\033[1;32m✔ Upon successful completion, the \033[1;34mresults_argo3D\033[1;32m directory will be created in the home directory containing all the results.\033[0m"
echo -e "\033[1;36m==============================================================\033[0m"
Rscript r_scripts/data_preprocessing_argo_data.R 
python python_scripts/main_script.py  \
   --dataset='argo3D'  \
      --lr=0.00001   \
    --epoch=2000 \
    --num_ds_layers=1  \
    --n_iter_GP=2000 \
    --step_size_GP=0.001 \
    --init_sigma_sq=0.8  \
    --init_range=0.05 \
    --init_nugget=0.01  \
    --max_iter=1 \
    --flowtype='ddsf' \
    --num_flow_layers=5 \
    --num_hid_layers=3 \
    --cuda=True    \
    --to_train=1  \
    --test_dataset_argo3D="sinpress"
python python_scripts/main_script.py \
    --dataset='argo3D'  \
      --lr=0.00001   \
    --epoch=2000 \
    --num_ds_layers=1  \
    --n_iter_GP=2000 \
    --step_size_GP=0.001 \
    --init_sigma_sq=0.8  \
    --init_range=0.05 \
    --init_nugget=0.01  \
    --max_iter=1 \
    --flowtype='ddsf' \
    --num_flow_layers=5 \
    --num_hid_layers=3 \
    --cuda=True    \
    --to_train=0  \
    --test_dataset_argo3D="sinloc"

Rscript r_scripts/GKriging_argo.R 
Rscript r_scripts/generate_predictions_for_plotting_argo.R
Rscript r_scripts/plot_results_argo.R 

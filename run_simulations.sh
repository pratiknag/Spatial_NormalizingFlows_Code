#!/bin/bash

echo -e "\033[1;36m==============================================================\033[0m"
echo -e "\033[1;36m### \033[1;33mRunning simulation scripts for all datasets         \033[1;36m###\033[0m"
echo -e "\033[1;36m==============================================================\033[0m"
echo ""
echo -e "\033[1;32m✔ Upon successful completion, \033[1;34mresults_* \033[1;32mdirectories will be created in the home directory for each simulation dataset.\033[0m"
echo -e "\033[1;32m✔ Model-based estimates and corresponding plots will be saved inside the \033[1;34m'plots/'\033[1;32m subfolder of each results directory.\033[0m"
echo -e "\033[1;36m==============================================================\033[0m"

Rscript r_scripts/data_preprocessing.R
#Rscript simulation_Twist2D.R
source env/bin/activate
python python_scripts/main_script.py \
    --dataset='TWIST_2D' \
    --lr=0.0001 \
    --epoch=3000 \
    --n_iter_GP=3000 \
    --init_sigma_sq=0.8 \
    --init_range=0.05 \
    --init_nugget=0.01 \
    --max_iter=3 \
    --flowtype='ddsf' \
    --num_flow_layers=5 \
    --num_hid_layers=3 \
    --cuda=True \
    --to_train=1 \
    --step_size_GP=0.001
python python_scripts/main_script.py \
    --dataset='AWU_RBF_LFT_2D' \
    --lr=0.0001 \
    --epoch=3000 \
    --n_iter_GP=3000 \
    --init_sigma_sq=0.8 \
    --init_range=0.05 \
    --init_nugget=0.01 \
    --max_iter=2 \
    --flowtype='ddsf' \
    --num_flow_layers=5 \
    --num_hid_layers=5 \
    --cuda=True \
    --to_train=1 \
    --step_size_GP=0.001 \
    --num_ds_layers=1
python python_scripts/main_script.py \
    --dataset='AWU_RBF_2D' \
    --lr=0.0001 \
    --epoch=3000 \
    --n_iter_GP=3000 \
    --init_sigma_sq=0.5 \
    --init_range=0.05 \
    --init_nugget=0.01 \
    --max_iter=2 \
    --flowtype='ddsf' \
    --num_flow_layers=5 \
    --num_hid_layers=3 \
    --cuda=True \
    --to_train=1 \
    --step_size_GP=0.001
deactivate
Rscript r_scripts/GKriging.R
Rscript r_scripts/generate_predictions_for_plotting.R
Rscript r_scripts/deepspat.R
Rscript r_scripts/plot_results_simulations.R

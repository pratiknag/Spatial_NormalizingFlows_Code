# !/usr/bin/env Rscript

rm(list = ls())
# args = commandArgs(trailingOnly=TRUE)
# file_path = args[1]
# setwd(file_path)
# setwd("/home/praktik/Desktop/Spatial_norm_flows/")
library(geoR)
library(MASS)
library(fields)
library(ggplot2)
library(rjson)
library(viridis)
library(latex2exp)
source("r_scripts/GKriging_functions.R")

cat("===========================================================\n")
cat("Generating predictions for plotting \n")
cat("===========================================================\n")


### Generating prediction results for single locations 

file_path <- paste0("results_argo3D/warped_train_data.csv")
data_train <- read.csv(file_path, header = T)

# train_ind <- c(1:round(0.9*dim(data_train)[1], digits = 0))
# data_train <- data_train[train_ind,]

file_path <- paste0("results_argo3D/warped_test_data-sinLoc.csv")
data_test <- read.csv(file_path, header = T)

indmat.estim <- readRDS("results_argo3D/warped_param_estimates.rds")

pred <- pred.summary.matern(indmat.estim$par,data_train,data_test)
saveRDS(pred, "results_argo3D/prediction_single_coordinate.rds")

file_path <- paste0("results_argo3D/warped_test_data-sinPres.csv")
data_test <- read.csv(file_path, header = T)

pred <- pred.summary.matern(indmat.estim$par,data_train,data_test)
saveRDS(pred, "results_argo3D/prediction_single_pressure.rds")






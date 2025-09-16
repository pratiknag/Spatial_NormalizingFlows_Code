# !/usr/bin/env Rscript

rm(list = ls())

cat("===========================================================\n")
cat("Gaussian Kriging parameter estimation for different comparing models ....\n")
cat("===========================================================\n")
# args = commandArgs(trailingOnly=TRUE)
# file_path = args[1]
# setwd("/home/praktik/Desktop/Spatial_norm_flows/")
# setwd(file_path)
library(geoR)
library(MASS)
library(fields)
library(reticulate)
np <- import("numpy")
# library(ggplot2)
source("r_scripts/GKriging_functions.R")

####################################################################
############ results on AWU_RBF_LFT_2D #############################
####################################################################

cat("===========================================================\n")
cat("Running Models for AWU_RBF_LFT_2D \n")
cat("===========================================================\n")

file_path = paste0("raw_datasets/AWU_RBF_LFT_2D.csv")
data = read.csv(file_path, header = TRUE)
train_ind = c(1:round(0.9*dim(data)[1], digits = 0))
data_train = data[train_ind,]
data_train = data_train[sample(1:dim(data_train)[1], 500),]
data_test = data[-train_ind,]
init.ind = c(0.1, 0.5, 1, 0.1, 0.5, 1, 0)
node_points = list(n1 = c(-0.25, -0.25), n2 = c(0.25, 0.25))

cat("===========================================================\n")
cat("Starting nonstationary Mat\'ern parameter estimation for the model...\n")
cat("===========================================================\n")

indmat.estim = optim_indmat_loglik(init.ind, data_train, node_points)

cat("===========================================================\n")
cat("Parameter estimation completed successfully!\n")
cat("Estimated parameters are being saved.\n")
cat("===========================================================\n")

saveRDS(indmat.estim, "results_AWU_RBF_LFT_2D/nonstat_param_estimates.rds")
# data_train = data[train_ind,]
# pred = pred.summary(indmat.estim$par,data_train,data_test)

# mse = mean((data_test$y-pred$pred)^2)

# lower_bound = pred$pred - 1.96*sqrt(diag(pred$conditional_var))
# upper_bound = pred$pred + 1.96*sqrt(diag(pred$conditional_var))

# count_var = 0
# for(i in 1:dim(data_test)[1]){
#   if ((data_test$y[i] > lower_bound[i]) & 
#       (data_test$y[i] < upper_bound[i])) count_var = count_var + 1
# }

# picp = count_var/dim(data_test)[1]
# print(picp)

# width = mean(upper_bound - lower_bound)
# print(paste0("mse for predictions is ", mse))
# print(paste0("picp for pred interval is ", picp))
# print(paste0("avg. width of the pred interval is ", width))


# results on AWU_RBF_LFT_2D Mat\'ern warped

file_path = paste0("results_AWU_RBF_LFT_2D/warped_train_data.csv")
data_train = read.csv(file_path, header = T)
# train_ind = c(1:round(0.9*dim(data)[1], digits = 0))
# data_train = data[train_ind,]
# data_test = data[-train_ind,]
init.ind = np$load("results_AWU_RBF_LFT_2D/param_estimates_python.npy")
init.ind = log(init.ind)
init.ind = c(init.ind[2],init.ind[1],-1,init.ind[3])

cat("===========================================================\n")
cat("Starting stationary Mat\'ern parameter estimation for estimated warped locations...\n")
cat("===========================================================\n")

indmat.estim = optim_matern_loglik(init.ind, data_train)
cat("===========================================================\n")
cat("Parameter estimation completed successfully!\n")
cat("Estimated parameters are being saved.\n")
cat("===========================================================\n")

saveRDS(indmat.estim, "results_AWU_RBF_LFT_2D/warped_param_estimates.rds")
# indmat.estim = readRDS("results_AWU_RBF_LFT_2D/warped_param_estimates.rds")
# data_train = data[train_ind,]
# pred = pred.summary.matern(indmat.estim$par,data_train,data_test)
# mse = mean((data_test$y-pred$pred)^2)

# lower_bound = pred$pred - 1.96*sqrt(diag(pred$conditional_var))
# upper_bound = pred$pred + 1.96*sqrt(diag(pred$conditional_var))
# count_var = 0
# for(i in 1:dim(data_test)[1]){
#   if ((data_test$y[i] > lower_bound[i]) & 
#       (data_test$y[i] < upper_bound[i])) count_var = count_var + 1
# }

# picp = count_var/dim(data_test)[1]
# # print(picp)

# width = mean(upper_bound - lower_bound)
# print(paste0("mse for predictions is ", mse))
# print(paste0("picp for pred interval is ", picp))
# print(paste0("avg. width of the pred interval is ", width))


# results on AWU_RBF_LFT_2D Mat\'ern orig

file_path = paste0("raw_datasets/AWU_RBF_LFT_2D.csv")
data = read.csv(file_path, header = T)
train_ind = c(1:round(0.9*dim(data)[1], digits = 0))
data_train = data[train_ind,]
data_test = data[-train_ind,]
init.ind = c(-0.1,-0.1,-0.1,-2)
cat("===========================================================\n")
cat("Starting stationary Mat\'ern parameter estimation for original observed locations...\n")
cat("===========================================================\n")

indmat.estim = optim_matern_loglik(init.ind, data_train)
cat("===========================================================\n")
cat("Parameter estimation completed successfully!\n")
cat("Estimated parameters are being saved.\n")
cat("===========================================================\n")
saveRDS(indmat.estim, "results_AWU_RBF_LFT_2D/orig_param_estimates.rds")
# data_train = data[train_ind,]
# pred = pred.summary.matern(indmat.estim$par,data_train,data_test)

# mse = mean((data_test$y-pred$pred)^2)

# lower_bound = pred$pred - 1.96*sqrt(diag(pred$conditional_var))
# upper_bound = pred$pred + 1.96*sqrt(diag(pred$conditional_var))

# count_var = 0
# for(i in 1:dim(data_test)[1]){
#   if ((data_test$y[i] > lower_bound[i]) & 
#       (data_test$y[i] < upper_bound[i])) count_var = count_var + 1
# }

# picp = count_var/dim(data_test)[1]
# # print(picp)

# width = mean(upper_bound - lower_bound)
# print(paste0("mse for predictions is ", mse))
# print(paste0("picp for pred interval is ", picp))
# print(paste0("avg. width of the pred interval is ", width))

####################################################################
############ results on AWU_RBF_2D #################################
####################################################################

cat("===========================================================\n")
cat("Running Models for AWU_RBF_2D \n")
cat("===========================================================\n")

file_path = paste0("raw_datasets/AWU_RBF_2D.csv")
data = read.csv(file_path, header = T)
train_ind = c(1:round(0.9*dim(data)[1], digits = 0))
data_train = data[train_ind,]
data_train = data_train[sample(1:dim(data_train)[1],500),]
data_test = data[-train_ind,]
init.ind = c(0.1,0.5,1,0.1,0.5,1,0)
node_points = list(n1 = c(-0.25,-0.25), n2 = c(0.25,0.25))
cat("===========================================================\n")
cat("Starting nonstationary Mat\'ern parameter estimation for the model...\n")
cat("===========================================================\n")

indmat.estim = optim_indmat_loglik(init.ind, data_train, node_points)

cat("===========================================================\n")
cat("Parameter estimation completed successfully!\n")
cat("Estimated parameters are being saved.\n")
cat("===========================================================\n")

saveRDS(indmat.estim, "results_AWU_RBF_2D/nonstat_param_estimates.rds")
# data_train = data[train_ind,]
# pred = pred.summary(indmat.estim$par,data_train,data_test)

# mse = mean((data_test$y-pred$pred)^2)

# lower_bound = pred$pred - 1.96*sqrt(diag(pred$conditional_var))
# upper_bound = pred$pred + 1.96*sqrt(diag(pred$conditional_var))

# count_var = 0
# for(i in 1:dim(data_test)[1]){
#   if ((data_test$y[i] > lower_bound[i]) & 
#       (data_test$y[i] < upper_bound[i])) count_var = count_var + 1
# }

# picp = count_var/dim(data_test)[1]
# # print(picp)

# width = mean(upper_bound - lower_bound)
# print(paste0("mse for predictions is ", mse))
# print(paste0("picp for pred interval is ", picp))
# print(paste0("avg. width of the pred interval is ", width))


# results on AWU_RBF_2D Mat\'ern warped

file_path = paste0("results_AWU_RBF_2D/warped_train_data.csv")
data_train = read.csv(file_path, header = T)
# train_ind = c(1:round(0.9*dim(data)[1], digits = 0))
# data_train = data[train_ind,]
# data_test = data[-train_ind,]
init.ind = np$load("results_AWU_RBF_2D/param_estimates_python.npy")
init.ind = log(init.ind)
init.ind = c(init.ind[2],init.ind[1],-1,init.ind[3])
cat("===========================================================\n")
cat("Starting stationary Mat\'ern parameter estimation for estimated warped locations...\n")
cat("===========================================================\n")

indmat.estim = optim_matern_loglik(init.ind, data_train)
cat("===========================================================\n")
cat("Parameter estimation completed successfully!\n")
cat("Estimated parameters are being saved.\n")
cat("===========================================================\n")
saveRDS(indmat.estim, "results_AWU_RBF_2D/warped_param_estimates.rds")
# data_train = data[train_ind,]
# pred = pred.summary.matern(indmat.estim$par,data_train,data_test)

# mse = mean((data_test$y-pred$pred)^2)

# lower_bound = pred$pred - 1.96*sqrt(diag(pred$conditional_var))
# upper_bound = pred$pred + 1.96*sqrt(diag(pred$conditional_var))

# count_var = 0
# for(i in 1:dim(data_test)[1]){
#   if ((data_test$y[i] > lower_bound[i]) & 
#       (data_test$y[i] < upper_bound[i])) count_var = count_var + 1
# }

# picp = count_var/dim(data_test)[1]
# # print(picp)

# width = mean(upper_bound - lower_bound)
# print(paste0("mse for predictions is ", mse))
# print(paste0("picp for pred interval is ", picp))
# print(paste0("avg. width of the pred interval is ", width))

# results on AWU_RBF_2D Matern orig

file_path = paste0("raw_datasets/AWU_RBF_2D.csv")
data = read.csv(file_path, header = T)
train_ind = c(1:round(0.9*dim(data)[1], digits = 0))
data_train = data[train_ind,]
data_test = data[-train_ind,]
init.ind = c(-0.1,-0.1,-0.1,-2)
cat("===========================================================\n")
cat("Starting stationary Mat\'ern parameter estimation for original observed locations...\n")
cat("===========================================================\n")

indmat.estim = optim_matern_loglik(init.ind, data_train)
cat("===========================================================\n")
cat("Parameter estimation completed successfully!\n")
cat("Estimated parameters are being saved.\n")
cat("===========================================================\n")
saveRDS(indmat.estim, "results_AWU_RBF_2D/orig_param_estimates.rds")
# data_train = data[train_ind,]
# pred = pred.summary.matern(indmat.estim$par,data_train,data_test)

# mse = mean((data_test$y-pred$pred)^2)

# lower_bound = pred$pred - 1.96*sqrt(diag(pred$conditional_var))
# upper_bound = pred$pred + 1.96*sqrt(diag(pred$conditional_var))

# count_var = 0
# for(i in 1:dim(data_test)[1]){
#   if ((data_test$y[i] > lower_bound[i]) & 
#       (data_test$y[i] < upper_bound[i])) count_var = count_var + 1
# }

# picp = count_var/dim(data_test)[1]
# # print(picp)

# width = mean(upper_bound - lower_bound)
# print(paste0("mse for predictions is ", mse))
# print(paste0("picp for pred interval is ", picp))
# print(paste0("avg. width of the pred interval is ", width))


####################################################################
############ results on TWIST_2D ###################################
####################################################################

cat("===========================================================\n")
cat("Running Models for TWIST_2D \n")
cat("===========================================================\n")

file_path = paste0("raw_datasets/TWIST_2D.csv")
data = read.csv(file_path, header = T)
train_ind = c(1:round(0.9*dim(data)[1], digits = 0))
data_train = data[train_ind,]
data_train = data_train[sample(1:dim(data_train)[1],500),]
data_test = data[-train_ind,]
init.ind = c(0.1,0.5,1,0.1,0.5,1,0)
node_points = list(n1 = c(-0.25,-0.25), n2 = c(0.25,0.25))
cat("===========================================================\n")
cat("Starting nonstationary Mat\'ern parameter estimation for the model...\n")
cat("===========================================================\n")

indmat.estim = optim_indmat_loglik(init.ind, data_train, node_points)

cat("===========================================================\n")
cat("Parameter estimation completed successfully!\n")
cat("Estimated parameters are being saved.\n")
cat("===========================================================\n")

saveRDS(indmat.estim, "results_TWIST_2D/nonstat_param_estimates.rds")
# data_train = data[train_ind,]
# pred = pred.summary(indmat.estim$par,data_train,data_test)

# mse = mean((data_test$y-pred$pred)^2)

# lower_bound = pred$pred - 1.96*sqrt(diag(pred$conditional_var))
# upper_bound = pred$pred + 1.96*sqrt(diag(pred$conditional_var))

# count_var = 0
# for(i in 1:dim(data_test)[1]){
#   if ((data_test$y[i] > lower_bound[i]) & 
#       (data_test$y[i] < upper_bound[i])) count_var = count_var + 1
# }

# picp = count_var/dim(data_test)[1]
# # print(picp)

# width = mean(upper_bound - lower_bound)
# print(paste0("mse for predictions is ", mse))
# print(paste0("picp for pred interval is ", picp))
# print(paste0("avg. width of the pred interval is ", width))


# results on TWIST_2D Mat\'ern warped

file_path = paste0("results_TWIST_2D/warped_train_data.csv")
data_train = read.csv(file_path, header = T)
data_train = data_train[sample(1:dim(data_train)[1],2000),]
# train_ind = c(1:round(0.9*dim(data)[1], digits = 0))
# data_train = data[train_ind,]
# data_test = data[-train_ind,]
init.ind = np$load("results_TWIST_2D/param_estimates_python.npy")
init.ind = log(init.ind)
init.ind = c(init.ind[2],init.ind[1],0,init.ind[3])
# init.ind = c(-0.1,-0.1,-0.1,-2)
cat("===========================================================\n")
cat("Starting stationary Mat\'ern parameter estimation for estimated warped locations...\n")
cat("===========================================================\n")

indmat.estim = optim_matern_loglik(init.ind, data_train)
cat("===========================================================\n")
cat("Parameter estimation completed successfully!\n")
cat("Estimated parameters are being saved.\n")
cat("===========================================================\n")
saveRDS(indmat.estim, "results_TWIST_2D/warped_param_estimates.rds")
# data_train = data[train_ind,]
# pred = pred.summary.matern(indmat.estim$par,data_train,data_test)

# mse = mean((data_test$y-pred$pred)^2)

# lower_bound = pred$pred - 1.96*sqrt(diag(pred$conditional_var))
# upper_bound = pred$pred + 1.96*sqrt(diag(pred$conditional_var))

# count_var = 0
# for(i in 1:dim(data_test)[1]){
#   if ((data_test$y[i] > lower_bound[i]) & 
#       (data_test$y[i] < upper_bound[i])) count_var = count_var + 1
# }

# picp = count_var/dim(data_test)[1]
# # print(picp)

# width = mean(upper_bound - lower_bound)
# print(paste0("mse for predictions is ", mse))
# print(paste0("picp for pred interval is ", picp))
# print(paste0("avg. width of the pred interval is ", width))

# results on TWIST_2D Matern orig

file_path = paste0("raw_datasets/TWIST_2D.csv")
data = read.csv(file_path, header = T)
train_ind = c(1:round(0.9*dim(data)[1], digits = 0))
data_train = data[train_ind,]
data_train = data_train[sample(1:dim(data_train)[1],1000),]
data_test = data[-train_ind,]
init.ind = c(-0.1,-0.1,-0.1,-2)
cat("===========================================================\n")
cat("Starting stationary Mat\'ern parameter estimation for original observed locations...\n")
cat("===========================================================\n")

indmat.estim = optim_matern_loglik(init.ind, data_train)
cat("===========================================================\n")
cat("Parameter estimation completed successfully!\n")
cat("Estimated parameters are being saved.\n")
cat("===========================================================\n")
saveRDS(indmat.estim, "results_TWIST_2D/orig_param_estimates.rds")

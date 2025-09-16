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
cat("Generating predictions on test set for all comparing models\n")
cat("===========================================================\n")

################################################################
################ AWU_RBF_LFT_2D ################################
################################################################


### Generating prediction results for nonstationary Matern kernel

json_file <- "raw_datasets/AWU_RBF_LFT_2D.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))
s <- json_data$s
all_locs <- matrix(NA,nrow = length(s), ncol = 2)

for(i in 1:length(s)){
  all_locs[i,1] <- s[[i]][1]
  all_locs[i,2] <- s[[i]][2]
}

file_path <- paste0("raw_datasets/AWU_RBF_LFT_2D.csv")
data_train <- read.csv(file_path, header = T)

node_points <- list(n1 = c(-0.25,-0.25), n2 = c(0.25,0.25))
indmat.estim <- readRDS("results_AWU_RBF_LFT_2D/nonstat_param_estimates.rds")

pred <- pred.summary(indmat.estim$par,data_train,all_locs)
## saving the predictions for nonstat Matern
saveRDS(pred, "results_AWU_RBF_LFT_2D/nonstat_predictions.rds")

### Generating prediction results for exponential covariance with warped locations 

file_path <- paste0("results_AWU_RBF_LFT_2D/warped_train_data.csv")
data_train <- read.csv(file_path, header = T)

file_path <- paste0("results_AWU_RBF_LFT_2D/warped_test_data.csv")
data_test <- read.csv(file_path, header = T)


indmat.estim <- readRDS("results_AWU_RBF_LFT_2D/warped_param_estimates.rds")

pred <- pred.summary.matern(indmat.estim$par,data_train,data_test)
saveRDS(pred, "results_AWU_RBF_LFT_2D/warped-loc_predictions.rds")

### Generating prediction results for Mat\'ern covariance with original locations 

file_path <- paste0("raw_datasets/AWU_RBF_LFT_2D.csv")
data_train <- read.csv(file_path, header = T)

indmat.estim <- readRDS("results_AWU_RBF_LFT_2D/orig_param_estimates.rds")

pred = pred.summary.matern(indmat.estim$par,data_train,all_locs)
saveRDS(pred, "results_AWU_RBF_LFT_2D/orig-loc_predictions.rds") 

################################################################
################ AWU_RBF_2D ####################################
################################################################

### Generating prediction results for nonstationary Matern kernel

json_file <- "raw_datasets/AWU_RBF_2D.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))
s <- json_data$s
all_locs <- matrix(NA,nrow = length(s), ncol = 2)

for(i in 1:length(s)){
  all_locs[i,1] <- s[[i]][1]
  all_locs[i,2] <- s[[i]][2]
}

file_path <- paste0("raw_datasets/AWU_RBF_2D.csv")
data_train <- read.csv(file_path, header = T)

node_points <- list(n1 = c(-0.25,-0.25), n2 = c(0.25,0.25))
indmat.estim <- readRDS("results_AWU_RBF_2D/nonstat_param_estimates.rds")

pred <- pred.summary(indmat.estim$par,data_train,all_locs)
## saving the predictions for nonstat Matern 
saveRDS(pred, "results_AWU_RBF_2D/nonstat_predictions.rds") 


file_path <- paste0("results_AWU_RBF_2D/warped_train_data.csv")
data_train <- read.csv(file_path, header = T)

file_path <- paste0("results_AWU_RBF_2D/warped_test_data.csv")
data_test <- read.csv(file_path, header = T)

indmat.estim <- readRDS("results_AWU_RBF_2D/warped_param_estimates.rds")

pred <- pred.summary.matern(indmat.estim$par,data_train,data_test)
saveRDS(pred, "results_AWU_RBF_2D/warped-loc_predictions.rds") 

### Generating prediction results for Mat\'ern covariance with orig locations 

file_path <- paste0("raw_datasets/AWU_RBF_2D.csv")
data_train <- read.csv(file_path, header = T)

indmat.estim <- readRDS("results_AWU_RBF_2D/warped_param_estimates.rds")
pred = pred.summary.matern(indmat.estim$par,data_train,all_locs)
saveRDS(pred, "results_AWU_RBF_2D/orig-loc_predictions.rds") 




################################################################
################ TWIST_2D ######################################
################################################################

### Generating prediction results for nonstationary Matern kernel

json_file <- "raw_datasets/TWIST_2D.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))
s <- json_data$s
all_locs <- matrix(NA,nrow = length(s), ncol = 2)

for(i in 1:length(s)){
  all_locs[i,1] <- s[[i]][1]
  all_locs[i,2] <- s[[i]][2]
}

file_path <- paste0("raw_datasets/TWIST_2D.csv")
data_train <- read.csv(file_path, header = T)

node_points <- list(n1 = c(-0.25,-0.25), n2 = c(0.25,0.25))
indmat.estim <- readRDS("results_TWIST_2D/nonstat_param_estimates.rds")

pred <- pred.summary(indmat.estim$par,data_train,all_locs)
## saving the predictions for nonstat Matern 
saveRDS(pred, "results_TWIST_2D/nonstat_predictions.rds") 


file_path <- paste0("results_TWIST_2D/warped_train_data.csv")
data_train <- read.csv(file_path, header = T)

file_path <- paste0("results_TWIST_2D/warped_test_data.csv")
data_test <- read.csv(file_path, header = T)

indmat.estim <- readRDS("results_TWIST_2D/warped_param_estimates.rds")

pred <- pred.summary.matern(indmat.estim$par,data_train,data_test)
saveRDS(pred, "results_TWIST_2D/warped-loc_predictions.rds") 

### Generating prediction results for Mat\'ern covariance with orig locations 

file_path <- paste0("raw_datasets/TWIST_2D.csv")
data_train <- read.csv(file_path, header = T)

indmat.estim <- readRDS("results_TWIST_2D/warped_param_estimates.rds")
# indmat.estim <- c(-0.1,-0.1,-0.1,-2)
pred = pred.summary.matern(indmat.estim$par,data_train,all_locs)
saveRDS(pred, "results_TWIST_2D/orig-loc_predictions.rds") 

cat("===========================================================\n")
cat("Predictions are generated and stored in respective folders. \n")
cat("===========================================================\n")






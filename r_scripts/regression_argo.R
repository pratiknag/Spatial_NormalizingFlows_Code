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


file_path = paste0("raw_datasets/argo3D.csv")
data = read.csv(file_path, header = T)

###### ANOVA analysis to find trends ###########

full_model <- lm(mean_temp ~ lon * lat * mean_pres, data = data)

# ANOVA table
anova(full_model)

# Summary for coefficient-level significance
summary(full_model)

# Load car package for advanced ANOVA
library(car)

# Type II ANOVA
Anova(full_model, type = 2)

# Final model with only significant terms
final_model <- lm(mean_temp ~ lon + mean_pres + lat, data = data)

# Summary of final model
summary(final_model)

# ANOVA of final model
anova(final_model)
save(final_model, file = "models/model_regression_argo.RData")
# Predicted values from final model
data$mean_temp_hat <- predict(final_model)

# Residuals: observed - predicted
data$mean_temp_resi <- data$mean_temp - data$mean_temp_hat

data <- data[, !names(data) %in% c("mean_temp", "mean_temp_hat")]

write.csv(data, "raw_datasets/argo3D_resi.csv", row.names = FALSE)
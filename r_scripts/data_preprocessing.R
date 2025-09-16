#!/usr/bin/env Rscript

rm(list = ls())
library(rjson)

#######################################################################
##################### Simulation studies ##############################
#######################################################################


## 1d-simulated datasets preprocessing
# args = commandArgs(trailingOnly=TRUE)
# ## setp-1d
# path = args[1]#"/Users/nagp/Desktop/Andrew_spatialNorm_FLOWs/project/Spatial_NormalizingFlows/"
# setwd(path)
json_file = "raw_datasets/step1D.json"

json_data = fromJSON(paste(readLines(json_file), collapse=""))
df = data.frame(x=json_data$sobs, y = json_data$y)
write.csv(df, file="raw_datasets/step1D.csv",row.names = F)

## Monterrubio-1d
json_file = "raw_datasets/Monterrubio1D.json"
json_data = fromJSON(paste(readLines(json_file), collapse=""))

df = data.frame(x=json_data$sobs, y = json_data$y)
write.csv(df, file="raw_datasets/Monterrubio1D.csv", row.names = F)

test_locs = data.frame(x = seq(-0.5,0.5,length.out = 1000))
write.csv(test_locs, file="raw_datasets/test_locs1D.csv", row.names = F)
## 2d-simulated datasets preprocessing

## AWU_RBF_2D 

json_file = "raw_datasets/AWU_RBF_2D.json"
json_data = fromJSON(paste(readLines(json_file), collapse=""))

sobs = json_data$sobs
y = json_data$y
true_data = matrix(NA,nrow = length(sobs), ncol = 2)
for(i in 1:length(sobs)){
  true_data[i,1] = sobs[[i]][1]
  true_data[i,2] = sobs[[i]][2]
}
df = data.frame(s1 = true_data[,1], s2 = true_data[,2], y = y)
write.csv(df, file="raw_datasets/AWU_RBF_2D.csv", row.names = F)

s = json_data$s
swarped = json_data$swarped
warped_data = matrix(NA,nrow = length(swarped), ncol = 2)
true_data = matrix(NA,nrow = length(swarped), ncol = 2)
for(i in 1:length(swarped)){
  warped_data[i,1] = swarped[[i]][1]
  warped_data[i,2] = swarped[[i]][2]
  true_data[i,1] = s[[i]][1]
  true_data[i,2] = s[[i]][2]
}
d1 = data.frame(s1 = true_data[,1], s2 = true_data[,2])
write.csv(d1, file="raw_datasets/test_locs2D.csv", row.names = F)
## AWU_RBF_LFT_2D

json_file = "raw_datasets/AWU_RBF_LFT_2D.json"
json_data = fromJSON(paste(readLines(json_file), collapse=""))

sobs = json_data$sobs
y = json_data$y
true_data = matrix(NA,nrow = length(sobs), ncol = 2)
for(i in 1:length(sobs)){
  true_data[i,1] = sobs[[i]][1]
  true_data[i,2] = sobs[[i]][2]
}
df = data.frame(s1 = true_data[,1], s2 = true_data[,2], y = y)
write.csv(df, file="raw_datasets/AWU_RBF_LFT_2D.csv", row.names = F)

s = json_data$s
swarped = json_data$swarped
warped_data = matrix(NA,nrow = length(swarped), ncol = 2)
true_data = matrix(NA,nrow = length(swarped), ncol = 2)
for(i in 1:length(swarped)){
  warped_data[i,1] = swarped[[i]][1]
  warped_data[i,2] = swarped[[i]][2]
  true_data[i,1] = s[[i]][1]
  true_data[i,2] = s[[i]][2]
}
d1 = data.frame(s1 = true_data[,1], s2 = true_data[,2])
write.csv(d1, file="raw_datasets/test_locs2D.csv", row.names = F)

x = seq(-0.5,0.5, length.out = 80)
y = seq(-0.5,0.5, length.out = 80)

d1 = expand.grid(x = x, y = y)
write.csv(d1, file="raw_datasets/test_locs2D-6400.csv", row.names = F)


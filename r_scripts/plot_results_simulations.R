# !/usr/bin/env Rscript

rm(list = ls())
# args = commandArgs(trailingOnly=TRUE)
# file_path = args[1]
# setwd(file_path)
# setwd("/home/praktik/Desktop/Spatial_norm_flows/")
cat("===========================================================\n")
cat("                 Generating plots. \n")
cat("===========================================================\n")
library(geoR)
library(MASS)
library(fields)
library(ggplot2)
library(rjson)
library(viridis)
library(latex2exp)
library(dplyr)
source("r_scripts/GKriging_functions.R")

# results on AWU_RBF_LFT_2D
l_s <- 10
l_t <- 10
a_s <- 13
plot_saving_width <- 1.4
plot_saving_height <- 1.6
base_size <- 1
bar_width <- 4
bar_height <- 0.5
lg_spacing <- 4
lg_margin <- 2
####################################################################
###################### AWU_RBF_LFT_2D ##############################
####################################################################
cat("############################################################\n")
cat("                 AWU_RBF_LFT_2D  \n")
cat("############################################################\n")
if (!dir.exists("results_AWU_RBF_LFT_2D/plots")){
  dir.create("results_AWU_RBF_LFT_2D/plots")
}
### plotting the true spatial process

load("raw_datasets/AWU_RBF_LFT_2D.json_ML.rda")

print(head(df_pred))

mse = mean((df_pred$pred_mean - df_pred$y)^2)

# lower_bound = df_pred$pred_95l
# upper_bound = df_pred$pred_95u

lower_bound = df_pred$pred_mean - 1.96*sqrt(df_pred$pred_var)
upper_bound = df_pred$pred_mean + 1.96*sqrt(df_pred$pred_var)

count_var = 0
for(i in 1:dim(df_pred)[1]){
  if ((df_pred$y[i] > lower_bound[i]) & 
      (df_pred$y[i] < upper_bound[i])) count_var = count_var + 1
}

picp = count_var/dim(df_pred)[1]
# print(picp)

width = mean(upper_bound - lower_bound)
cat("===========================================================\n")
cat("                 Diagnostic for GP deepspat  \n")
cat("===========================================================\n")
print(paste0("mse for predictions is ", mse))
print(paste0("picp for pred interval is ", picp))
print(paste0("avg. width of the pred interval is ", width))

plot_data <- data.frame(x = c(df_pred$s1), 
                       y = c(df_pred$s2), 
                       Observations = c(df_pred$pred_mean))

obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/3
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval), digits = 0)
  breaks_manual <- c(-2, 2, 5)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "A", 
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing))),
                     breaks = breaks_manual) +
  labs( 
       fill = "Pred.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t), hjust = 1), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")
ggsave("results_AWU_RBF_LFT_2D/plots/pred_true_process.pdf", 
       plot = p1, width = plot_saving_width ,
       height = plot_saving_height, units = "in")

plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = c(sqrt(df_pred$pred_var)))
obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- (obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/3
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval), digits = 1)

p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "G", limits=c(min_point, breaks_manual[4]),
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1.0,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing)) ),
                     breaks = breaks_manual) +
  labs( 
       fill = "se.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t)), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")
ggsave("results_AWU_RBF_LFT_2D/plots/se_true_process.pdf", 
       plot = p1, width = plot_saving_width,
       height = plot_saving_height, units = "in")

### plotting nonstat spatial process 

pred <- readRDS("results_AWU_RBF_LFT_2D/nonstat_predictions.rds")

pred_mean <- pred$pred
pred_var <- diag(pred$conditional_var)
rm(pred)
mse = mean((pred_mean - df_pred$y)^2)

lower_bound = pred_mean - 1.96*sqrt(pred_var)
upper_bound = pred_mean + 1.96*sqrt(pred_var)

count_var = 0
for(i in 1:dim(df_pred)[1]){
  if ((df_pred$y[i] > lower_bound[i]) & 
      (df_pred$y[i] < upper_bound[i])) count_var = count_var + 1
}

picp = count_var/dim(df_pred)[1]
# print(picp)
cat("===========================================================\n")
cat("       Diagnostic for GP nonstationary Mat\'ern  \n")
cat("===========================================================\n")
width = mean(upper_bound - lower_bound)
print(paste0("mse for predictions is ", mse))
print(paste0("picp for pred interval is ", picp))
print(paste0("avg. width of the pred interval is ", width))

plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = c(pred_mean))
obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/3
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval), digits = 0)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "A", 
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1.0,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing)) ),
                     breaks = breaks_manual) +
  labs( 
       fill = "Pred.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t), hjust = 1), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")
ggsave("results_AWU_RBF_LFT_2D/plots/pred_nonstat_process.pdf", 
       plot = p1, width = plot_saving_width ,
       height = plot_saving_height, units = "in")

plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = sqrt(pred_var))
obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/3
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval), digits = 1)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "G", 
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1.0,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing)) ),
                     breaks = breaks_manual) +
  labs( 
       fill = "se.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t), hjust = 1), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")
ggsave("results_AWU_RBF_LFT_2D/plots/se_nonstat_process.pdf", 
       plot = p1, width = plot_saving_width,
       height = plot_saving_height, units = "in")


### plotting warped spatial process 

pred <- readRDS("results_AWU_RBF_LFT_2D/warped-loc_predictions.rds")

pred_mean <- pred$pred
pred_var <- diag(pred$conditional_var)
rm(pred)
mse = mean((pred_mean - df_pred$y)^2)

lower_bound = pred_mean - 1.96*sqrt(pred_var)
upper_bound = pred_mean + 1.96*sqrt(pred_var)

count_var = 0
for(i in 1:dim(df_pred)[1]){
  if ((df_pred$y[i] > lower_bound[i]) & 
      (df_pred$y[i] < upper_bound[i])) count_var = count_var + 1
}

picp = count_var/dim(df_pred)[1]
# print(picp)
cat("===========================================================\n")
cat("       Diagnostic for GP warped Mat\'ern  \n")
cat("===========================================================\n")
width = mean(upper_bound - lower_bound)
print(paste0("mse for predictions is ", mse))
print(paste0("picp for pred interval is ", picp))
print(paste0("avg. width of the pred interval is ", width))


plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = c(pred_mean))
obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/3
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval), digits = 0)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "A", 
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1.0,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing)) ),
                     breaks = breaks_manual) +
  labs( 
       fill = "Pred.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t), hjust = 1), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")
ggsave("results_AWU_RBF_LFT_2D/plots/pred_warped_process.pdf", 
       plot = p1, width = plot_saving_width ,
       height = plot_saving_height, units = "in")

plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = sqrt(pred_var))
obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- obs_range[1]
  interval <- (obs_range[2] - obs_range[1])/3
  breaks_manual <- round(c(min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval), digits = 1)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "G", limits=c(min_point, breaks_manual[4]),
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1.0,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing)) ),
                     breaks = breaks_manual) +
  labs( 
       fill = "se.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t)), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")
ggsave("results_AWU_RBF_LFT_2D/plots/se_warped_process.pdf", 
       plot = p1, width = plot_saving_width,
       height = plot_saving_height, units = "in")

### plotting orig spatial process 

pred <- readRDS("results_AWU_RBF_LFT_2D/orig-loc_predictions.rds")

pred_mean <- pred$pred
pred_var <- diag(pred$conditional_var)
rm(pred)

mse = mean((pred_mean - df_pred$y)^2)

lower_bound = pred_mean - 1.96*sqrt(pred_var)
upper_bound = pred_mean + 1.96*sqrt(pred_var)

count_var = 0
for(i in 1:dim(df_pred)[1]){
  if ((df_pred$y[i] > lower_bound[i]) & 
      (df_pred$y[i] < upper_bound[i])) count_var = count_var + 1
}

picp = count_var/dim(df_pred)[1]
# print(picp)
cat("===========================================================\n")
cat("       Diagnostic for GP Mat\'ern orig locs \n")
cat("===========================================================\n")
width = mean(upper_bound - lower_bound)
print(paste0("mse for predictions is ", mse))
print(paste0("picp for pred interval is ", picp))
print(paste0("avg. width of the pred interval is ", width))


plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = c(pred_mean))
obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/3
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval), digits = 0)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "A", 
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1.0,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing)) ),
                     breaks = breaks_manual) +
  labs( 
       fill = "Pred.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t), hjust = 1), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")
ggsave("results_AWU_RBF_LFT_2D/plots/pred_orig_process.pdf", 
       plot = p1, width = plot_saving_width ,
       height = plot_saving_height, units = "in")

plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = sqrt(pred_var))
obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/3
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval), digits = 1)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "G", 
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1.0,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing)) ),
                     breaks = breaks_manual) +
  labs( 
       fill = "se.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t), hjust = 1), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")
ggsave("results_AWU_RBF_LFT_2D/plots/se_orig_process.pdf", 
       plot = p1, width = plot_saving_width,
       height = plot_saving_height, units = "in")

plot_saving_width <- 2.5
plot_saving_height <- 2
base_size <- 1
bar_width <- 0.5
bar_height <- 3
plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = c(df_pred$y))
obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- (obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/4
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 4*interval), digits = 0)
  breaks_manual <- c(-2, 2, 5)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "A", 
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 5.5),
                     breaks = breaks_manual) +
  labs(x = "", 
       y = "", 
       fill = expression(Y[1])) +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1),
       legend.text=element_text(size=rel(l_t+1), hjust = 1), 
        legend.title = element_text(size=l_s),
        legend.margin = margin(l = 2, r = 2, t = 2, b = 2),
        axis.title=element_text(size=a_s), 
        axis.text=element_blank(),
        # add margin between titles and axes
    axis.title.x = element_text(margin = margin(t = 5)), # top margin
    axis.title.y = element_text(margin = margin(r = 5)))
ggsave("results_AWU_RBF_LFT_2D/plots/AWU_process.pdf", 
       plot = p1, width = plot_saving_width ,
       height = plot_saving_height, units = "in")

# ===========================================================
#                  Diagnostic for GP deepspat  
# ===========================================================
# [1] "mse for predictions is 0.00911962716548531"
# [1] "picp for pred interval is 0.931889030487207"
# [1] "avg. width of the pred interval is 0.468513238492237"
# ===========================================================
#        Diagnostic for GP nonstationary Mat'ern  
# ===========================================================
# [1] "mse for predictions is 0.0842095112961599"
# [1] "picp for pred interval is 0.851583178119792"
# [1] "avg. width of the pred interval is 1.68802008124007"
# ===========================================================
#        Diagnostic for GP warped Mat'ern  
# ===========================================================
# [1] "mse for predictions is 0.015299139550847"
# [1] "picp for pred interval is 0.924713263405548"
# [1] "avg. width of the pred interval is 0.487109969569822"
# ===========================================================
#        Diagnostic for GP Mat'ern orig locs 
# ===========================================================
# [1] "mse for predictions is 0.1525347620257868"
# [1] "picp for pred interval is 0.920596019998039"
# [1] "avg. width of the pred interval is 0.81496900788961"

####################################################################
###################### AWU_RBF_2D ##################################
####################################################################

# cat("############################################################\n")
# cat("                 AWU_RBF_2D  \n")
# cat("############################################################\n")

# if (!dir.exists("results_AWU_RBF_2D/plots")){
#   dir.create("results_AWU_RBF_2D/plots")
# }
# ### plotting the true spatial process

# load("raw_datasets/AWU_RBF_2D.json_ML.rda")

# mse = mean((df_pred$pred_mean - df_pred$y)^2)

# # lower_bound = df_pred$pred_95l
# # upper_bound = df_pred$pred_95u

# lower_bound = df_pred$pred_mean - 1.96*sqrt(df_pred$pred_var)
# upper_bound = df_pred$pred_mean + 1.96*sqrt(df_pred$pred_var)

# count_var = 0
# for(i in 1:dim(df_pred)[1]){
#   if ((df_pred$y[i] > lower_bound[i]) & 
#       (df_pred$y[i] < upper_bound[i])) count_var = count_var + 1
# }

# picp = count_var/dim(df_pred)[1]
# # print(picp)
# cat("===========================================================\n")
# cat("       Diagnostic for GP deepspat \n")
# cat("===========================================================\n")
# width = mean(upper_bound - lower_bound)
# print(paste0("mse for predictions is ", mse))
# print(paste0("picp for pred interval is ", picp))
# print(paste0("avg. width of the pred interval is ", width))


# plot_data <- data.frame(x = c(df_pred$s1), 
#                         y = c(df_pred$s2), 
#                         Observations = c(df_pred$pred_mean))
# obs_range <- range(plot_data$Observations, na.rm = TRUE)
# breaks_manual <- round(seq(obs_range[1], obs_range[2], length.out = 5))
# p1 <- ggplot(data = plot_data, 
#              aes(x = x, 
#                  y = y)) +
#   geom_raster(data = subset(plot_data, !is.na(Observations)), 
#               aes(fill=Observations)) +
#   scale_fill_viridis(option = "A",
#                      guide = guide_colorbar(barwidth = bar_width, 
#                                             barheight = bar_height,
#                                             title.vjust = 3.5),
#                      breaks = breaks_manual) +
#   labs(x = TeX("$s_1$"), 
#        y = TeX("$s_2$"), 
#        fill = "Pred.") +
#   theme_bw(base_size = base_size) + 
#   theme(legend.text=element_text(size=rel(l_t)), 
#         legend.title = element_text(size=l_s),
#         axis.title=element_text(size=a_s), 
#         axis.text=element_blank())
# ggsave("results_AWU_RBF_2D/plots/pred_true_process.pdf", 
#        plot = p1, width = plot_saving_width ,
#        height = plot_saving_height, units = "in")

# plot_data <- data.frame(x = c(df_pred$s1), 
#                         y = c(df_pred$s2), 
#                         Observations = c(sqrt(df_pred$pred_var)))
# p1 <- ggplot(data = plot_data, 
#              aes(x = x, 
#                  y = y)) +
#   geom_raster(data = subset(plot_data, !is.na(Observations)), 
#               aes(fill=Observations)) +
#   scale_fill_viridis(option = "G",
#                      guide = guide_colorbar(barwidth = bar_width, 
#                                             barheight = bar_height,
#                                             title.vjust = 3.5),
#                      breaks = pretty(range(plot_data$Observations, 
#                                            na.rm = TRUE), n=4)) +
#   labs(x = TeX("$s_1$"), 
#        y = TeX("$s_2$"), 
#        fill = "se.") +
#   theme_bw(base_size = base_size) + 
#   theme(legend.text=element_text(size=rel(l_t)), 
#         legend.title = element_text(size=l_s),
#         axis.title=element_text(size=a_s), 
#         axis.text=element_blank())
# ggsave("results_AWU_RBF_2D/plots/se_true_process.pdf", 
#        plot = p1, width = plot_saving_width,
#        height = plot_saving_height, units = "in")

# ### plotting nonstat spatial process 

# pred <- readRDS("results_AWU_RBF_2D/nonstat_predictions.rds")

# pred_mean <- pred$pred
# pred_var <- diag(pred$conditional_var)
# rm(pred)
# mse = mean((pred_mean - df_pred$y)^2)

# lower_bound = pred_mean - 1.96*sqrt(pred_var)
# upper_bound = pred_mean + 1.96*sqrt(pred_var)

# count_var = 0
# for(i in 1:dim(df_pred)[1]){
#   if ((df_pred$y[i] > lower_bound[i]) & 
#       (df_pred$y[i] < upper_bound[i])) count_var = count_var + 1
# }

# picp = count_var/dim(df_pred)[1]
# # print(picp)
# cat("===========================================================\n")
# cat("       Diagnostic for GP nonstationary Mat\'ern \n")
# cat("===========================================================\n")
# width = mean(upper_bound - lower_bound)
# print(paste0("mse for predictions is ", mse))
# print(paste0("picp for pred interval is ", picp))
# print(paste0("avg. width of the pred interval is ", width))

# plot_data <- data.frame(x = c(df_pred$s1), 
#                         y = c(df_pred$s2), 
#                         Observations = c(pred_mean))
# obs_range <- range(plot_data$Observations, na.rm = TRUE)
# breaks_manual <- round(seq(obs_range[1], obs_range[2], length.out = 5))
# p1 <- ggplot(data = plot_data, 
#              aes(x = x, 
#                  y = y)) +
#   geom_raster(data = subset(plot_data, !is.na(Observations)), 
#               aes(fill=Observations)) +
#   scale_fill_viridis(option = "A",
#                      guide = guide_colorbar(barwidth = bar_width, 
#                                             barheight = bar_height,
#                                             title.vjust = 3.5),
#                      breaks = breaks_manual) +
#   labs(x = TeX("$s_1$"), 
#        y = TeX("$s_2$"), 
#        fill = "Pred.") +
#   theme_bw(base_size = base_size) + 
#   theme(legend.text=element_text(size=rel(l_t)), 
#         legend.title = element_text(size=l_s),
#         axis.title=element_text(size=a_s), 
#         axis.text=element_blank())
# ggsave("results_AWU_RBF_2D/plots/pred_nonstat_process.pdf", 
#        plot = p1, width = plot_saving_width ,
#        height = plot_saving_height, units = "in")

# plot_data <- data.frame(x = c(df_pred$s1), 
#                         y = c(df_pred$s2), 
#                         Observations = sqrt(pred_var))
# p1 <- ggplot(data = plot_data, 
#              aes(x = x, 
#                  y = y)) +
#   geom_raster(data = subset(plot_data, !is.na(Observations)), 
#               aes(fill=Observations)) +
#   scale_fill_viridis(option = "G",
#                      guide = guide_colorbar(barwidth = bar_width, 
#                                             barheight = bar_height,
#                                             title.vjust = 3.5),
#                      breaks = pretty(range(plot_data$Observations, 
#                                            na.rm = TRUE), n=4)) +
#   labs(x = TeX("$s_1$"), 
#        y = TeX("$s_2$"), 
#        fill = "se.") +
#   theme_bw(base_size = base_size) + 
#   theme(legend.text=element_text(size=rel(l_t)), 
#         legend.title = element_text(size=l_s),
#         axis.title=element_text(size=a_s), 
#         axis.text=element_blank())
# ggsave("results_AWU_RBF_2D/plots/se_nonstat_process.pdf", 
#        plot = p1, width = plot_saving_width,
#        height = plot_saving_height, units = "in")


# ### plotting warped spatial process 

# pred <- readRDS("results_AWU_RBF_2D/warped-loc_predictions.rds")

# pred_mean <- pred$pred
# pred_var <- diag(pred$conditional_var)
# rm(pred)
# mse = mean((pred_mean - df_pred$y)^2)

# lower_bound = pred_mean - 1.96*sqrt(pred_var)
# upper_bound = pred_mean + 1.96*sqrt(pred_var)

# count_var = 0
# for(i in 1:dim(df_pred)[1]){
#   if ((df_pred$y[i] > lower_bound[i]) & 
#       (df_pred$y[i] < upper_bound[i])) count_var = count_var + 1
# }

# picp = count_var/dim(df_pred)[1]
# # print(picp)
# cat("===========================================================\n")
# cat("       Diagnostic for GP Mat\'ern warped locs \n")
# cat("===========================================================\n")
# width = mean(upper_bound - lower_bound)
# print(paste0("mse for predictions is ", mse))
# print(paste0("picp for pred interval is ", picp))
# print(paste0("avg. width of the pred interval is ", width))


# plot_data <- data.frame(x = c(df_pred$s1), 
#                         y = c(df_pred$s2), 
#                         Observations = c(pred_mean))
# obs_range <- range(plot_data$Observations, na.rm = TRUE)
# breaks_manual <- round(seq(obs_range[1], obs_range[2], length.out = 5))
# p1 <- ggplot(data = plot_data, 
#              aes(x = x, 
#                  y = y)) +
#   geom_raster(data = subset(plot_data, !is.na(Observations)), 
#               aes(fill=Observations)) +
#   scale_fill_viridis(option = "A",
#                      guide = guide_colorbar(barwidth = bar_width, 
#                                             barheight = bar_height,
#                                             title.vjust = 3.5),
#                      breaks = breaks_manual) +
#   labs(x = TeX("$s_1$"), 
#        y = TeX("$s_2$"), 
#        fill = "Pred.") +
#   theme_bw(base_size = base_size) + 
#   theme(legend.text=element_text(size=rel(l_t)), 
#         legend.title = element_text(size=l_s),
#         axis.title=element_text(size=a_s), 
#         axis.text=element_blank())
# ggsave("results_AWU_RBF_2D/plots/pred_warped_process.pdf", 
#        plot = p1, width = plot_saving_width ,
#        height = plot_saving_height, units = "in")

# plot_data <- data.frame(x = c(df_pred$s1), 
#                         y = c(df_pred$s2), 
#                         Observations = sqrt(pred_var))
# p1 <- ggplot(data = plot_data, 
#              aes(x = x, 
#                  y = y)) +
#   geom_raster(data = subset(plot_data, !is.na(Observations)), 
#               aes(fill=Observations)) +
#   scale_fill_viridis(option = "G",
#                      guide = guide_colorbar(barwidth = bar_width, 
#                                             barheight = bar_height,
#                                             title.vjust = 3.5),
#                      breaks = pretty(range(plot_data$Observations, 
#                                            na.rm = TRUE), n=4)) +
#   labs(x = TeX("$s_1$"), 
#        y = TeX("$s_2$"), 
#        fill = "se.") +
#   theme_bw(base_size = base_size) + 
#   theme(legend.text=element_text(size=rel(l_t)), 
#         legend.title = element_text(size=l_s),
#         axis.title=element_text(size=a_s), 
#         axis.text=element_blank())
# ggsave("results_AWU_RBF_2D/plots/se_warped_process.pdf", 
#        plot = p1, width = plot_saving_width,
#        height = plot_saving_height, units = "in")

# ### plotting orig spatial process 

# pred <- readRDS("results_AWU_RBF_2D/orig-loc_predictions.rds")

# pred_mean <- pred$pred
# pred_var <- diag(pred$conditional_var)
# rm(pred)
# mse = mean((pred_mean - df_pred$y)^2)
# print(mse)
# lower_bound = pred_mean - 1.96*sqrt(pred_var)
# upper_bound = pred_mean + 1.96*sqrt(pred_var)

# count_var = 0
# for(i in 1:dim(df_pred)[1]){
#   if ((df_pred$y[i] > lower_bound[i]) & 
#       (df_pred$y[i] < upper_bound[i])) count_var = count_var + 1
# }

# picp = count_var/dim(df_pred)[1]
# # print(picp)
# cat("===========================================================\n")
# cat("       Diagnostic for GP Mat\'ern orig locs \n")
# cat("===========================================================\n")
# width = mean(upper_bound - lower_bound)
# print(paste0("mse for predictions is ", mse))
# print(paste0("picp for pred interval is ", picp))
# print(paste0("avg. width of the pred interval is ", width))


# plot_data <- data.frame(x = c(df_pred$s1), 
#                         y = c(df_pred$s2), 
#                         Observations = c(pred_mean))
# obs_range <- range(plot_data$Observations, na.rm = TRUE)
# breaks_manual <- round(seq(obs_range[1], obs_range[2], length.out = 5))
# p1 <- ggplot(data = plot_data, 
#              aes(x = x, 
#                  y = y)) +
#   geom_raster(data = subset(plot_data, !is.na(Observations)), 
#               aes(fill=Observations)) +
#   scale_fill_viridis(option = "A",
#                      guide = guide_colorbar(barwidth = bar_width, 
#                                             barheight = bar_height,
#                                             title.vjust = 3.5),
#                      breaks = breaks_manual) +
#   labs(x = TeX("$s_1$"), 
#        y = TeX("$s_2$"), 
#        fill = "Pred.")+
#   theme_bw(base_size = base_size) + 
#   theme(legend.text=element_text(size=rel(l_t)), 
#         legend.title = element_text(size=l_s),
#         axis.title=element_text(size=a_s), 
#         axis.text=element_blank())
# ggsave("results_AWU_RBF_2D/plots/pred_orig_process.pdf", 
#        plot = p1, width = plot_saving_width ,
#        height = plot_saving_height, units = "in")

# plot_data <- data.frame(x = c(df_pred$s1), 
#                         y = c(df_pred$s2), 
#                         Observations = sqrt(pred_var))
# p1 <- ggplot(data = plot_data, 
#              aes(x = x, 
#                  y = y)) +
#   geom_raster(data = subset(plot_data, !is.na(Observations)), 
#               aes(fill=Observations)) +
#   scale_fill_viridis(option = "G",
#                      guide = guide_colorbar(barwidth = bar_width, 
#                                             barheight = bar_height,
#                                             title.vjust = 3.5),
#                      breaks = pretty(range(plot_data$Observations, 
#                                            na.rm = TRUE), n=4)) +
#   labs(x = TeX("$s_1$"), 
#        y = TeX("$s_2$"), 
#        fill = "se.") +
#   theme_bw(base_size = base_size) + 
#   theme(legend.text=element_text(size=rel(l_t)), 
#         legend.title = element_text(size=l_s),
#         axis.title=element_text(size=a_s), 
#         axis.text=element_blank())
# ggsave("results_AWU_RBF_2D/plots/se_orig_process.pdf", 
#        plot = p1, width = plot_saving_width,
#        height = plot_saving_height, units = "in")

# plot_data <- data.frame(x = c(df_pred$s1), 
#                         y = c(df_pred$s2), 
#                         Observations = c(df_pred$y))
# obs_range <- range(plot_data$Observations, na.rm = TRUE)
# breaks_manual <- round(seq(obs_range[1], obs_range[2], length.out = 5))
# p1 <- ggplot(data = plot_data, 
#              aes(x = x, 
#                  y = y)) +
#   geom_raster(data = subset(plot_data, !is.na(Observations)), 
#               aes(fill=Observations)) +
#   scale_fill_viridis(option = "A",
#                      guide = guide_colorbar(barwidth = bar_width, 
#                                             barheight = bar_height,
#                                             title.vjust = 3.5),
#                      breaks = breaks_manual) +
#   labs(x = TeX("$s_1$"), 
#        y = TeX("$s_2$"), 
#        fill = "Val.") +
#   theme_bw(base_size = base_size) + 
#   theme(legend.text=element_text(size=rel(l_t)), 
#         legend.title = element_text(size=l_s),
#         axis.title=element_text(size=a_s), 
#         axis.text=element_blank())
# ggsave("results_AWU_RBF_2D/plots/AWU_process.pdf", 
#        plot = p1, width = plot_saving_width ,
#        height = plot_saving_height, units = "in")

# ===========================================================
#        Diagnostic for GP deepspat 
# ===========================================================
# [1] "mse for predictions is 0.0191649358953712"
# [1] "picp for pred interval is 0.955131849818645"
# [1] "avg. width of the pred interval is 0.396833613216249"
# ===========================================================
#        Diagnostic for GP nonstationary Mat'ern 
# ===========================================================
# [1] "mse for predictions is 0.0978357948946095"
# [1] "picp for pred interval is 0.88530536221939"
# [1] "avg. width of the pred interval is 1.48366916080385"
# ===========================================================
#        Diagnostic for GP Mat'ern warped locs 
# ===========================================================
# [1] "mse for predictions is 0.0279785356983173"
# [1] "picp for pred interval is 0.946475835702382"
# [1] "avg. width of the pred interval is 0.425840411260475"
# [1] 0.9387468
# ===========================================================
#        Diagnostic for GP Mat'ern orig locs 
# ===========================================================
# [1] "mse for predictions is 0.138746771646803"
# [1] "picp for pred interval is 0.880955788648172"
# [1] "avg. width of the pred interval is 1.295737129754939"

####################################################################
###################### TWIST_2D ####################################
####################################################################
plot_saving_width <- 1.4
plot_saving_height <- 1.6
base_size <- 1
bar_width <- 4
bar_height <- 0.5
cat("############################################################\n")
cat("                 TWIST_2D  \n")
cat("############################################################\n")

if (!dir.exists("results_TWIST_2D/plots")){
  dir.create("results_TWIST_2D/plots")
}
### plotting the true spatial process

load("raw_datasets/TWIST_2D.json_ML.rda")
json_file <- "raw_datasets/TWIST_2D.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))
# df_pred$y <- json_data$f_true
print(head(df_pred))
######## diagnostic for GP_deepspat #######################
mse = mean((df_pred$pred_mean - df_pred$y)^2)

# lower_bound = df_pred$pred_95l
# upper_bound = df_pred$pred_95u

lower_bound = df_pred$pred_mean - 1.96*sqrt(df_pred$pred_var)
upper_bound = df_pred$pred_mean + 1.96*sqrt(df_pred$pred_var)

count_var = 0
for(i in 1:dim(df_pred)[1]){
  if ((df_pred$y[i] > lower_bound[i]) & 
      (df_pred$y[i] < upper_bound[i])) count_var = count_var + 1
}

picp = count_var/dim(df_pred)[1]
# print(picp)
cat("===========================================================\n")
cat("       Diagnostic for GP deepspat \n")
cat("===========================================================\n")
width = mean(upper_bound - lower_bound)
print(paste0("mse for predictions is ", mse))
print(paste0("picp for pred interval is ", picp))
print(paste0("avg. width of the pred interval is ", width))

plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = c(df_pred$pred_mean))
obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/3
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval), digits = 1)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "A", 
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1.0,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing)) ),
                     breaks = breaks_manual) +
  labs( 
       fill = "Pred.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t), hjust = 1), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")
ggsave("results_TWIST_2D/plots/pred_true_process.pdf", 
       plot = p1, width = plot_saving_width ,
       height = plot_saving_height, units = "in")

plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = c(sqrt(df_pred$pred_var)))
plot_data <- plot_data %>%
  mutate(Observations_capped = pmin(Observations, 0.1))

obs_range <- range(plot_data$Observations_capped, na.rm = TRUE)
min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/2
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval), digits = 2)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations_capped)), 
              aes(fill=Observations_capped)) +
  scale_fill_viridis(option = "G", 
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1.0,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing)) ),
                     breaks = breaks_manual) +
  labs( 
       fill = "se.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t)), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")
ggsave("results_TWIST_2D/plots/se_true_process.pdf", 
       plot = p1, width = plot_saving_width,
       height = plot_saving_height, units = "in")

### plotting nonstat spatial process 

pred <- readRDS("results_TWIST_2D/nonstat_predictions.rds")

pred_mean <- pred$pred
pred_var <- diag(pred$conditional_var)
rm(pred)
mse = mean((pred_mean - df_pred$y)^2)

lower_bound = pred_mean - 1.96*sqrt(pred_var)
upper_bound = pred_mean + 1.96*sqrt(pred_var)

count_var = 0
for(i in 1:dim(df_pred)[1]){
  if ((df_pred$y[i] > lower_bound[i]) & 
      (df_pred$y[i] < upper_bound[i])) count_var = count_var + 1
}

picp = count_var/dim(df_pred)[1]
# print(picp)
cat("===========================================================\n")
cat("       Diagnostic for GP nonstationary Mat\'ern  \n")
cat("===========================================================\n")
width = mean(upper_bound - lower_bound)
print(paste0("mse for predictions is ", mse))
print(paste0("picp for pred interval is ", picp))
print(paste0("avg. width of the pred interval is ", width))

plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = c(pred_mean))
obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/3
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval), digits = 1)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "A", 
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1.0,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing)) ),
                     breaks = breaks_manual) +
  labs( 
       fill = "Pred.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t), hjust = 1), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")
ggsave("results_TWIST_2D/plots/pred_nonstat_process.pdf", 
       plot = p1, width = plot_saving_width ,
       height = plot_saving_height, units = "in")

plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = sqrt(pred_var))
obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/2
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval), digits = 2)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "G", 
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1.0,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing)) ),
                     breaks = breaks_manual) +
  labs( 
       fill = "se.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t)), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")
ggsave("results_TWIST_2D/plots/se_nonstat_process.pdf", 
       plot = p1, width = plot_saving_width,
       height = plot_saving_height, units = "in")


### plotting warped spatial process 

pred <- readRDS("results_TWIST_2D/warped-loc_predictions.rds")

pred_mean <- pred$pred
pred_var <- diag(pred$conditional_var)
rm(pred)
mse = mean((pred_mean - df_pred$y)^2)

lower_bound = pred_mean - 1.96*sqrt(pred_var)
upper_bound = pred_mean + 1.96*sqrt(pred_var)

count_var = 0
for(i in 1:dim(df_pred)[1]){
  if ((df_pred$y[i] > lower_bound[i]) & 
      (df_pred$y[i] < upper_bound[i])) count_var = count_var + 1
}

picp = count_var/dim(df_pred)[1]
# print(picp)
cat("===========================================================\n")
cat("       Diagnostic for GP Mat\'ern warped locs \n")
cat("===========================================================\n")
width = mean(upper_bound - lower_bound)
print(paste0("mse for predictions is ", mse))
print(paste0("picp for pred interval is ", picp))
print(paste0("avg. width of the pred interval is ", width))


plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = c(pred_mean))
obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/3
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval), digits = 1)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "A", 
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1.0,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing)) ),
                     breaks = breaks_manual) +
  labs( 
       fill = "Pred.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t), hjust = 1), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")
ggsave("results_TWIST_2D/plots/pred_warped_process.pdf", 
       plot = p1, width = plot_saving_width ,
       height = plot_saving_height, units = "in")

plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = sqrt(pred_var))
plot_data <- plot_data %>%
  mutate(Observations_capped = pmin(Observations, 0.1))

obs_range <- range(plot_data$Observations_capped, na.rm = TRUE)
min_point <- obs_range[1]
  interval <- (obs_range[2] - obs_range[1])/2
  breaks_manual <- round(c(
                    min_point + interval,
                    min_point + 2*interval), digits = 2)
print(obs_range)
print(breaks_manual)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations_capped)), 
              aes(fill=Observations_capped)) +
  scale_fill_viridis(option = "G", limit = c(min_point,breaks_manual[3]),
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1.0,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing)) ),
                     breaks = breaks_manual) +
  labs( 
       fill = "se.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t)), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")

ggsave("results_TWIST_2D/plots/se_warped_process.pdf", 
       plot = p1, width = plot_saving_width,
       height = plot_saving_height, units = "in")

### plotting orig spatial process 

pred <- readRDS("results_TWIST_2D/orig-loc_predictions.rds")

pred_mean <- pred$pred
pred_var <- diag(pred$conditional_var)
rm(pred)

mse = mean((pred_mean - df_pred$y)^2)

lower_bound = pred_mean - 1.96*sqrt(pred_var)
upper_bound = pred_mean + 1.96*sqrt(pred_var)

count_var = 0
for(i in 1:dim(df_pred)[1]){
  if ((df_pred$y[i] > lower_bound[i]) & 
      (df_pred$y[i] < upper_bound[i])) count_var = count_var + 1
}

picp = count_var/dim(df_pred)[1]
# print(picp)
cat("===========================================================\n")
cat("       Diagnostic for GP Mat\'ern orig locs \n")
cat("===========================================================\n")
width = mean(upper_bound - lower_bound)
print(paste0("mse for predictions is ", mse))
print(paste0("picp for pred interval is ", picp))
print(paste0("avg. width of the pred interval is ", width))


plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = c(pred_mean))
obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/3
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval), digits = 1)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "A", 
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1.0,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing)) ),
                     breaks = breaks_manual) +
  labs( 
       fill = "Pred.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t), hjust = 1), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")
ggsave("results_TWIST_2D/plots/pred_orig_process.pdf", 
       plot = p1, width = plot_saving_width ,
       height = plot_saving_height, units = "in")

plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = sqrt(pred_var))
obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/2
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval), digits = 2)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "G", 
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 1.0,
                                            direction = "horizontal" ,
                                            label.theme = element_text(margin = margin(t = lg_spacing)) ),
                     breaks = breaks_manual) +
  labs( 
       fill = "se.") +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(
       legend.text=element_text(size=rel(l_t)), 
        legend.title = element_text(size=l_s),
        legend.spacing.y = unit(lg_spacing, "cm"),
        legend.margin = margin(l = lg_margin, r = lg_margin, t = lg_margin, b = lg_margin),
        axis.title=element_blank(), 
        axis.text=element_blank(),
       legend.position = "bottom")
ggsave("results_TWIST_2D/plots/se_orig_process.pdf", 
       plot = p1, width = plot_saving_width,
       height = plot_saving_height, units = "in")

plot_saving_width <- 2.5
plot_saving_height <- 2
base_size <- 1
bar_width <- 0.5
bar_height <- 3
plot_data <- data.frame(x = c(df_pred$s1), 
                        y = c(df_pred$s2), 
                        Observations = c(df_pred$y))
obs_range <- range(plot_data$Observations, na.rm = TRUE)
min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/3
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval), digits = 0)
p1 <- ggplot(data = plot_data, 
       aes(x = x, 
           y = y)) +
  geom_raster(data = subset(plot_data, !is.na(Observations)), 
              aes(fill=Observations)) +
  scale_fill_viridis(option = "A", 
                     guide = guide_colorbar(barwidth = bar_width, 
                                            barheight = bar_height,
                                            title.vjust = 5.5),
                     breaks = breaks_manual) +
  labs(x = "", 
       y = "", 
       fill = expression(Y[2])) +
  theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0)) +
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1),
       legend.text=element_text(size=rel(l_t+1), hjust = 1), 
        legend.title = element_text(size=l_s),
        legend.margin = margin(l = 2, r = 2, t = 2, b = 2),
        axis.title=element_text(size=a_s), 
        axis.text=element_blank(),
        # add margin between titles and axes
    axis.title.x = element_text(margin = margin(t = 5)), # top margin
    axis.title.y = element_text(margin = margin(r = 5)))
ggsave("results_TWIST_2D/plots/pred_twist_process.pdf", 
       plot = p1, width = plot_saving_width ,
       height = plot_saving_height, units = "in")

# ===========================================================
#        Diagnostic for GP deepspat 
# ===========================================================
# [1] "mse for predictions is 0.00686474155316794"
# [1] "picp for pred interval is 0.672189001078326"
# [1] "avg. width of the pred interval is 0.151115045692465"
# ===========================================================
#        Diagnostic for GP nonstationary Mat'ern  
# ===========================================================
# [1] "mse for predictions is 0.00358464617943334"
# [1] "picp for pred interval is 0.829624546613077"
# [1] "avg. width of the pred interval is 0.112540288940427"
# ===========================================================
#        Diagnostic for GP Mat'ern warped locs 
# ===========================================================
# [1] "mse for predictions is 0.0018403174560476"
# [1] "picp for pred interval is 0.949612783060484"
# [1] "avg. width of the pred interval is 0.131883280260016"
# ===========================================================
#        Diagnostic for GP Mat'ern orig locs 
# ===========================================================
# [1] "mse for predictions is 0.00386822171577391"
# [1] "picp for pred interval is 0.985589648073718"
# [1] "avg. width of the pred interval is 0.381180354544665"










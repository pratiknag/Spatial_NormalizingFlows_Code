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
source("r_scripts/GKriging_functions.R")

if (!dir.exists("results_argo3D/plots")){
  dir.create("results_argo3D/plots")
}
denormalize <- function(x, mn = 6.64, var = 4.11){
 return((x*var) + mn)
}

minmax_inverse <- function(x_scaled, orig_min, orig_max) {
  x_orig <- x_scaled * (orig_max - orig_min) + orig_min
  return(x_orig)
}

l_s <- 9
l_t <- 8
a_s <- 9
plot_saving_width <- 2
plot_saving_height <- 1.6
base_size <- 1.7
bar_width <- 0.5
bar_height <- 5

file_path = paste0("results_argo3D/warped_train_data.csv")
data = read.csv(file_path, header = T)
set.seed(12298)
data_train = data
file_path = paste0("results_argo3D/warped_test_data-sinLoc.csv")
data_test = read.csv(file_path, header = T)
indmat.estim = readRDS("results_argo3D/warped_param_estimates.rds")
pred = readRDS("results_argo3D/prediction_single_coordinate.rds")
test_obs = read.csv("raw_datasets/test_data_single-coordinate.csv", header = T)

data_test$pred = pred$pred
data_test$pred <- denormalize(data_test$pred)
data_test$yl = pred$pred - 1.96*sqrt(diag(pred$conditional_var))
data_test$yl <- denormalize(data_test$yl)
data_test$ym = pred$pred + 1.96*sqrt(diag(pred$conditional_var))
data_test$ym <- denormalize(data_test$ym)
groups <- split(1:nrow(test_obs), list(test_obs$lon, test_obs$lat))

# Remove empty groups
groups <- groups[lengths(groups) > 0]

# Now each element of 'groups' is a vector of indices
# Example: first 5 groups into separate arrays
index_loc1 <- groups[[1]]
index_loc2 <- groups[[2]]

### loc 1   -29.9930 61.8020

test_obs1 = test_obs[-c(index_loc1,index_loc2),]
test_obs1$mean_temp <- denormalize(test_obs1$mean_temp)
df2 = data.frame(matrix(nrow = 1010,ncol = 3))
colnames(df2) = c("Values","Status","Standardized.pressure")
df2$Values = c(test_obs1$mean_temp,data_test$pred[1:1000])
df2$Status = c(rep("Obs. (°C)",10),rep("Pred. (°C)",1000))
df2$Standardized.pressure = c(3000*test_obs1$mean_pres,seq(0,3000,length.out = 1000))

df2$yl = c(rep(NA,10),data_test$yl[1:1000])

df2$ym = c(rep(NA,10),data_test$ym[1:1000])

p1 <- ggplot(data = df2,
       aes(x = Standardized.pressure)) +
  geom_ribbon(data = subset(df2, !is.na(yl)),
              aes(ymin = yl, ymax = ym, fill = "interval"), alpha = 0.4) +
  geom_line(data = subset(df2, !is.na(yl)),
            aes(y = Values, color = Status), size = 1) +
  geom_point(data = subset(df2, is.na(yl)),
            aes(y = Values, color = Status), size = 1) +
  coord_flip()+
  scale_x_reverse()+
  # scale_x_date(breaks = scales::date_breaks('1 year'), 
  #              labels = scales::date_format('%b-%y')) +
  scale_color_manual(name = "",
                     values = c(
                       "Pred. (°C)" = "brown",
                       "Obs. (°C)" = "black")) + 
  scale_fill_manual(name = "",
                    values = c("95% interval" = "gray30")) +
  labs(x = "Pressure (dbar)", 
       y = "Vals.") +
  # coord_cartesian(ylim = c(1300, 2500)) +
  theme_bw(base_size = base_size) +
  theme(
    legend.text = element_text(size = l_t), 
    axis.title.x = element_blank(), 
    axis.text.x = element_text(size = a_s),        # Remove x-axis title
    # axis.text.x = element_blank(),         # Remove x-axis text
    axis.title.y = element_text(size = a_s), 
    axis.text.y = element_text(size = a_s), 
    legend.position = "bottom"
  )
ggsave("results_argo3D/plots/loc_1_pred.pdf", 
       plot = p1, width = plot_saving_width,
       height = plot_saving_height, units = "in")


## Loc2 -60.0520 40.4300



test_obs1 = test_obs[index_loc2,]
test_obs1$mean_temp <- denormalize(test_obs1$mean_temp)
df2 = data.frame(matrix(nrow = 1010,ncol = 3))
colnames(df2) = c("Values","Status","Standardized pressure")
df2$Values = c(test_obs1$mean_temp,data_test$pred[1001:2000])
df2$Status = c(rep("Obs. (°C)",10),rep("Pred. (°C)",1000))
df2$Standardized.pressure = c(3000*test_obs1$mean_pres,seq(0,3000,length.out = 1000))

df2$yl = c(rep(NA,10),data_test$yl[1001:2000])

df2$ym = c(rep(NA,10),data_test$ym[1001:2000])

p1 <- ggplot(data = df2,
             aes(x = Standardized.pressure)) +
  geom_ribbon(data = subset(df2, !is.na(yl)),
              aes(ymin = yl, ymax = ym, fill = "interval"), alpha = 0.4) +
  geom_line(data = subset(df2, !is.na(yl)),
            aes(y = Values, color = Status), size = 1) +
  geom_point(data = subset(df2, is.na(yl)),
             aes(y = Values, color = Status), size = 1) +
  coord_flip()+
  scale_x_reverse()+
  # scale_x_date(breaks = scales::date_breaks('1 year'), 
  #              labels = scales::date_format('%b-%y')) +
  scale_color_manual(name = "",
                     values = c(
                       "Pred. (°C)" = "brown",
                       "Obs. (°C)" = "black")) +
  scale_fill_manual(name = "",
                    values = c("95% interval" = "gray30")) +
  labs(x = "Pressure (dbar)", 
       y = "Vals.") +
  # coord_cartesian(ylim = c(1300, 2500)) +
  theme_bw(base_size = base_size) +
  theme(
    legend.text = element_text(size = l_t), 
    axis.title.x = element_blank(), 
    axis.text.x = element_text(size = a_s),
    axis.title.y = element_text(size = a_s), 
    axis.text.y = element_text(size = a_s), 
    legend.position = "bottom"
  )
ggsave("results_argo3D/plots/loc_2_pred.pdf", 
       plot = p1, width = plot_saving_width,
       height = plot_saving_height, units = "in")

## Loc3 -33.61000 50.00200



test_obs1 = test_obs[index_loc1,]
test_obs1$mean_temp <- denormalize(test_obs1$mean_temp)
df2 = data.frame(matrix(nrow = 1010,ncol = 3))
colnames(df2) = c("Values","Status","Standardized pressure")
df2$Values = c(test_obs1$mean_temp,data_test$pred[2001:3000])
df2$Status = c(rep("Obs. (°C)",10),rep("Pred. (°C)",1000))
df2$Standardized.pressure = c(3000*test_obs1$mean_pres,seq(0,3000,length.out = 1000))

df2$yl = c(rep(NA,10),data_test$yl[2001:3000])

df2$ym = c(rep(NA,10),data_test$ym[2001:3000])

p1 <- ggplot(data = df2,
             aes(x = Standardized.pressure)) +
  geom_ribbon(data = subset(df2, !is.na(yl)),
              aes(ymin = yl, ymax = ym, fill = "interval"), alpha = 0.4) +
  geom_line(data = subset(df2, !is.na(yl)),
            aes(y = Values, color = Status), size = 1) +
  geom_point(data = subset(df2, is.na(yl)),
             aes(y = Values, color = Status), size = 1) +
  coord_flip()+
  scale_x_reverse()+
  # scale_x_date(breaks = scales::date_breaks('1 year'), 
  #              labels = scales::date_format('%b-%y')) +
  scale_color_manual(name = "",
                     values = c(
                       "Pred. (°C)" = "brown",
                       "Obs. (°C)" = "black")) +
  scale_fill_manual(name = "",
                    values = c("95% interval" = "gray30")) +
  labs(x = "Pressure (dbar)", 
       y = "Vals.") +
  # coord_cartesian(ylim = c(1300, 2500)) +
  theme_bw(base_size = base_size) +
  theme(
    legend.text = element_text(size = l_t), 
    axis.title.x = element_blank(), 
    axis.text.x = element_text(size = a_s),
    axis.title.y = element_text(size = a_s), 
    axis.text.y = element_text(size = a_s), 
    legend.position = "bottom"
  )
ggsave("results_argo3D/plots/loc_3_pred.pdf", 
       plot = p1, width = plot_saving_width,
       height = plot_saving_height, units = "in")

#######################################################
### Interpolations locations on single pressure levels
#######################################################

plot_saving_width <- 2
plot_saving_height <- 1.6
base_size <- 1.3
bar_width <- 0.5
bar_height <- 5

file_path = paste0("results_argo3D/warped_train_data.csv")
data = read.csv(file_path, header = T)
data_train = data[sample(1:dim(data)[1],3000),]
file_path = paste0("results_argo3D/warped_test_data-sinPres.csv")
data_test = read.csv(file_path, header = T)
indmat.estim = readRDS("results_argo3D/warped_param_estimates.rds")
# pred = pred.summary.exp(indmat.estim$par,data_train,data_test)
# saveRDS(pred, "results_argo3D/prediction_single_pressure.rds")
pred = readRDS("results_argo3D/prediction_single_pressure.rds")
test_obs = read.csv("raw_datasets/test_locs_single-pres.csv", header = T)
test_obs$pred = pred$pred
test_obs$pred <- denormalize(test_obs$pred)
test_obs$std_error = sqrt(diag(pred$conditional_var))*4.11


## Pres level 0.1 0.5 0.9

name = c("pres1","pres2","pres3")
pres_level = c(0.1,0.5,0.9)

for(i in 1:3){
  df_pred <- subset(test_obs, pres == pres_level[i])
  df_pred$lon <- minmax_inverse(df_pred$lon, -65, -10)
  df_pred$lat <- minmax_inverse(df_pred$lat, 32, 66)
  obs_range <- range(df_pred$pred, na.rm = TRUE)
  print(obs_range)
  # breaks_manual <- round(seq(obs_range[1], obs_range[2], length.out = 4))
  min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/4
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval,
                    min_point + 4*interval), digits = 1)
  print(breaks_manual)
  ## Predictions
  p1 <- ggplot(data = df_pred, 
         aes(x = lon, 
             y = lat, 
             fill = pred)) +
    geom_raster() +
    scale_fill_viridis(option = "A",
                       guide = guide_colorbar(barwidth = bar_width, 
                                              barheight = bar_height,
                                              title.vjust = 3.5),
                       breaks = breaks_manual) +
    labs(
  x = "Longitude (degrees)",
  y = "Latitude (degrees)",
  fill = "Pred. (°C)"
)  +
    
    theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(-60, -40, -20) ) +
scale_y_continuous(expand = c(0, 0),
                   breaks = c(35, 50, 65)) +
    theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1),
       legend.text=element_text(size=rel(l_t), hjust = 1), 
          legend.title = element_text(size=l_s),
          # legend.spacing.y = unit(50,"cm"),
          axis.title=element_text(size=a_s), 
       axis.text=element_text(size=a_s),
          axis.title.x = element_text(margin = margin(t = 5)), # top margin
    axis.title.y = element_text(margin = margin(r = 5)))
  ggsave(paste0("results_argo3D/plots/interpolation_",name[i],".pdf"), 
         plot = p1, width = plot_saving_width,
         height = plot_saving_height, units = "in")
  ## Standard errors
  p1 <- ggplot(data = df_pred, 
         aes(x = lon, 
             y = lat, 
             fill = std_error)) +
    geom_raster() +
    scale_fill_viridis(option = "G",
                       guide = guide_colorbar(barwidth = bar_width, 
                                              barheight = bar_height,
                                              title.vjust = 3.5),
                       breaks = pretty(range(subset(test_obs, 
                                                    pres == pres_level[i])$std_error, 
                                             na.rm = TRUE), n=3)) +
    labs(
  x = "Longitude (degrees)",
  y = "Latitude (degrees)",
  fill = "se. (°C)"
)+
    
    theme_bw(base_size = base_size) + 
    scale_x_continuous(expand = c(0, 0),
                       breaks = c(-60, -40, -20) ) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = c(35, 50, 65)) +
    theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1),
       legend.text=element_text(size=rel(l_t), hjust = 1), 
          legend.title = element_text(size=l_s , margin = margin(r = 10)),
          axis.title=element_text(size=a_s), 
       axis.text=element_text(size=a_s),
          axis.title.x = element_text(margin = margin(t = 5)), # top margin
    axis.title.y = element_text(margin = margin(r = 5)))
  ggsave(paste0("results_argo3D/plots/std-error_",name[i],".pdf"), 
         plot = p1, width = plot_saving_width,
         height = plot_saving_height, units = "in")
}


########################################################
### Plotting spatial map ##############################
########################################################

plot_saving_width <- 5
plot_saving_height <- 3.5
base_size <- 6


## train-test data for single location
new_df = read.csv("raw_datasets/argo_data_subset.csv", header = T)
new_df = new_df[,c(1,2,3,4)]
unique.lonlat = unique(new_df[,c(1,2)])
min_lon = min(unique.lonlat$lon)
max_lon = max(unique.lonlat$lon)

min_lat = min(unique.lonlat$lat)
max_lat = max(unique.lonlat$lat)

#### choice of 4 points for testing 
excluded_locs = data.frame(lon = c(-60.05200,-29.99300,-33.61000 ),
                           lat = c(40.43000,61.80200,50.00200 ))

### plotting the points on map

world1 = map_data("world")
p1 <- ggplot() +
  geom_polygon(data = world1, aes(x=long, y=lat, group=group),
               colour="darkgrey",fill="grey", alpha=1) +
  coord_sf(xlim = c(min_lon,max_lon), ylim=c(min_lat,max_lat)) +
  geom_point(data=unique.lonlat, aes(x=lon, y=lat, colour="Observations"), 
             pch=20, size=2) +
  geom_point(data=excluded_locs, aes(x=lon, y=lat, colour="Test locations"), 
             pch=8, size=1.5, stroke = 1) +
  theme_bw(base_size = base_size)+
  labs(x = "Longitude (degrees)", 
       y = "Latitude (degrees)") +
  scale_color_manual(values = c("Observations" = "steelblue4","Test locations" = "red3")) +
  theme(legend.title = element_blank(), legend.position = "bottom", 
        legend.text=element_text(size=l_t),
        axis.title=element_text(size=a_s), 
        axis.text=element_text(size=a_s))
ggsave("results_argo3D/plots/observed_area.pdf", 
       plot = p1, width = plot_saving_width,
       height = plot_saving_height, units = "in")


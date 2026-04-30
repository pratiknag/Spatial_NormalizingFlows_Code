
###### Additional diagnostic plots for Argo data
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
library(grid)
library(maps)
library(dplyr)
library(ggplot2)
library(deldir)
library(dplyr)
library(patchwork)  # for combining plots

minmax_inverse <- function(x_scaled, orig_min, orig_max) {
  x_orig <- x_scaled * (orig_max - orig_min) + orig_min
  return(x_orig)
}


setwd("/home/praktik/Desktop/Spatial_NormalizingFlows")
df <- read.csv("results_argo3D/warped_test_data-sinPres_6400.csv", header = T)
df_orig <- read.csv("raw_datasets/meshgrid_argo_6400.csv", header = T)
new_df = read.csv("raw_datasets/argo_data_subset.csv", header = T)
new_df = new_df[,c(1,2,3,4)]
unique.lonlat = unique(new_df[,c(1,2)])
min_lon = min(unique.lonlat$lon)
max_lon = max(unique.lonlat$lon)

min_lat = min(unique.lonlat$lat)
max_lat = max(unique.lonlat$lat)

df_orig_1 <- df_orig
df1 <- df
df1$s1_ref <- df_orig_1$lon
df1$s2_ref <- df_orig_1$lat
df1$s1 <- minmax_inverse(df1$s1, min_lon +0.3, max_lon-0.3)
df1$s2 <- minmax_inverse(df1$s2, min_lat+0.3, max_lat-0.3)

df1$s1_ref <- minmax_inverse(df1$s1_ref, min_lon +0.3, max_lon-0.3)
df1$s2_ref <- minmax_inverse(df1$s2_ref, min_lat+0.3, max_lat-0.3)

df_plot <- df1 %>%
  sample_n(min(20000, nrow(df1)))

l_s <- 9
l_t <- 2
a_s <- 9
plot_saving_width <- 6.5
plot_saving_height <- 2.8
base_size <- 5.3
bar_width <- 0.5
bar_height <- 5


# --- Triangulation for Original space ---
tri_ref <- deldir(df_plot$s1_ref, df_plot$s2_ref)
edges_ref <- data.frame(
  x = tri_ref$delsgs$x1,
  y = tri_ref$delsgs$y1,
  xend = tri_ref$delsgs$x2,
  yend = tri_ref$delsgs$y2
)

# --- Triangulation for Warped space ---
tri_warp <- deldir(df_plot$s1, df_plot$s2)
edges_warp <- data.frame(
  x = tri_warp$delsgs$x1,
  y = tri_warp$delsgs$y1,
  xend = tri_warp$delsgs$x2,
  yend = tri_warp$delsgs$y2
)

# --- Original space plot ---
p1 <- ggplot() +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = c(-60, -40, -20)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(35, 50, 63)
  ) +
  # Triangulation edges
  geom_segment(data = edges_ref, aes(x = x, y = y, xend = xend, yend = yend), color = "#0a4c868b", alpha = 0.5) +
  # Points
  geom_point(data = df_plot, aes(x = s1_ref, y = s2_ref), color = "#0a4c868b", size = 0.3) +
  coord_sf(
    xlim = c(min_lon, max_lon),
    ylim = c(min_lat, max_lat),
    expand = FALSE) +
  theme_bw(base_size = base_size) +
  labs(title = "Original Space", x = "Longitude (degrees)", y = "Latitude (degrees)") + 
  theme(
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1),
    title = element_text(size = a_s),
    axis.title = element_text(size = a_s),
    axis.text = element_text(size = a_s)
  )

# --- Warped space plot ---
p2 <- ggplot() +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = c(-60, -40, -20)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(35, 50, 63)
  ) +
  # Triangulation edges
  geom_segment(data = edges_warp, aes(x = x, y = y, xend = xend, yend = yend), color = "#0a5a86", alpha = 0.5) +
  # Points
  geom_point(data = df_plot, aes(x = s1, y = s2), color = "#0a5a86", size = 0.3) +
  coord_sf(
    xlim = c(min_lon, max_lon),
    ylim = c(min_lat, max_lat),
    expand = FALSE) +
  theme_bw(base_size = base_size) +
  labs(title = "Warped Space", x = "", y = "") +
  theme(
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1),
    title = element_text(size = a_s),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = a_s),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
  )

# --- Combine plots side by side ---
combined_plot <- p1 + p2 + plot_layout(ncol = 2)

# --- Display ---
ggsave(
  "results_argo3D/plots/warping_surface_polygon.pdf",
  plot = combined_plot,
  width = plot_saving_width,
  height = plot_saving_height,
  units = "in"
)

#--------------------------------------------
# 2. Single combined plot
#--------------------------------------------

world1 <- map_data("world")

#--------------------------------------------
# 3. Combined plot
#--------------------------------------------

p <- ggplot() +
  
  # --- Land layer (background) ---
  geom_polygon(
    data = world1,
    aes(x = long, y = lat, group = group),
    fill = "grey85",
    colour = "grey60",
    linewidth = 0.2
  ) +
  
  # --- Deformation arrows ---
  geom_segment(
    data = df_plot,
    aes(x = s1_ref,
        y = s2_ref,
        xend = s1,
        yend = s2,
        color = s3),
    arrow = arrow(length = unit(0.12, "cm")),
    alpha = 0.9,
    linewidth = 0.4
  ) +
  
  scale_color_viridis_c(option = "viridis") +
  
  coord_sf(
    xlim = c(min_lon, max_lon),
    ylim = c(min_lat, max_lat)) +
  
  labs(
    x = "Longitude (degrees)",
    y = "Latitude (degrees)",
    color = "Pressure"
  ) +
  
  theme_bw(base_size = base_size) +
  
  scale_x_continuous(
    expand = c(0, 0),
    breaks = c(-60, -40, -20)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(35, 50, 63)
  ) +
  
  theme(
    legend.position = "right",
    legend.text = element_text(size = rel(l_t)),
    legend.title = element_text(size = l_s),
    axis.title = element_text(size = a_s),
    axis.text = element_text(size = a_s)
  )

ggsave(
  "results_argo3D/plots/warping_surface_colored_arrows.pdf",
  plot = p,
  width = plot_saving_width,
  height = plot_saving_height,
  units = "in"
)



### plotting the sea surface current 
l_s <- 9
l_t <- 2
a_s <- 9
plot_saving_width <- 3
plot_saving_height <- 2.0
base_size <- 5.3
bar_width <- 0.5
bar_height <- 5
world1 <- map_data("world")
bwr <- colorRampPalette(c("blue","white","red"))
color_array <- bwr(100)
df_current <- read.csv("df_current.csv", header = T)
df_current$lon <- df_current$lon - 360
df_current$vel <- sqrt(df_current$u^2 + df_current$v^2)
  obs_range <- range(df_current$vel, na.rm = TRUE)
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
  ### Plot
p1 <- ggplot() +
  
  # --- Land overlay ---
  geom_polygon(
    data = world1,
    aes(x = long, y = lat, group = group),
    fill = "grey85",
    colour = "grey50",
    linewidth = 0.2
  ) +
  # --- Raster layer ---
  geom_raster(
    data = df_current,
    aes(x = lon, y = lat, fill = vel)
  ) +
  coord_sf(
    xlim = c(min_lon, max_lon),
    ylim = c(min_lat, max_lat))+
  
  scale_fill_gradientn(
    colours = color_array,
    guide = guide_colorbar(
      barwidth = bar_width,
      barheight = bar_height,
      title.hjust = 0.5,
      title.vjust = 2.5
    ),
    breaks = breaks_manual
  ) +
  
  labs(
    x = "Longitude (degrees)",
    y = "Latitude (degrees)",
    fill = "Speed (m/s)"
  ) +
  
  theme_bw(base_size = base_size) +
  
  scale_x_continuous(
    expand = c(0, 0),
    breaks = c(-60, -40, -20)
  ) +
  
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(35, 50, 63)
  ) +
  
  theme(
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1),
    legend.text = element_text(size = rel(l_t)),
    legend.title = element_text(size = l_s),
    axis.title = element_text(size = a_s),
    axis.text = element_text(size = a_s),
    axis.title.x = element_text(margin = margin(t = 4)),
    axis.title.y = element_text(margin = margin(r = 4))
  )

ggsave(
  "results_argo3D/plots/ocean_current.pdf",
  plot = p1,
  width = plot_saving_width,
  height = plot_saving_height,
  units = "in"
)



### plotting argo data 
l_s <- 9
l_t <- 2
a_s <- 9
plot_saving_width <- 4
plot_saving_height <- 3.6
base_size <- 5.3
bar_width <- 0.5
bar_height <- 5
denormalize <- function(x, mn = 6.64, var = 4.11){
 return((x*var) + mn)
}

minmax_inverse <- function(x_scaled, orig_min, orig_max) {
  x_orig <- x_scaled * (orig_max - orig_min) + orig_min
  return(x_orig)
}
df_argo <- read.csv("raw_datasets/argo3D.csv", header = T)

library(ggplot2)


df_argo$pres_bin <- cut(
  df_argo$mean_pres,
  breaks = c(-Inf, 0.2, 0.5, Inf),
  labels = c("Surface", "Mid", "Deep"),
  include.lowest = TRUE
)
df_argo$mean_temp <- denormalize(df_argo$mean_temp)

df_argo$lon <- minmax_inverse(df_argo$lon, -65, -10)
df_argo$lat <- minmax_inverse(df_argo$lat, 32, 66)

obs_range <- range(df_argo$mean_temp, na.rm = TRUE)
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
p <- ggplot(df_argo,
            aes(lon, lat, color = mean_temp)) +
  geom_point(size = 2) +
  facet_wrap(~ pres_bin, nrow = 2) +
  scale_color_viridis_c(
    option = "A",
    guide = guide_colorbar(
      barwidth = bar_width,
      barheight = bar_height,
      title.vjust = 3.5
    ),
    breaks = breaks_manual
  ) +
  # scale_color_viridis_c() +
  # coord_equal() +
  labs(
    x = "Longitude (degrees)",
    y = "Latitude (degrees)",
    color = "Temp. (°C)",
  ) +
    
    theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(-60, -40, -20) ) +
scale_y_continuous(expand = c(0, 0),
                   breaks = c(35, 50, 65)) +
    theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1),
    strip.text = element_text(size = a_s),
       legend.text=element_text(size=rel(l_t), hjust = 1), 
          legend.title = element_text(size=l_s),
          axis.title=element_text(size=a_s), 
       axis.text=element_text(size=a_s),
          axis.title.x = element_text(size = a_s), # top margin
    axis.title.y = element_text(size = a_s))
  ggsave(paste0("results_argo3D/plots/argo_train_data.pdf"), 
         plot = p, width = plot_saving_width,
         height = plot_saving_height, units = "in")

##########################################################################
### Argo all plots on same graph 
##########################################################################


library(ggplot2)
library(dplyr)
library(patchwork)
library(viridis)

####################################################
## USER SIZE CONTROLS
####################################################
axis_title_size  <- 15
axis_text_size   <- 15
legend_title_size <- 16
legend_text_size  <- 15
strip_text_size  <- 12
base_size <- 5.3

bar_width <- 8
bar_height <- 0.5

x_breaks <- c(-60, -40, -20)
y_breaks <- c(35, 50, 63)

####################################################
## Helper functions
####################################################
denormalize <- function(x, mn = 6.64, var = 4.11){
  (x * var) + mn
}

minmax_inverse <- function(x_scaled, orig_min, orig_max) {
  x_scaled * (orig_max - orig_min) + orig_min
}

####################################################
## 1️⃣ OBSERVATIONS
####################################################
df_argo <- read.csv("raw_datasets/argo3D.csv")

df_argo$pres_bin <- cut(
  df_argo$mean_pres,
  breaks = c(-Inf, 0.2, 0.5, Inf),
  labels = c("Surface", "Mid", "Deep"),
  include.lowest = TRUE
)

df_argo$mean_temp <- denormalize(df_argo$mean_temp) + 3
df_argo$lon <- minmax_inverse(df_argo$lon, -65, -10)
df_argo$lat <- minmax_inverse(df_argo$lat, 32, 66)

####################################################
## 2️⃣ PREDICTIONS + SE
####################################################
pred <- readRDS("results_argo3D/prediction_single_pressure.rds")
test_obs <- read.csv("raw_datasets/test_locs_single-pres.csv")
load("models/model_regression_argo.RData")

new_data_clean <- test_obs %>%
  rename(mean_pres = pres) %>%
  select(lon, lat, mean_pres)

test_obs$pred <- predict(final_model, newdata = new_data_clean)
test_obs$pred <- denormalize(test_obs$pred + pred$pred) + 3
test_obs$std_error <- sqrt(diag(pred$conditional_var)) * 4.11

test_obs$lon <- minmax_inverse(test_obs$lon, -65, -10)
test_obs$lat <- minmax_inverse(test_obs$lat, 32, 66)

####################################################
## 3️⃣ SHARED TEMPERATURE SCALE
####################################################
df_sst <- read.csv("df_sst.csv", header = T)
# obs_range <- range(df_sst$analysed_sst, na.rm = TRUE)
# print(obs_range)
#   # breaks_manual <- round(seq(obs_range[1], obs_range[2], length.out = 4))
# min_point <- floor(obs_range[1])
# interval <- temp_range
# breaks_manual <- temp_breaks
# print(breaks_manual)
temp_range <- range(c(df_argo$mean_temp, test_obs$pred, df_sst$analysed_sst), na.rm = TRUE)
temp_breaks <- round(seq(floor(temp_range[1]), temp_range[2], length.out = 6), 0)

temp_scale <- scale_fill_viridis_c(
  option = "A",
  limits = temp_range,
  breaks = temp_breaks,
  guide = guide_colorbar(
    barwidth = bar_width,
    barheight = bar_height,
    title = "Temp. (°C)"
  )
)

####################################################
## COMMON THEMES
####################################################
theme_common <- theme_bw(base_size = base_size) +
  theme(
    strip.text = element_text(size = strip_text_size, face = "bold"),
    legend.title = element_text(size = legend_title_size),
    legend.text  = element_text(size = legend_text_size),
    legend.position = "bottom"
  )

theme_no_axes <- theme(
  axis.title = element_blank(),
  axis.text  = element_blank(),
  axis.ticks = element_blank()
)

theme_left_axis <- theme(
  axis.text.y  = element_text(size = axis_text_size),
  axis.ticks.y = element_line(),
  axis.text.x  = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title = element_blank()
)

theme_bottom_axis <- theme(
  axis.text.x  = element_text(size = axis_text_size),
  axis.ticks.x = element_line(),
  axis.text.y  = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title = element_blank()
)

theme_bottom_left_axis <- theme(
  axis.text.x  = element_text(size = axis_text_size),
  axis.ticks.x = element_line(),
  axis.text.y  = element_text(size = axis_text_size),
  axis.ticks.y = element_blank(),
  axis.title = element_blank()
)

####################################################
## 4️⃣ OBSERVATION PANELS (ROW 1)
####################################################
p_obs <- ggplot(df_argo, aes(lon, lat, fill = mean_temp)) +
  geom_point(shape = 21, size = 1.7, color = "black", stroke = 0.2) +
  facet_wrap(~ pres_bin, nrow = 1) +
  temp_scale +
  scale_x_continuous(expand = c(0,0), breaks = x_breaks) +
  scale_y_continuous(expand = c(0,0), breaks = y_breaks) +
  theme_common +
  theme_left_axis

####################################################
## 5️⃣ PREDICTION & SE PANELS
####################################################
pres_levels <- c(0.1, 0.5, 0.9)
pred_plots <- list()
se_plots <- list()

se_range <- range(test_obs$std_error, na.rm = TRUE)
se_breaks <- pretty(se_range, n = 3)

for (i in 1:3) {

  df <- subset(test_obs, pres == pres_levels[i])

  ## Predictions (middle row)
  pred_plots[[i]] <- ggplot(df, aes(lon, lat, fill = pred)) +
    geom_raster() +
    temp_scale +
    scale_x_continuous(expand = c(0,0), breaks = x_breaks) +
    scale_y_continuous(expand = c(0,0), breaks = y_breaks) +
    theme_common +
    theme_no_axes

  ## Standard errors (bottom row)
  se_plots[[i]] <- ggplot(df, aes(lon, lat, fill = std_error)) +
    geom_raster() +
    scale_fill_viridis_c(
      option = "G",
      limits = se_range,
      breaks = se_breaks,
      guide = guide_colorbar(
        barwidth = bar_width,
        barheight = bar_height,
        title = "se (°C)"
      )
    ) +
    scale_x_continuous(expand = c(0,0), breaks = x_breaks) +
    scale_y_continuous(expand = c(0,0), breaks = y_breaks) +
    theme_common +
    theme_no_axes
}

####################################################
## 6️⃣ ADD AXES ONLY ON BOUNDARIES
####################################################
pred_plots[[1]] <- pred_plots[[1]] + theme_left_axis
se_plots[[1]]   <- se_plots[[1]]   + theme_left_axis

se_plots[[1]] <- se_plots[[1]] + theme_bottom_left_axis
se_plots[[2]] <- se_plots[[2]] + theme_bottom_axis
se_plots[[3]] <- se_plots[[3]] + theme_bottom_axis

####################################################
## 7️⃣ FINAL LAYOUT WITH UNIVERSAL AXES
####################################################
final_plot <-
  p_obs /
  wrap_plots(pred_plots, nrow = 1) /
  wrap_plots(se_plots, nrow = 1) +
  plot_layout(guides = "collect")

final_plot <- final_plot & theme(
  legend.position = "top"
)
library(grid)
pdf(
  "results_argo3D/plots/argo_3x3_shared_axes.pdf",
  width = 11,
  height = 8
)

grid.newpage()

# Layout: 2 rows (plot + x label), 2 cols (y label + plot)
pushViewport(
  viewport(
    layout = grid.layout(
      nrow = 2, ncol = 2,
      heights = unit(c(0.90, 0.10), "npc"),
      widths  = unit(c(0.10, 0.90), "npc")
    )
  )
)

# Left axis title
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.text(
  "Latitude (degrees)",
  rot = 90,
  gp = gpar(fontsize = axis_title_size)
)
popViewport()

# Bottom axis title
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
grid.text(
  "Longitude (degrees)",
  gp = gpar(fontsize = axis_title_size)
)
popViewport()

# Main plot
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
print(final_plot, newpage = FALSE)
popViewport()

dev.off()

#####################################################################
############## Plotting Sea Surface Temp ############################
#####################################################################

### numerical model SST plot
l_s <- 9
l_t <- 2
a_s <- 9
plot_saving_width <- 3
plot_saving_height <- 2.0
base_size <- 5.3
bar_width <- 0.5
bar_height <- 5
world1 <- map_data("world")

  ## Predictions
  
  p1 <- ggplot() +
  
  # --- Land overlay ---
  geom_polygon(
    data = world1,
    aes(x = long, y = lat, group = group),
    fill = "grey85",
    colour = "grey50",
    linewidth = 0.2
  ) +
  geom_raster(
    data = df_sst,
    aes(x = lon, y = lat, fill = analysed_sst)
  ) +
  coord_sf(
    xlim = c(min_lon, max_lon),
    ylim = c(min_lat, max_lat))+
    scale_fill_viridis(option = "A",
                       guide = guide_colorbar(barwidth = bar_width, 
                                              barheight = bar_height,
                                              title.vjust = 3.5),
                       breaks = temp_breaks) +
    labs(
  x = "Longitude (degrees)",
  y = "Latitude (degrees)",
  fill = "Temp.(°C)"
)  +
    
    theme_bw(base_size = base_size) + 
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(-60, -40, -20) ) +
scale_y_continuous(expand = c(0, 0),
                   breaks = c(35, 50, 63)) +
    theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1),
       legend.text=element_text(size=rel(l_t), hjust = 1), 
          legend.title = element_text(size=l_s),
          # legend.spacing.y = unit(50,"cm"),
          axis.title=element_text(size=a_s), 
       axis.text=element_text(size=a_s),
          axis.title.x = element_text(margin = margin(t = 5)), # top margin
    axis.title.y = element_text(margin = margin(r = 5)))
  ggsave(paste0("results_argo3D/plots/numerical_sst.pdf"), 
         plot = p1, width = plot_saving_width,
         height = plot_saving_height, units = "in")




####################################################
## 1️⃣ Libraries
####################################################
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(gstat)
library(sp)
library(geoR)
####################################################
## 2️⃣ Parameters
####################################################
l_s <- 9
l_t <- 2
a_s <- 9
plot_saving_width <- 3
plot_saving_height <- 1.5
base_size <- 5

####################################################
## 3️⃣ Load and clean data
####################################################
df_argo <- read.csv("raw_datasets/argo3D_resi.csv", header = TRUE)

# Remove missing values (important)
df_argo <- df_argo %>%
  filter(!is.na(lon), !is.na(lat), !is.na(mean_pres), !is.na(mean_temp_resi))

##########################################################
## Variogram with geoR ##################################
##########################################################

compute_local_variogram <- function(region_data) {
  
  # Guard: need enough points
  if (nrow(region_data) < 5) return(NULL)
  
  # Build geoR geodata object
  geo_obj <- list(
    coords = as.matrix(region_data[, c("lon", "lat")]),
    data   = region_data$mean_temp_resi
  )
  
  # Compute variogram
  vg <- tryCatch(
    variog(
      geodata = geo_obj,
      # uvec = seq(0, max(dist(geo_obj$coords))/2, length.out = 15), # lag bins
      estimator.type = "classical",
      messages = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(vg)) return(NULL)
  
  # Convert to data.frame similar to gstat output
  vg_df <- data.frame(
    dist  = vg$u,
    gamma = vg$v,
    np    = vg$n
  )
  
  return(vg_df)
}

##############################################################################
## 4️⃣ Create spatial regions 4 (robust)
##############################################################################

lon_mid <- median(df_argo$lon, na.rm = TRUE)
lat_mid <- median(df_argo$lat, na.rm = TRUE)

df_argo <- df_argo %>%
  mutate(
    region = case_when(
      lat >= lat_mid & lon < lon_mid  ~ "NW",
      lat >= lat_mid & lon >= lon_mid ~ "NE",
      lat < lat_mid  & lon < lon_mid  ~ "SW",
      lat < lat_mid  & lon >= lon_mid ~ "SE",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(region))

df_argo$pres_bin <- cut(
  df_argo$mean_pres,
  breaks = c(-Inf, 0.2, 0.5, Inf),
  labels = c("Surface", "Mid", "Deep"),
  include.lowest = TRUE
)
## surface 
df_argo1 <- df_argo %>%
  filter(pres_bin == "Surface")
# df_argo <- df_argo %>%
#   mutate(mean_temp_resi = as.numeric(scale(mean_temp_resi)))

# df_argo1 <- df_argo1 %>%
#   group_by(region) %>%
#   mutate(mean_temp_resi = as.numeric(scale(mean_temp_resi))) %>%
#   ungroup()

# Apply across regions
regions <- unique(df_argo1$region)

variogram_all <- lapply(regions, function(reg) {
  
  region_data <- df_argo1 %>% filter(region == reg)
  
  vg <- compute_local_variogram(region_data)
  
  if (!is.null(vg)) {
    vg$region <- reg
    return(vg)
  }
})

# Combine into one data frame
variogram_all <- bind_rows(variogram_all)

####################################################
## 7️⃣ Plot
####################################################
p <- ggplot(variogram_all,
            aes(x = dist,
                y = gamma,
                color = region)) +
  geom_point(size = 1.8, alpha = 0.9) +
  geom_line(linewidth = 0.6) +
  labs(
    title = "Surface",
    x = "Lag distance",
    y = "Semivariance",
    color = "Sub-region"
  ) +
  theme_bw(base_size = base_size) +
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1),
    plot.title = element_text(size = a_s),
    axis.title = element_text(size = a_s),
    legend.position = "right",
       legend.text=element_text(size=rel(2), hjust = 1), 
          legend.title = element_text(size=a_s),
       axis.text=element_text(size=a_s)) 

####################################################
## 8️⃣ Save
####################################################
ggsave(
  "results_argo3D/plots/argo_variogram_surface.pdf",
  plot = p,
  width = plot_saving_width,
  height = plot_saving_height,
  units = "in"
)

## mid
df_argo1 <- df_argo %>%
  filter(pres_bin == "Mid")
# df_argo <- df_argo %>%
#   mutate(mean_temp_resi = as.numeric(scale(mean_temp_resi)))

# df_argo1 <- df_argo1 %>%
#   group_by(region) %>%
#   mutate(mean_temp_resi = as.numeric(scale(mean_temp_resi))) %>%
#   ungroup()

# Apply across regions
regions <- unique(df_argo1$region)

variogram_all <- lapply(regions, function(reg) {
  
  region_data <- df_argo1 %>% filter(region == reg)
  
  vg <- compute_local_variogram(region_data)
  
  if (!is.null(vg)) {
    vg$region <- reg
    return(vg)
  }
})

# Combine into one data frame
variogram_all <- bind_rows(variogram_all)

####################################################
## 7️⃣ Plot
####################################################
p <- ggplot(variogram_all,
            aes(x = dist,
                y = gamma,
                color = region)) +
  geom_point(size = 1.8, alpha = 0.9) +
  geom_line(linewidth = 0.6) +
  labs(
    title = "Mid",
    x = "Lag distance",
    y = "Semivariance",
    color = "Sub-region"
  ) +
  theme_bw(base_size = base_size) +
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1),
    plot.title = element_text(size = a_s),
    axis.title = element_text(size = a_s),
    legend.position = "right",
       legend.text=element_text(size=rel(2), hjust = 1), 
          legend.title = element_text(size=a_s),
       axis.text=element_text(size=a_s)) 

####################################################
## 8️⃣ Save
####################################################
ggsave(
  "results_argo3D/plots/argo_variogram_mid.pdf",
  plot = p,
  width = plot_saving_width,
  height = plot_saving_height,
  units = "in"
)

## deep 
df_argo1 <- df_argo %>%
  filter(pres_bin == "Deep")
# df_argo <- df_argo %>%
#   mutate(mean_temp_resi = as.numeric(scale(mean_temp_resi)))

# df_argo1 <- df_argo1 %>%
#   group_by(region) %>%
#   mutate(mean_temp_resi = as.numeric(scale(mean_temp_resi))) %>%
#   ungroup()

# Apply across regions
regions <- unique(df_argo1$region)

variogram_all <- lapply(regions, function(reg) {
  
  region_data <- df_argo1 %>% filter(region == reg)
  
  vg <- compute_local_variogram(region_data)
  
  if (!is.null(vg)) {
    vg$region <- reg
    return(vg)
  }
})

# Combine into one data frame
variogram_all <- bind_rows(variogram_all)

####################################################
## 7️⃣ Plot
####################################################
p <- ggplot(variogram_all,
            aes(x = dist,
                y = gamma,
                color = region)) +
  geom_point(size = 1.8, alpha = 0.9) +
  geom_line(linewidth = 0.6) +
  labs(
    title = "Deep",
    x = "Lag distance",
    y = "Semivariance",
    color = "Sub-region"
  ) +
  theme_bw(base_size = base_size) +
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1),
    plot.title = element_text(size = a_s),
    axis.title = element_text(size = a_s),
    legend.position = "right",
       legend.text=element_text(size=rel(2), hjust = 1), 
          legend.title = element_text(size=a_s),
       axis.text=element_text(size=a_s)) 

####################################################
## 8️⃣ Save
####################################################
ggsave(
  "results_argo3D/plots/argo_variogram_deep.pdf",
  plot = p,
  width = plot_saving_width,
  height = plot_saving_height,
  units = "in"
)




########################################################
#### Sample covariance based on model 
########################################################
l_s <- 9
l_t <- 7
a_s <- 9
plot_saving_width <- 2.7
plot_saving_height <- 1.3
base_size <- 1.7
bar_width <- 0.5
bar_height <- 3
# Read prediction object
pred <- readRDS("results_argo3D/prediction_single_pressure.rds")

pred$pred <- pred$pred[1:2341, ]
pred$conditional_var <- pred$conditional_var[1:2341, 1:2341]

# Read grid and restrict to prediction locations
grid <- read.csv("results_argo3D/warped_test_data-sinPres.csv")
grid <- grid[1:2341, ]

# Anchor points
start_pt <- grid[614, c("s1","s2", "s3")]
end_pt   <- grid[2060, c("s1","s2", "s3")]

# Distance function
dist_fun <- function(pt, grid){
  sqrt((grid$s1 - pt$s1)^2 + (grid$s2 - pt$s2)^2 + (grid$s3 - pt$s3)^2)
}

# Distances
d_start <- dist_fun(start_pt, grid)
d_end   <- dist_fun(end_pt, grid)

# Select ~100 nearest locations around each anchor
idx_start <- order(d_start)
idx_end   <- order(d_end)


library(MASS)


anchor <- 614   # example: start location

un.grd.train = grid
dist.mat = rdist(un.grd.train)
a=0.2
sigma2=2.3
nu= 1.02

N = dim(un.grd.train)[1]
C = sigma2*matern(dist.mat,a,nu)
C1 <- C[idx_start, idx_start]
R1 <- cov2cor(C1)^2
cor_array1 <- R1[1,]

C2 <- C[idx_end, idx_end]
R2 <- cov2cor(C2)^2
cor_array2 <- R2[1,]


grid <- read.csv("raw_datasets/test_locs_single-pres.csv")
grid <- grid[1:2341, ]
anchor <- 614
near_locs <- grid[idx_start, ]
near_locs$pred <- cor_array1
near_locs$lon <- minmax_inverse(near_locs$lon, -65, -10)
near_locs$lat <- minmax_inverse(near_locs$lat, 32, 66)
  obs_range <- range(cor_array1, na.rm = TRUE)
  print(obs_range)
  # breaks_manual <- round(seq(obs_range[1], obs_range[2], length.out = 4))
  min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/3
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval), digits = 1)
  print(breaks_manual)
  ## Predictions
p1 <- ggplot(data = near_locs, 
         aes(x = lon, y = lat, fill = pred)) +
    geom_raster() +
    geom_point(
      data = near_locs[1, ],
      aes(x = lon, y = lat),
      shape = 4,
      size = 1,
      stroke = 1.5,
      colour = "black"
    ) +
    
    scale_fill_gradientn(
      colours = c("#2c7bb6", "#ffffbf", "#d7191c"),
      guide = guide_colorbar(
        barwidth = bar_width,
        barheight = bar_height,
        title.vjust = 3.5
      ),
      breaks = breaks_manual
    ) +
    
    scale_x_continuous(
      expand = c(0,0),
      breaks = c(-60, -40, -20)
    ) +
    
    scale_y_continuous(
      expand = c(0,0),
      breaks = c(35, 50, 65)
    ) +
    
    labs(
      x = "Longitude (degrees)",
      y = "Latitude (degrees)",
      fill = expression(widehat(cov)(Y(s[0]), Y(s)))
    ) +
    
    theme_bw(base_size = base_size) +
    theme(
      plot.margin = margin(t = 2.5, r = 1, b = 1, l = 1),
      legend.text = element_text(size = rel(l_t-2), hjust = 1),
      legend.title = element_text(size = l_s-1),
      axis.title = element_text(size = a_s-1),
      axis.text = element_text(size = a_s),
      axis.title.x = element_text(margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 5))
    )
  ggsave(paste0("results_argo3D/plots/corr_loc1.pdf"), 
         plot = p1, width = plot_saving_width,
         height = plot_saving_height, units = "in")



near_locs <- grid[idx_end, ]
near_locs$pred <- cor_array2
near_locs$lon <- minmax_inverse(near_locs$lon, -65, -10)
near_locs$lat <- minmax_inverse(near_locs$lat, 32, 66)
  obs_range <- range(cor_array1, na.rm = TRUE)
  print(obs_range)
  # breaks_manual <- round(seq(obs_range[1], obs_range[2], length.out = 4))
  min_point <- floor(obs_range[1])
  interval <- (obs_range[2] - obs_range[1])/3
  breaks_manual <- round(c(min_point,
                    min_point + interval,
                    min_point + 2*interval,
                    min_point + 3*interval), digits = 1)
  print(breaks_manual)
  ## Predictions
p1 <- ggplot(data = near_locs, 
         aes(x = lon, y = lat, fill = pred)) +
    geom_raster() +
    geom_point(
      data = near_locs[1, ],
      aes(x = lon, y = lat),
      shape = 4,
      size = 1,
      stroke = 1.5,
      colour = "black"
    ) +
    
    scale_fill_gradientn(
      colours = c("#2c7bb6", "#ffffbf", "#d7191c"),
      guide = guide_colorbar(
        barwidth = bar_width,
        barheight = bar_height,
        title.vjust = 3.5
      ),
      breaks = breaks_manual
    ) +
    
    scale_x_continuous(
      expand = c(0,0),
      breaks = c(-60, -40, -20)
    ) +
    
    scale_y_continuous(
      expand = c(0,0),
      breaks = c(35, 50, 65)
    ) +
    
    labs(
      x = "Longitude (degrees)",
      y = "Latitude (degrees)",
      fill = expression(widehat(cov)(Y(s[0]), Y(s)))
    ) +
    
    theme_bw(base_size = base_size) +
    theme(
      plot.margin = margin(t = 2.5, r = 1, b = 1, l = 1),
      legend.text = element_text(size = rel(l_t-2), hjust = 1),
      legend.title = element_text(size = l_s-1),
      axis.title = element_text(size = a_s-1),
      axis.text = element_text(size = a_s),
      axis.title.x = element_text(margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 5))
    )
  ggsave(paste0("results_argo3D/plots/corr_loc2.pdf"), 
         plot = p1, width = plot_saving_width,
         height = plot_saving_height, units = "in")



#### Trace plot of the optimization 

# Load libraries
library(ggplot2)
library(dplyr)
l_s <- 9
l_t <- 7
a_s <- 9
plot_saving_width <- 5
plot_saving_height <- 3.5
base_size <- 6
bar_width <- 0.5
bar_height <- 3
# Read CSV
df <- read.csv("raw_datasets/optimization_trace.csv")

# Determine unique global epochs for vertical lines
epoch_lines <- df %>%
  group_by(global_epoch) %>%
  summarise(max_iter = max(iteration))

# Plot
p <- ggplot(df, aes(x = iteration, y = log_likelihood)) +
  geom_line(color = "#1f77b4", size = 1) +
  # geom_point(aes(color = factor(global_epoch)), size = 2) +
  geom_vline(data = epoch_lines, aes(xintercept = max_iter),
             linetype = "dashed", color = "red") +
  labs(
    title = "Optimization trace of Log-likelihood",
    x = "Iteration",
    y = "Log-likelihood",
    color = "Global Epoch"
  ) +
  theme_bw(base_size = base_size) +
  theme(
    plot.title = element_text(size = a_s, hjust = 0.5),
        axis.title=element_text(size=a_s), 
        axis.text=element_text(size=a_s),
    legend.position = "none"
  )

# Optional: Save plot
ggsave("results_argo3D/plots/optimization_trace_plot.pdf",
plot = p, width = plot_saving_width,
       height = plot_saving_height, units = "in")
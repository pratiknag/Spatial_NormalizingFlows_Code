library(geoR)
library(MASS)
library(fields)
library(ggplot2)
library(rjson)
library(viridis)
library(reshape2)

json_file <- "raw_datasets/AWU_RBF_LFT_2D.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

make_grid <- function(n = 101, lim = 2) {
  expand.grid(
    s1 = seq(-lim, lim, length.out = n),
    s2 = seq(-lim, lim, length.out = n)
  )
}
warp_twist <- function(df, a = 0.3, b = 0.05) {
  r <- sqrt(df$s1^2 + df$s2^2)
  theta <- atan2(df$s2, df$s1)
  twist <- a * r + b * r^3
  theta_new <- theta + twist
  data.frame(
    s1 = r * cos(theta_new),
    s2 = r * sin(theta_new)
  )
}

df_grid = make_grid(n = 101)
data_loc = warp_twist(df_grid) 
rng <- range(as.matrix(df_grid))
df_grid[] <- (df_grid - rng[1]) / diff(rng) - 0.5
rng <- range(as.matrix(data_loc))
data_loc[] <- (data_loc - rng[1]) / diff(rng) - 0.5
s <- split(df_grid, seq(nrow(df_grid)))
s <- lapply(s, as.numeric)
json_data$s <- s
swarped <- split(data_loc, seq(nrow(data_loc)))
swarped <- lapply(swarped, as.numeric)
json_data$swarped <- swarped

ggplot(data_loc, aes(s1, s2)) +
geom_point(size = 0.6, alpha = 0.7) +
theme_minimal(base_size = 14) +
labs(title = "Nonlinear Bijective Warpings of a 2D Grid")


##### generating the dataset
dist.mat = rdist(data_loc)
a=0.05
sigma2=0.8
nu=1.5
nug=0.01
N = dim(data_loc)[1]
C = sigma2*matern(dist.mat,a,nu)
NUG = diag(nug,nrow = N,ncol=N)

# Sigma_long <- melt(C)

# # Plot with ggplot2
# ggplot(Sigma_long, aes(Var1, Var2, fill = value)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
#   geom_text(aes(label = round(value, 2)), color = "black") +
#   theme_minimal() +
#   labs(title = "Covariance Matrix Heatmap",
#        x = "",
#        y = "")

data = mvrnorm(n = 1, mu = rep(0,N), Sigma = C)
############## Inverting C11 ##########

data_noise = data + rnorm(length(data), 0, 0.01)

df_grid$y = data
df_grid$z = data_noise
json_data$f_true <- df_grid$y

df_sample <- df_grid[sample(nrow(df_grid), 4000), ]
sobs <- split(df_sample[,1:2], seq(nrow(df_sample)))
sobs <- lapply(sobs, as.numeric)
json_data$sobs <- sobs
json_data$y <- df_sample$z
json_data <- toJSON(json_data)
write(json_data, file = "raw_datasets/TWIST_2D.json")
df_sample$y <- NULL
write.csv(df_sample, file = "raw_datasets/TWIST_2D.csv", row.names = FALSE)
df_grid$h1 <- data_loc$s1
df_grid$h1 <- data_loc$s2
write.csv(df_grid, file = "raw_datasets/simulation_Twist2d-full_data.csv", row.names = FALSE)

# l_s <- 10
# l_t <- 10.2
# a_s <- 13
# plot_saving_width <- 2.3
# plot_saving_height <- 1.7
# base_size <- 1
# bar_width <- 0.5
# bar_height <- 5
# p1 <- ggplot(data = df_grid, 
#              aes(x = s1, 
#                  y = s2)) +
#   geom_raster(data = subset(df_grid, !is.na(z)), 
#               aes(fill=z)) +
#   scale_fill_viridis(option = "A",
#                      guide = guide_colorbar(barwidth = bar_width, 
#                                             barheight = bar_height,
#                                             title.vjust = 3.5),
#                      breaks = pretty(range(df_grid$y, 
#                                            na.rm = TRUE), n=4)) +
#   labs(x = "s1", 
#        y = "s2", 
#        fill = "Pred.") +
#   theme_bw(base_size = base_size) + 
#   theme(legend.text=element_text(size=rel(l_t)), 
#         legend.title = element_text(size=l_s),
#         axis.title=element_text(size=a_s), 
#         axis.text=element_blank())
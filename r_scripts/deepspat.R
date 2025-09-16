##############################################################################
## Title:  Reproducible script for generating the results of the 2D experiment
##         in Section 4.2.2
## Author: Andrew Zammit-Mangion
## Date:   15 March 2020
##############################################################################
rm(list = ls())

cat("===========================================================\n")
cat("Running the deepspat models for different simulation scenarios ....\n")
cat("===========================================================\n")
library(reticulate)
use_virtualenv("/home/praktik/Desktop/Spatial_NormalizingFlows/TFv2", required = TRUE)
library(tensorflow)
library(tfprobability)
# library(keras)
library(deepspat)
library(devtools)
library(dplyr)


  tf <- import("tensorflow", delay_load = TRUE)
  tf$cholesky_lower <- tf$linalg$cholesky
  tf$cholesky_upper <- function(x) tf$linalg$matrix_transpose(tf$linalg$cholesky(x))
  tf$matrix_inverse <- tf$linalg$inv
  # assign(".tf", tf, envir = .deepspat_env)
  # get(".tf", envir = .deepspat_env)


## Set up model
set.seed(1)
set_deepspat_seed(1L)

r1 <- 50L  # number of basis functions for the AWUs
r2 <- 400L # number of basis functions in top layer

# simnames <- c("AWU_RBF_LFT_2D.json",
#               "AWU_RBF_2D.json",
#               "TWIST_2D.json")

simnames <- c("TWIST_2D.json")

for(simname in simnames) {
  for(method in c("ML")) {

  ## Load data
  sim <- jsonlite::read_json(paste0("raw_datasets/", simname),  simplifyVector = TRUE)
  if(simname == "TWIST_2D.json"){
    sim$sobs <- unname(sim$sobs)
    sim$sobs <- do.call(rbind, sim$sobs)
    sim$s <- unname(sim$s)
    sim$s <- do.call(rbind, sim$s)
    sim$swarped <- unname(sim$swarped)
    sim$swarped <- do.call(rbind, sim$swarped)
    df <- cbind(sim$sobs, sim$y) %>% as.data.frame()
  }else{
    df <- cbind(sim$sobs, sim$y) %>% as.data.frame()
  }
  
  names(df) <- c("s1", "s2", "z")

  # Set up layers
  # layers <- c(AWU(r = r1, dim = 1L, grad = 200, lims = c(-0.5, 0.5)),
  #             AWU(r = r1, dim = 2L, grad = 200, lims = c(-0.5, 0.5)),
  #             RBF_block(),
  #             LFT(),
  #             bisquares2D(r = r2))

  ## Start stopwatch
  t1 <- proc.time()
 layers_gp <- c(AWU(r = r1, dim = 1L, grad = 200, lims = c(-0.5, 0.5)),
              AWU(r = r1, dim = 2L, grad = 200, lims = c(-0.5, 0.5)),
              RBF_block(),
              LFT())
 # 
 # 
  d <- deepspat_GP(f = z ~ s1 + s2 - 1, data = df, layers = layers_gp,
            g = ~1,
            method = "REML",
            family = "exp_nonstat",
            nsteps = 200L,
            par_init = initvars(l_top_layer = 0.5),
            learn_rates = init_learn_rates(eta_mean = 0.02))
  ## Fit model and predict
  # d <- deepspat(f = z ~ s1 + s2 - 1, data = df, layers = layers,
  #               method = method, MC = 10L, nsteps = 400L,
  #               learn_rates = init_learn_rates(eta_mean = 0.02))

  newdata <- data.frame(s1 = sim$s[, 1],
                        s2 = sim$s[, 2],
                        y = sim$f_true)

  pred_results <- predict(d, newdata = newdata, nsims = 100L)


  ## Stop stopwatch
  t2 <- proc.time()
  cat("Completed in: ", (t2 - t1)[3]," seconds")


  ## Extract results and save
  df_pred <- pred_results$df_pred %>%
             left_join(newdata, by = c("s1", "s2"))
  save(df_pred, file = paste0("raw_datasets/",simname,"_",method,".rda"))
  }
}
cat("===========================================================\n")
cat("Training completed and results have been stored in 'raw_datasets' folder\n")
cat("===========================================================\n")


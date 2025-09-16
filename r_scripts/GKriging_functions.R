#!/usr/bin/env Rscript

library(geoR)
library(MASS)
library(fields)

k = function(u1,u2,h){
  distance = sum((u1 - u2)^2)
  return(exp(-distance/(2*h)))
}

param_est = function(u,nod_points,sigma,h,split){
  k_part = rep(NA,split)
  for(i in 1:split){
    
    k_part[i] = k(u,unlist(nod_points[i],use.names = FALSE),h)
    
  }
  sum_k = sum(k_part)
  param_est1 = 0
  for(i in 1:split){
    param_est1 = param_est1 + (sigma[i]*(k_part[i]/sum_k))
    # param_est1 = param_est1 + (sigma[i]*(k_part[i]))
  }
  return(param_est1)
}

estimation_vec = function(nod_points, sigma_l, h ,split, N, co_ords,multidim){
  param_est_vec = rep(NA,N)
  for(i in 1:N){
    if(multidim == TRUE){
      u1<- unlist(co_ords[i,],use.names = FALSE)
      param_est_vec[i] = param_est(u1,nod_points,sigma_l,h,split)
    }
    else{
      u1<- co_ords[i]
      param_est_vec[i] = param_est(u1,nod_points,sigma_l,h,split)
    }
  }
  return(param_est_vec)
}

NS_matern = function(u1, u2,sigma_vec,nu_vec, a_vec, i,j ){
  cap_sigma_i = a_vec[i]*diag(2)
  cap_sigma_j = a_vec[j]*diag(2)
  term1 = sigma_vec[i]*sigma_vec[j]*(sqrt(a_vec[i]))*(sqrt(a_vec[j]))
  
  term2 = 2/(a_vec[i]+a_vec[j])
  
  neuij = (nu_vec[i] + nu_vec[j])/2
  # neuij = 0.95
  Qij = term2* sum((u1 - u2)^2)
  prod1 = 2*sqrt(neuij * Qij)
  term3 = matern(prod1, 1, neuij)
  
  return(term1*term2*term3)
} 

mle_ind_mat=function(p,data_train, node_points)
{
  un.grd.train = data_train
  #dist.mat = rdist(un.grd.train[,-dim(data_train)[2]])
  z= un.grd.train[,dim(un.grd.train)[2]]
  a1=p[1]
  nu1=p[2]
  sigma1=p[3]
  a2=p[4]
  nu2=p[5]
  sigma2=p[6]
  nug=p[7]
  if(sum(p[1:6]<0)!=0||nug<0)
  {
    nloglikelihood=10000000
    return(list(mlv=nloglikelihood,params=NULL))
  }
  else
  {
    split = 2
    N = dim(data_train)[1]
    co_ords = un.grd.train[,-dim(data_train)[2]]
    if(dim(data_train)[2]>2) multidim = TRUE
    else multidim = FALSE
    sigma_vec = estimation_vec(node_points,c(sigma1,sigma2), h= 0.05,split,N, co_ords, multidim)
    nu_vec = estimation_vec(node_points,c(nu1,nu2), h= 0.05,split,N, co_ords, multidim)
    a_vec = estimation_vec(node_points,c(a1,a2), h= 0.05,split,N, co_ords, multidim)
    # print(sigma_vec)
    # print(nu_vec)
    # print(a_vec)
    NS_Cov_matrix = matrix(0, nrow = N, ncol = N)
    for(i in 1:N){
      for(j in i:N){
        if(multidim == TRUE){
        u1 <- unlist(co_ords[i,],use.names = FALSE)
        u2 <- unlist(co_ords[j,],use.names = FALSE)
        value = NS_matern(u1, u2,sigma_vec,nu_vec, a_vec, i,j )
        NS_Cov_matrix[i,j] <- value
        NS_Cov_matrix[j,i] <- value
        }
        else{
          u1 <- co_ords[i]
          u2 <- co_ords[j]
          value = NS_matern(u1, u2,sigma_vec,nu_vec, a_vec, i,j )
          NS_Cov_matrix[i,j] <- value
          NS_Cov_matrix[j,i] <- value
        }
      }
    }
    NUG=diag(nug,nrow = N,ncol=N)
    
    
    ############## Inverting C11 ##########
    C=NS_Cov_matrix+NUG
    ## Inverse calculation of C
    H = eigen(C)
    e_vec = H$vectors
    D = H$values
    D = 1/D
    for(i in 1:length(D)){
      if(D[i] > 1e+05) D[i] = 0
    }
    D.inv = diag(D)
    Cinv = e_vec%*%D.inv%*%t(e_vec)
    logD=determinant(C)$modulus
    
    nloglikelihood =(0.5 * logD + 0.5 * t(z) %*% Cinv %*% z+0.5*length(z)*log(2*pi))
    if(abs(nloglikelihood) == Inf || is.nan(nloglikelihood)){ nloglikelihood = 1e+08}
    return(list(mlv=nloglikelihood,full.cov=C, df_train=un.grd.train))
  }
  
}




optim_indmat_loglik = function(pars, data_train, node_points){
  mle_ind_mlv=function(pars)
{
  return(mle_ind_mat(p=pars,data_train, node_points)$mlv)
}
p <- optim(par=pars,
        fn = mle_ind_mlv,
        hessian=FALSE, 
        control=list(trace=6,
                     pgtol=0,
                     parscale=rep(0.1,length(pars)),
                     maxit=300))
  return(p)
}


pred.summary=function(estim.par, data_train, test.data)
{
  un.grd.train = data_train
  p = estim.par
  #dist.mat = rdist(un.grd.train[,-dim(data_train)[2]])
  z= un.grd.train[,dim(un.grd.train)[2]]
  a1=p[1]
  nu1=p[2]
  sigma1=p[3]
  a2=p[4]
  nu2=p[5]
  sigma2=p[6]
  nug=p[7]
  # print("I am here 1")
  if(dim(test.data)[2] == 1){
    co_ords = rbind(as.matrix(test.data[,1]),
                    as.matrix(un.grd.train[,-dim(data_train)[2]]))
  }
  else if(dim(test.data)[2] == 2){
    co_ords = rbind(as.matrix(test.data[,c(1,2)]),
                    as.matrix(un.grd.train[,-dim(data_train)[2]]))
  }
  else{
    co_ords = rbind(as.matrix(test.data[,-dim(data_train)[2]]),
                    as.matrix(un.grd.train[,-dim(data_train)[2]]))
  }
  split = 2
  N = dim(data_train)[1] + dim(test.data)[1]
  
  if(dim(data_train)[2]>2) multidim = TRUE
  else multidim = FALSE
  # print(node_points)
  # print(N)
  # print(co_ords)
  sigma_vec = estimation_vec(node_points,c(sigma1,sigma2), h= 0.05,split,N, co_ords, multidim)
  nu_vec = estimation_vec(node_points,c(nu1,nu2), h= 0.05,split,N, co_ords, multidim)
  a_vec = estimation_vec(node_points,c(a1,a2), h= 0.05,split,N, co_ords, multidim)
  # print(sigma_vec)
  # print(nu_vec)
  # print(a_vec)
  # print("I am here 2")
  NS_Cov_matrix = matrix(0, nrow = N, ncol = N)
  if (multidim) {
    co_ords_mat <- as.matrix(co_ords)  # Ensure matrix once
    for (i in 1:N) {
      u1 <- co_ords_mat[i, ]
      for (j in i:N) {
        u2 <- co_ords_mat[j, ]
        value <- NS_matern(u1, u2, sigma_vec, nu_vec, a_vec, i, j)
        NS_Cov_matrix[i, j] <- value
        if (i != j) NS_Cov_matrix[j, i] <- value
      }
      # if (i %% 50 == 0) {
      #   cat(sprintf("Completed row %d out of %d\n", i, N))
      # }
    }
  } else {
    co_ords_vec <- as.numeric(co_ords)  # Ensure vector once
    for (i in 1:N) {
      u1 <- co_ords_vec[i]
      for (j in i:N) {
        u2 <- co_ords_vec[j]
        value <- NS_matern(u1, u2, sigma_vec, nu_vec, a_vec, i, j)
        NS_Cov_matrix[i, j] <- value
        if (i != j) NS_Cov_matrix[j, i] <- value
      }
      # if (i %% 50 == 0) {
      #   cat(sprintf("Completed row %d out of %d\n", i, N))
      # }
    }
  }
  # NUG=diag(nug,nrow = N,ncol=N)
  
  # print("I am here 3")
  ############## Inverting C11 ##########
  C=NS_Cov_matrix
  test.index = c(1:dim(test.data)[1])
  
  C.test=C[test.index,test.index]
  C.train=C[-test.index,-test.index]
  C.test.train=C[test.index,-test.index]
  
  print(dim(C.train))
  print(dim(C.test.train))
  
  
  NUG=diag(nug,nrow = dim(un.grd.train)[1],ncol=dim(un.grd.train)[1])
  ## Inverse of C.train
  H = eigen(C.train + NUG)
  e_vec = H$vectors
  D = H$values
  D = 1/D
  for(i in 1:length(D)){
    if(D[i] > 1e+05) D[i] = 0
  }
  D.inv = diag(D)
  C.train.inv<-e_vec%*%D.inv%*%t(e_vec)
  ##################
  
  
  prediction= C.test.train%*%(C.train.inv)%*%(z) #Conditional mean of a Multivariate Gaussian
  cond_var_MAT= C.test - C.test.train%*%(C.train.inv)%*%t(C.test.train)
  return(list(pred = prediction, conditional_var = cond_var_MAT))
  
  
}

##########################################################
####### GP exp covariance ################################
##########################################################

exp_cov = function(dist.mat, a, sigma2){
  return(sigma2 * exp((-(dist.mat))/a))
}

mle_exp=function(p,data_train)
{
  un.grd.train = data_train
  dist.mat = rdist(un.grd.train[,-dim(data_train)[2]])
  z= un.grd.train$y
  a=exp(p[1])
  sigma2=exp(p[2])
  nug=exp(p[3])
  N = dim(un.grd.train)[1]
    # print(a)
    # print(sigma)
    # print(dist.mat[1:10,1:10])
    # print(nug)
    C = exp_cov(dist.mat,a,sigma2)
    NUG = diag(nug,nrow = N,ncol=N)
    
    ############## Inverting C11 ##########
    C=C+NUG

    Cinv <- chol2inv(chol(C))
    ## Inverse calculation of C
    # H = eigen(C)
    # e_vec = H$vectors
    # D = H$values
    # D = 1/D
    # for(i in 1:length(D)){
    #   if(D[i] > 1e+05) D[i] = 0
    # }
    # D.inv = diag(D)
    # Cinv = e_vec%*%D.inv%*%t(e_vec)
    logDetC=determinant(C)$modulus
    
    nloglikelihood =(0.5 * logDetC + 0.5 * t(z) %*% Cinv %*% z+0.5*length(z)*log(2*pi))
    if(abs(nloglikelihood) == Inf || is.nan(nloglikelihood)){ nloglikelihood = 1e+08}
    return(list(mlv=nloglikelihood,full.cov=C, df_train=un.grd.train))
  
}


mle_exp_mlv=function(pars)
{
  return(mle_exp(p=pars,data_train)$mlv)
}

optim_exp_loglik = function(par){
  optim(par=par,
        fn = mle_exp_mlv,
        hessian=FALSE, 
        control=list(trace=6,
                     pgtol=0,
                     parscale=c(0.1,0.1,2),
                     maxit=500))
}


pred.summary.exp=function(estim.par, data_train, test.data)
{
  un.grd.train = data_train
  p = estim.par
  z= un.grd.train$y
  N = dim(data_train)[1] + dim(test.data)[1]
  a=exp(p[1])
  sigma2=exp(p[2])
  nug=exp(p[3])
  print(a)
  print(sigma2)
  print(nug)
  if(dim(test.data)[2] == 1){
    co_ords = rbind(as.matrix(test.data[,1]),
                    as.matrix(un.grd.train[,-dim(data_train)[2]]))
  }
  else if(dim(test.data)[2] == 2){
    co_ords = rbind(as.matrix(test.data[,c(1,2)]),
                    as.matrix(un.grd.train[,-dim(data_train)[2]]))
  }
  else{
    co_ords = rbind(as.matrix(test.data[,-dim(data_train)[2]]),
                    as.matrix(un.grd.train[,-dim(data_train)[2]]))
  }
  
  dist.mat = rdist(co_ords)
  C = exp_cov(dist.mat,a,sigma2)
  print(C[1:10,1:10])
  
  ############## Inverting C11 ##########
  
  test.index = c(1:dim(test.data)[1])
  
  C.test=C[test.index,test.index]
  C.train=C[-test.index,-test.index]
  C.test.train=C[test.index,-test.index]
  
  print(dim(C.train))
  print(dim(C.test.train))
  
  # z= un.grd.train$y
  
  
  NUG=diag(nug,nrow = dim(un.grd.train)[1],ncol=dim(un.grd.train)[1])
  # ## Inverse of C.train
  # H = eigen(C.train)
  # e_vec = H$vectors
  # D = H$values
  # D = 1/D
  # for(i in 1:length(D)){
  #   if(D[i] > 1e+05) D[i] = 0
  # }
  # D.inv = diag(D)
  C.train.inv<- chol2inv(chol(C.train))
  ##################
  
  
  prediction= (C.test.train)%*%(C.train.inv)%*%(z) #Conditional mean of a Multivariate Gaussian
  cond_var_MAT= C.test - (C.test.train)%*%(C.train.inv)%*%t(C.test.train)
  return(list(pred = prediction, conditional_var = cond_var_MAT))
  
  
}



####################################################################################
#################### GP Mat\'ern covariance ########################################
####################################################################################

manual_inv=function(C){
  H = eigen(C)
  e_vec = H$vectors
  D = H$values
  D = 1/D
  for(i in 1:length(D)){
    if(D[i] > 1e+05) D[i] = 0
  }
  D.inv = diag(D)
  Cinv = e_vec%*%D.inv%*%t(e_vec)
  return(Cinv)
}

mle_matern=function(p,data_train)
{
  un.grd.train = data_train
  dist.mat = rdist(un.grd.train[,-dim(data_train)[2]])
  z= un.grd.train[,dim(un.grd.train)[2]]
  a=exp(p[1])
  sigma2=exp(p[2])
  nu=exp(p[3])
  nug=exp(p[4])
  N = dim(un.grd.train)[1]
  # print(a)
  # print(sigma)
  # print(dist.mat[1:10,1:10])
  # print(nug)
  C = sigma2*matern(dist.mat,a,nu)
  NUG = diag(nug,nrow = N,ncol=N)
  
  ############## Inverting C11 ##########
  C=C+NUG
  
  Cinv <- tryCatch({
    # Try Cholesky-based inversion (faster and more stable for positive definite matrices)
    chol2inv(chol(C))
  }, error = function(e) {
    message("Cholesky decomposition failed: ", e$message)
    message("Falling back to solve() for matrix inversion.")
    
    # Fallback: Use solve() which works for any invertible matrix
    tryCatch({
      manual_inv(C)
    }, error = function(e2) {
      message("Matrix inversion failed using manual inverse: ", e2$message)
      return(NULL)  # Final fallback
    })
  })
  ## Inverse calculation of C
  
  logDetC=determinant(C)$modulus
  
  nloglikelihood =(0.5 * logDetC + 0.5 * t(z) %*% Cinv %*% z+0.5*length(z)*log(2*pi))
  if(abs(nloglikelihood) == Inf || is.nan(nloglikelihood)){ nloglikelihood = 1e+08}
  return(list(mlv=nloglikelihood,full.cov=C, df_train=un.grd.train))
  
}


optim_matern_loglik = function(pars, data_train){
  mle_matern_mlv=function(pars)
{
  return(mle_matern(p=pars,data_train)$mlv)
}
  return(optim(par=pars,
        fn = mle_matern_mlv,
        hessian=FALSE, 
        control=list(trace=6,
                     pgtol=0,
                     parscale=c(0.1,0.1,0.1,0.1),
                     maxit=500)))
}


pred.summary.matern=function(estim.par, data_train, test.data)
{
  un.grd.train = data_train
  p = estim.par
  z= un.grd.train[,dim(un.grd.train)[2]]
  N = dim(data_train)[1] + dim(test.data)[1]
  a=exp(p[1])
  sigma2=exp(p[2])
  nu=exp(p[3])
  nug=exp(p[4])
  print(a)
  print(sigma2)
  print(nug)
  
  if(dim(test.data)[2] == 1){
    co_ords = rbind(as.matrix(test.data[,1]),
                    as.matrix(un.grd.train[,-dim(data_train)[2]]))
  }
  else if(dim(test.data)[2] == 2){
    co_ords = rbind(as.matrix(test.data[,c(1,2)]),
                    as.matrix(un.grd.train[,-dim(data_train)[2]]))
  }
  else{
    co_ords = rbind(as.matrix(test.data[,c(1,2,3)]),
                    as.matrix(un.grd.train[,-dim(data_train)[2]]))
  }
  
  dist.mat = rdist(co_ords)
  C = sigma2*matern(dist.mat,a,nu)
  
  ############## Inverting C11 ##########
  
  test.index = c(1:dim(test.data)[1])
  
  C.test=C[test.index,test.index]
  C.train=C[-test.index,-test.index]
  C.test.train=C[test.index,-test.index]
  
  print(dim(C.train))
  print(dim(C.test.train))
  
  # z= un.grd.train$y
  
  
  NUG=diag(nug,nrow = dim(un.grd.train[1]),ncol=dim(un.grd.train[1]))
  # ## Inverse of C.train
  # H = eigen(C.train)
  # e_vec = H$vectors
  # D = H$values
  # D = 1/D
  # for(i in 1:length(D)){
  #   if(D[i] > 1e+05) D[i] = 0
  # }
  # D.inv = diag(D)
  C.train.inv<- chol2inv(chol(C.train + NUG))
  ##################
  
  
  prediction= (C.test.train)%*%(C.train.inv)%*%(z) #Conditional mean of a Multivariate Gaussian
  cond_var_MAT= C.test - (C.test.train)%*%(C.train.inv)%*%t(C.test.train)
  return(list(pred = prediction, conditional_var = cond_var_MAT))
}



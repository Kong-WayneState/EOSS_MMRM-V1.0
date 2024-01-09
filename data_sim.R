#'Generate MMRM Data.

library(parallel)
# Set the number of cores for parallel calculation. 
# This step is not necessary if you have already defined it in app.R

if(!exists("num.core")) {
  num.core = detectCores() - 1
    }

#Generate data for a single trial
MMRM.sim.1 <- function(nsim, ss, trt.rate, mu, sd, corr, fu1, fu2, fu3=NULL){
  library(dplyr)
  nf = ifelse(is.list(fu3), 3, 2) # nf: number of post-baseline visits
  mean <- rep(mu, (nf+1))   # baseline mean vector
  rho <- matrix(corr, (nf+1), (nf+1))
  diag(rho) <- 1       # correlation matrix
  sigma <- sd^2 * rho # covariance matrix
  
  #Generate (nf+1)-dimensional multinormal data
  #ss: sample size
  sim_data <- MASS::mvrnorm(ss, mean, sigma) 
  
  
  #Randomize TRT assignments using a specified treatment rate
  #0 for PBO and 1 for treatment TRT.
  trt <- sample(0:1, ss, replace = T, prob=c(1-trt.rate, trt.rate))
  
  #TRT set and PBO set
  trt_set <- which(trt == 1)
  pbo_set <- which(trt == 0)
  
  #Randomize ET status
  if (nf == 2){
    et_end <- rep(0, ss) 
    et_end[trt_set] <- sample(0:nf, length(trt_set), replace =T, 
                              prob = c((1 - fu1$et.rate.trt - fu2$et.rate.trt), 
                                       fu1$et.rate.trt, fu2$et.rate.trt)) 
    et_end[pbo_set] <- sample(0:nf, length(pbo_set), replace =T, 
                              prob = c((1 - fu1$et.rate.pbo-fu2$et.rate.pbo), 
                                       fu1$et.rate.pbo, fu2$et.rate.pbo))
    #ET subjects at each FU
    et_fu1 <-  which(et_end == 1)
    et_fu2 <-  which(et_end == 1 | et_end == 2) 
    
    eff_fu1 = rep(fu1$eff, ss) #Efficacy at FU1, that will be adjust by TRT, ET and EFU
    eff_fu2 = rep(fu2$eff, ss) #Efficacy at FU2, that will be adjust by TRT, ET and EFU
    
    eff_fu1[et_fu1] <- sample(c(fu1$eff.efu, NA), length(et_fu1), replace =T, 
                              prob = c(fu1$efu.rate, 1-fu1$efu.rate)) 
    eff_fu2[et_fu2] <- sample(c(fu2$eff.efu, NA), length(et_fu2), replace =T, 
                              prob = c(fu2$efu.rate, 1-fu2$efu.rate))
    
    
    #Generate data with TRT Efficacy and EFU Efficacy
    MMRM_data = data.frame(id=1:ss, trt = trt, et_end = et_end,
                           eff.y1 = eff_fu1*trt,
                           eff.y2 = eff_fu2*trt) %>%
      dplyr::mutate(y0 = round(sim_data[, 1], 2),
                    y1 = round(sim_data[, 2]-eff.y1, 2),
                    y2 = round(sim_data[, 3]-eff.y2, 2))
    
    MMRM_data = MMRM_data %>% 
      dplyr::select(id, trt, y0, y1, y2)
  }else {
    et_end <- rep(0, ss) 
    et_end[trt_set] <- sample(0:nf, length(trt_set), replace =T, 
                              prob = c((1 - fu1$et.rate.trt - fu2$et.rate.trt - fu3$et.rate.trt), 
                                       fu1$et.rate.trt, fu2$et.rate.trt, fu3$et.rate.trt))
    et_end[pbo_set] <- sample(0:nf, length(pbo_set), replace =T, 
                              prob = c((1 - fu1$et.rate.pbo - fu2$et.rate.pbo - fu3$et.rate.pbo), 
                                       fu1$et.rate.pbo, fu2$et.rate.pbo, fu3$et.rate.pbo))
    #ET subjects at each FU
    et_fu1 <-  which(et_end == 1)
    et_fu2 <-  which(et_end == 1 | et_end == 2)
    et_fu3 <-  which(et_end == 1 | et_end == 2 | et_end == 3)
    
    eff_fu1 = rep(fu1$eff, ss) #Efficacy at FU1, that will be adjust by TRT, ET and EFU
    eff_fu2 = rep(fu2$eff, ss) #Efficacy at FU2, that will be adjust by TRT, ET and EFU
    eff_fu3 = rep(fu3$eff, ss) #Efficacy at FU3, that will be adjust by TRT, ET and EFU
    
    eff_fu1[et_fu1] <- sample(c(fu1$eff.efu, NA), length(et_fu1), replace =T, 
                              prob = c(fu1$efu.rate, 1-fu1$efu.rate))
    eff_fu2[et_fu2] <- sample(c(fu2$eff.efu, NA), length(et_fu2), replace =T, 
                              prob = c(fu2$efu.rate, 1-fu2$efu.rate))
    eff_fu3[et_fu3] <- sample(c(fu3$eff.efu, NA), length(et_fu3), replace =T, 
                              prob = c(fu3$efu.rate, 1-fu3$efu.rate))
    
    
    #Generate data with TRT Efficacy and EFU Efficacy
    MMRM_data = data.frame(id=1:ss, trt = trt, et_end = et_end,
                           eff.y1 = eff_fu1*trt,
                           eff.y2 = eff_fu2*trt,
                           eff.y3 = eff_fu3*trt) %>%
      dplyr::mutate(y0 = round(sim_data[, 1], 2),
                    y1 = round(sim_data[, 2]-eff.y1, 2),
                    y2 = round(sim_data[, 3]-eff.y2, 2),
                    y3 = round(sim_data[, 4]-eff.y3, 2))
    
    
    MMRM_data = MMRM_data %>% 
      dplyr::select(id, trt, y0, y1, y2, y3)
  }
  
  return(MMRM_data)
}

#Generate data for n trials.
MMRM.sim.n <- function(n, seed, ss, trt.rate, mu, sd, corr, 
                       fu1, fu2, fu3=NULL){
  #Set up parallel generators and a seed
  my_cluster <- makeCluster(num.core) #
  clusterSetRNGStream(cl = my_cluster, seed)
  
  simdata <- parLapply(my_cluster, 1:n, MMRM.sim.1, ss=ss, trt.rate=trt.rate, 
                       mu=mu, sd=sd, corr=corr, 
                       fu1=fu1, fu2=fu2, fu3=fu3)
  
  stopCluster(my_cluster)
  
  return(simdata)
}

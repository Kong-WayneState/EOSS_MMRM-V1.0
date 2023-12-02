#'Generate Data.

#'Set the number of cores for parallel calculation. 
#'The default setup is with 6 cores.
# num.core = 6     #select and run the code in the console
#'This step is not necessary if you have already defined it in app.R

library(parallel)

#Generate data for a single trial
MMRM.sim.1 <- function(nsim, ss, trt.rate, mu0, sd0, corr0, fu1, fu2, fu3=NULL){
  library(dplyr)
  if (is.list(fu3)){nf = 3} else {nf = 2} #nf: number of post-baseline visits
  mu <- rep(mu0, (nf+1))   # baseline mean vector
  rho <- matrix(corr0, (nf+1), (nf+1))
  diag(rho) <- 1       # correlation matrix
  sigma <- sd0^2 * rho # covariance matrix
  
  #Generate (nf+1)-dimensional multinormal data
  #ss: sample size
  sim_data <- MASS::mvrnorm(ss, mu, sigma) 
  
  
  #Randomize TRT assignments using a specified treatment rate
  #0 for PBO and 1 for treatment TRT.
  trt <- 1 * (runif(ss) <= trt.rate)
  
  #TRT set and PBO set
  trt_set <- which(trt == 1)
  pbo_set <- which(trt == 0)
  
  #Randomize ET status
  if (nf == 2){
    # For 2 post-baseline visits:
    # 0 means not ET,
    # 1 means ET before follow-up 1, EFU,
    # 2 means ET before follow-up 1, not EFU,
    # 3 means ET between follow-up 1 and follow-up 2, EFU ,
    # 4 means ET between follow-up 1 and follow-up 2, not EFU.
    efu_trt <- sample(0:4, length(trt_set), replace =T, 
                      prob = c((1 - fu1$et.rate.trt - fu2$et.rate.trt), 
                               (fu1$et.rate.trt)*c(fu1$efu.rate, 1-fu1$efu.rate), 
                               (fu2$et.rate.trt)*c(fu2$efu.rate, 1-fu2$efu.rate))) 
    efu_pbo <- sample(0:4, length(pbo_set), replace =T, 
                      prob = c((1 - fu1$et.rate.pbo - fu2$et.rate.pbo), 
                               (fu1$et.rate.pbo)*c(fu1$efu.rate, 1-fu1$efu.rate), 
                               (fu2$et.rate.pbo)*c(fu2$efu.rate, 1-fu2$efu.rate))) 
    
    trt_end = pbo_end = rep(0, ss) 
    trt_end[trt_set] <- efu_trt
    pbo_end[pbo_set] <- efu_pbo
    
    #Generate data with TRT Efficacy and EFU Efficacy
    MMRM_data = data.frame(id=1:ss, trt = trt, 
                        trt_end = trt_end,
                        pbo_end = pbo_end,
                        eff.y1 = (fu1$eff)*trt, 
                        eff.y2 = (fu2$eff)*trt) %>%
      dplyr::mutate(eff.y1 = replace(eff.y1, ((trt_end==2)|(pbo_end==2)), NA),
                    eff.y1 = replace(eff.y1, ((trt==1)&(trt_end==1)), fu1$eff.efu),
                    eff.y2 = replace(eff.y2, ((trt_end %in% c(1:2,4))|(pbo_end %in% c(1:2,4))), NA),
                    eff.y2 = replace(eff.y2, ((trt==1)&(trt_end==3)), fu2$eff.efu)) %>%
      dplyr::mutate(y0 = round(sim_data[, 1], 2),
                    y1 = round(sim_data[, 2]-eff.y1, 2),
                    y2 = round(sim_data[, 3]-eff.y2, 2))
    
    MMRM_data = MMRM_data %>% 
      dplyr::select(id, trt, y0, y1, y2)
  }else {
    # For 3 post-baseline visits:
    # 0 means not ET,
    # 1 means ET before follow-up 1, EFU,
    # 2 means ET before follow-up 1, not EFU,
    # 3 means ET between follow-up 1 and follow-up 2, EFU ,
    # 4 means ET between follow-up 1 and follow-up 2, not EFU.
    # 5 means ET between follow-up 2 and follow-up 3, EFU ,
    # 6 means ET between follow-up 2 and follow-up 3, not EFU.
    efu_trt <- sample(0:6, length(trt_set), replace =T, 
                      prob = c((1 - fu1$et.rate.trt - fu2$et.rate.trt - fu3$et.rate.trt), 
                               (fu1$et.rate.trt)*c(fu1$efu.rate, 1-fu1$efu.rate), 
                               (fu2$et.rate.trt)*c(fu2$efu.rate, 1-fu2$efu.rate),
                               (fu3$et.rate.trt)*c(fu3$efu.rate, 1-fu3$efu.rate))) 
    efu_pbo <- sample(0:6, length(pbo_set), replace =T, 
                      prob = c((1 - fu1$et.rate.pbo - fu2$et.rate.pbo - fu3$et.rate.pbo), 
                               (fu1$et.rate.pbo)*c(fu1$efu.rate, 1-fu1$efu.rate), 
                               (fu2$et.rate.pbo)*c(fu2$efu.rate, 1-fu2$efu.rate),
                               (fu3$et.rate.pbo)*c(fu3$efu.rate, 1-fu3$efu.rate))) 
    
    trt_end = pbo_end = rep(0, ss) 
    trt_end[trt_set] <- efu_trt
    pbo_end[pbo_set] <- efu_pbo
   
    #Generate data with TRT Efficacy and EFU Efficacy
    MMRM_data = data.frame(id=1:ss, trt = trt, 
                        trt_end = trt_end,
                        pbo_end = pbo_end,
                        eff.y1 = (fu1$eff)*trt, 
                        eff.y2 = (fu2$eff)*trt,
                        eff.y3 = (fu3$eff)*trt) %>%
      dplyr::mutate(eff.y1 = replace(eff.y1, ((trt_end==2)|(pbo_end==2)), NA),
                    eff.y1 = replace(eff.y1, ((trt==1)&(trt_end==1)), fu1$eff.efu),
                    eff.y2 = replace(eff.y2, ((trt_end %in% c(1:2,4))|(pbo_end %in% c(1:2,4))), NA),
                    eff.y2 = replace(eff.y2, ((trt==1)&(trt_end==3)), fu2$eff.efu),
                    eff.y3 = replace(eff.y3, ((trt_end %in% c(1:4,6))|(pbo_end %in% c(1:4,6))), NA),
                    eff.y3 = replace(eff.y3, ((trt==1)&(trt_end==5)), fu3$eff.efu)) %>%
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
MMRM.sim.n <- function(n, seed, ss, trt.rate, mu0, sd0, corr0, 
                             fu1, fu2, fu3=NULL){
  #Set up parallel generators and a seed
  my_cluster <- makeCluster(num.core) #
  clusterSetRNGStream(cl = my_cluster, seed)
  
  simdata <- parLapply(my_cluster, 1:n, MMRM.sim.1, ss=ss, trt.rate=trt.rate, 
                       mu0=mu0, sd0=sd0, corr0=corr0, 
                       fu1=fu1, fu2=fu2, fu3=fu3)
  
  stopCluster(my_cluster)
  
  return(simdata)
}

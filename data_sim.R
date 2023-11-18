#' Generates a CAPS5 data from a mixed-model repeated measure(MMRM) 
#' with 2 or 3 Follow-up Visits
#'Set number of cores for parallel calculation

#ncore = detectCores()-1 # github for parallel calculation

library(parallel)
library(dplyr)

CAPS5.MMRM.sim <- function(nsim, ss, trt.rate, mu0, sd0, corr0, fu1, fu2, fu3=NULL){
  library(dplyr)
  if (is.list(fu3)){nd = 4} else {nd = 3} # nd: number of visits
  mu <- rep(mu0, nd)   # mean vector
  rho <- matrix(corr0, nd, nd)
  diag(rho) <- 1       # correlation matrix
  sigma <- sd0^2 * rho # covariance matrix
  
  #generate nd-dim multi-normal data of 
  #ss: sample size
  sim.data <- MASS::mvrnorm(ss, mu, sigma) 
  
  
  #generate trt with trt.rate: 0 for placebo and 1 for trt
  trt <- 1*(runif(ss)<trt.rate)
  
  # trt set and pbo set
  trt.set <- which(trt == 1)
  pbo.set <- which(trt == 0)
  
  
  #randomize early terminate(et) and post baseline follow up visits of et
  
  if (nd == 3){
    efu.trt <- sample(0:4, length(trt.set), replace =T, 
                      prob = c((1 - fu1$et.rate.trt - fu2$et.rate.trt), 
                               (fu1$et.rate.trt)*c(fu1$efu.rate, 1-fu1$efu.rate), 
                               (fu2$et.rate.trt)*c(fu2$efu.rate, 1-fu2$efu.rate))) 
    efu.pbo <- sample(0:4, length(pbo.set), replace =T, 
                      prob = c((1 - fu1$et.rate.pbo - fu2$et.rate.pbo), 
                               (fu1$et.rate.pbo)*c(fu1$efu.rate, 1-fu1$efu.rate), 
                               (fu2$et.rate.pbo)*c(fu2$efu.rate, 1-fu2$efu.rate))) 
    
    trt.end = pbo.end = rep(0, ss) 
    trt.end[trt.set] <- efu.trt
    pbo.end[pbo.set] <- efu.pbo
    # 0 means not et, 
    # 1 means et and efu before follow up 1, 
    # 2 means et and not efu before follow up 1, 
    # 3 means et and efu after follow up 1 and before follow up 2, 
    # 4 means et and not efu after follow up 1 and before follow up 2, 
    
    DataET = data.frame(id=1:ss, trt = trt, 
                        trt.end = trt.end,
                        pbo.end = pbo.end,
                        eff.y1 = (fu1$eff)*trt, 
                        eff.y2 = (fu2$eff)*trt) %>%
      dplyr::mutate(eff.y1 = replace(eff.y1, ((trt.end==2)|(pbo.end==2)), NA),
                    eff.y1 = replace(eff.y1, ((trt==1)&(trt.end==1)), fu1$eff.efu),
                    eff.y2 = replace(eff.y2, ((trt.end %in% c(1:2,4))|(pbo.end %in% c(1:2,4))), NA),
                    eff.y2 = replace(eff.y2, ((trt==1)&(trt.end==3)), fu2$eff.efu)) %>%
      dplyr::mutate(y0 = round(sim.data[, 1], 2),
                    y1 = round(sim.data[, 2]-eff.y1, 2),
                    y2 = round(sim.data[, 3]-eff.y2, 2))
    
    MMRM_caps5 = DataET %>% 
      dplyr::select(id, trt, y0, y1, y2)
  }else {
    efu.trt <- sample(0:6, length(trt.set), replace =T, 
                      prob = c((1 - fu1$et.rate.trt - fu2$et.rate.trt - fu3$et.rate.trt), 
                               (fu1$et.rate.trt)*c(fu1$efu.rate, 1-fu1$efu.rate), 
                               (fu2$et.rate.trt)*c(fu2$efu.rate, 1-fu2$efu.rate),
                               (fu3$et.rate.trt)*c(fu3$efu.rate, 1-fu3$efu.rate))) 
    efu.pbo <- sample(0:6, length(pbo.set), replace =T, 
                      prob = c((1 - fu1$et.rate.pbo - fu2$et.rate.pbo - fu3$et.rate.pbo), 
                               (fu1$et.rate.pbo)*c(fu1$efu.rate, 1-fu1$efu.rate), 
                               (fu2$et.rate.pbo)*c(fu2$efu.rate, 1-fu2$efu.rate),
                               (fu3$et.rate.pbo)*c(fu3$efu.rate, 1-fu3$efu.rate))) 
    
    trt.end = pbo.end = rep(0, ss) 
    trt.end[trt.set] <- efu.trt
    pbo.end[pbo.set] <- efu.pbo
    # 0 means not et, 
    # 1 means et and efu before follow up 1, 
    # 2 means et and not efu before follow up 1, 
    # 3 means et and efu after follow up 1 and before follow up 2, 
    # 4 means et and not efu after follow up 1 and before follow up 2, 
    # 5 means et and efu after follow up 2 and before follow up 3, 
    # 6 means et and not efu after follow up 2 and before follow up 3, 
    
    DataET = data.frame(id=1:ss, trt = trt, 
                        trt.end = trt.end,
                        pbo.end = pbo.end,
                        eff.y1 = (fu1$eff)*trt, 
                        eff.y2 = (fu2$eff)*trt,
                        eff.y3 = (fu3$eff)*trt) %>%
      dplyr::mutate(eff.y1 = replace(eff.y1, ((trt.end==2)|(pbo.end==2)), NA),
                    eff.y1 = replace(eff.y1, ((trt==1)&(trt.end==1)), fu1$eff.efu),
                    eff.y2 = replace(eff.y2, ((trt.end %in% c(1:2,4))|(pbo.end %in% c(1:2,4))), NA),
                    eff.y2 = replace(eff.y2, ((trt==1)&(trt.end==3)), fu2$eff.efu),
                    eff.y3 = replace(eff.y3, ((trt.end %in% c(1:4,6))|(pbo.end %in% c(1:4,6))), NA),
                    eff.y3 = replace(eff.y3, ((trt==1)&(trt.end==5)), fu3$eff.efu)) %>%
      dplyr::mutate(y0 = round(sim.data[, 1], 2),
                    y1 = round(sim.data[, 2]-eff.y1, 2),
                    y2 = round(sim.data[, 3]-eff.y2, 2),
                    y3 = round(sim.data[, 4]-eff.y3, 2))
    
    
    MMRM_caps5 = DataET %>% 
      dplyr::select(id, trt, y0, y1, y2, y3)
  }
  
  
  return(MMRM_caps5)
  
}


CAPS5.MMRM.sim.n <- function(n, seed, ss, trt.rate, mu0, sd0, corr0, 
                             fu1, fu2, fu3=NULL){
  #set up parallel generators and seed
  my_cluster <- makeCluster(ncore) #
  clusterSetRNGStream(cl = my_cluster, seed)
  
  #generate n-trails of data
  simdata <- parLapply(my_cluster, 1:n, CAPS5.MMRM.sim, ss=ss, trt.rate=trt.rate, 
                       mu0=mu0, sd0=sd0, corr0=corr0, 
                       fu1=fu1, fu2=fu2, fu3=fu3)
  
  stopCluster(my_cluster)
  
  return(simdata)
}

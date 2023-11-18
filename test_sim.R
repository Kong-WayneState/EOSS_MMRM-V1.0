#' Calculate the Power for the Simulation Data
#'Set number of cores for parallel calculation

# ncore = detectCores()-1 # github for parallel calculation


library(parallel)
library(dplyr)
library(mmrm)


CAPS5.MMRM.test <- function(MMRM){ #one trial of simulated data
  library(dplyr)
  library(mmrm)
  if ("y3" %in% names(MMRM)){
    longData = reshape(MMRM, varying=c("y1", "y2", "y3"),
                       direction="long", sep="", idvar="id") %>%
      dplyr::arrange(desc(id), desc(time)) %>%
      dplyr::mutate(time = factor(time), trt = factor(trt), id = factor(id)) 
    
    fit3 = mmrm::mmrm(formula = y ~ y0+trt+time+trt*time+y0*time + us(time|id), 
                      data = longData,
                      method = "Kenward-Roger-Linear")
    
    contrast <- numeric(length(component(fit3, "beta_est")))
    contrast[c(3, 7)] <- 1
    p.val = df_1d(fit3, contrast)$p_val
    
  } else{
    longData = reshape(MMRM, varying=c("y1", "y2"),
                       direction="long", sep="", idvar="id") %>%
      dplyr::arrange(desc(id), desc(time)) %>%
      dplyr::mutate(time = factor(time), trt = factor(trt), id = factor(id))
    
    fit2 = mmrm::mmrm(formula = y ~ y0+trt+time+trt*time+y0*time + us(time|id), 
                      data = longData,
                      method = "Kenward-Roger-Linear")
    
    contrast <- numeric(length(component(fit2, "beta_est")))
    contrast[c(3, 5)] <- 1
    
    p.val = df_1d(fit2, contrast)$p_val
    
  }
  return(p.val)
  
}


CAPS5.MMRM.test.n <- function(simdata, alpha = 0.05){
  #set up parallel generation
  my_cluster <- makeCluster(ncore) #
  
  #generate n-pvalues from mmrm test
  p.val = unlist(parLapply(my_cluster, simdata, CAPS5.MMRM.test))
  power = mean(p.val < alpha)
  
  stopCluster(my_cluster)
  return(power = power)
}

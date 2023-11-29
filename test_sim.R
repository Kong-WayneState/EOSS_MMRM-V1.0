#'Calculate the Power

#'Set the number of cores for parallel calculation. 
#'The default setup is with 6 cores.
# num.core = 6     #select and run the code in the console
#'This step is not necessary if you have already defined it in app.R


library(parallel)

#Calculate the p-value for one trial of data using mmrm() regression
MMRM.test.1 <- function(MMRM.data){
  if ("y3" %in% names(MMRM.data)){
    longData = reshape(MMRM.data, varying=c("y1", "y2", "y3"),
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
    longData = reshape(MMRM.data, varying=c("y1", "y2"),
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


#Calculate p-values for each trial and determine the simulated power.
MMRM.test.n <- function(simdata, alpha = 0.05){
  #Set up parallel generators
  my_cluster <- makeCluster(num.core) 
  
  #Generate p-values for each trial of the simulated data (n-trial) and 
  #calculate the simulated power (proportion of times the null hypothesis is rejected)
  p.vals = unlist(parLapply(my_cluster, simdata, MMRM.test.1))
  power = mean(p.vals < alpha)
  
  stopCluster(my_cluster)
  
  return(power = power)
}

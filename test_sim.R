#'Calculate the Power

#'Set the number of cores for parallel calculation. 
#'The default setup is with 6 cores.
# num.core = 6     #select and run the code in the console
#'This step is not necessary if you have already defined it in app.R


library(parallel)

#Calculate the p-value for one trial of data using mmrm() regression
MMRM.test.1 <- function(MMRM_data){
  library(dplyr)
  library(mmrm)
  if ("y3" %in% names(MMRM_data)){
    longData = reshape(MMRM_data, varying=c("y1", "y2", "y3"),
                       direction="long", sep="", idvar="id") %>%
      dplyr::arrange(id, time) %>%
      dplyr::mutate(time = factor(time), trt = factor(trt), id = factor(id)) 
    
    fit3 = mmrm::mmrm(formula = y ~ y0+trt+time+trt*time+y0*time + us(time|id), 
                      data = longData, 
                      control = mmrm_control(method = "Kenward-Roger-Linear")) # mmrm v0.2.2
                      
    #control = mmrm_control(method = "Kenward-Roger",
    #vcov = "Kenward-Roger-Linear")) # mmrm v0.3.6
    
    contrast <- numeric(length(mmrm::component(fit3, "beta_est")))
    contrast[c(3, 7)] <- 1
    p_val = df_1d(fit3, contrast)$p_val
  } else{
    longData = reshape(MMRM_data, varying=c("y1", "y2"),
                       direction="long", sep="", idvar="id") %>%
      dplyr::arrange(id, time) %>%
      dplyr::mutate(time = factor(time), trt = factor(trt), id = factor(id))
    
    fit2 = mmrm::mmrm(formula = y ~ y0+trt+time+trt*time+y0*time + us(time|id), 
                      data = longData, 
                      control = mmrm_control(method = "Kenward-Roger-Linear")) # mmrm v0.2.2
    
    #control = mmrm_control(method = "Kenward-Roger",
    #vcov = "Kenward-Roger-Linear")) # mmrm v0.3.6
    
    contrast <- numeric(length(mmrm::component(fit2, "beta_est")))
    contrast[c(3, 5)] <- 1
    p_val = df_1d(fit2, contrast)$p_val
  }
  
  return(p_val)
}


#Calculate p-values for each trial and determine the simulated power.
MMRM.test.n <- function(simdata, alpha = 0.05){
  #Set up parallel generators
  my_cluster <- makeCluster(num.core) 
  
  #Generate p-values for each trial (n-trials) and 
  #calculate the simulated power (proportion of times the null hypothesis is rejected)
  p_vals = unlist(parLapply(my_cluster, simdata, MMRM.test.1))
  power = mean(p_vals < alpha)
  
  stopCluster(my_cluster)
  
  return(power = power)
}

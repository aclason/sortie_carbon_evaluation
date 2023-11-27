# A.Clason

library(equivalence)


###### Functions ###################################################################################
# RMSE and Bias --------------------------------------------------
bias <- function(actual_i, predicted_i, n){
  sum(actual_i - predicted_i)/n
}

rmse <- function(actual_i, predicted_i, n){
  sqrt(sum(((actual_i - predicted_i)^2)/n))
}


# Cohen's D ------------------------------------------------------
cohen_d <- function(actual, predicted){
    mean_diff <- mean(actual) - mean(predicted)
    pooled_sd <- sqrt((sd(actual)^2 + sd(predicted)^2) / 2)
    d <- mean_diff / pooled_sd
    return(d)
}
#effect size - small effect: d = 0.2, med effect: d = 0.5, large effect: d= 0.8
#positive means actual mean is greater than predicted


# Equivalence tests ----------------------------------------------
equi_result <- function(actual, predicted, eq_margin){
  tost(x = actual, 
       y = predicted, 
       epsilon = eq_margin)
}

# equivalence with bootstrapping --------
equi_boot <- function(actual, predicted, n_bootstraps = 1000,
                      eq_margin){
  test_statistic <- function(x, y) {
    mean_diff <- mean(x) - mean(y)
    return(mean_diff)
  }
  
  # Perform equivalence test with bootstrapping
  bootstrapped_stats <- numeric(n_bootstraps)
  
  for (i in 1:n_bootstraps) {
    # Generate bootstrap samples
    bootstrap_model <- sample(predicted, replace = TRUE)
    bootstrap_real <- sample(actual, replace = TRUE)
    
    # Calculate the test statistic for each bootstrap sample
    bootstrapped_stats[i] <- test_statistic(bootstrap_model, bootstrap_real)
  }
  
  # Calculate the confidence interval
  ci_lower <- quantile(bootstrapped_stats, 0.025)
  ci_upper <- quantile(bootstrapped_stats, 0.975)
  
  # Check if the confidence interval falls within the equivalence margin
  equivalence_low <- ci_lower > -eq_margin
  equivalence_upp <- ci_upper < eq_margin
  
  eq_interval <- c(equivalence_low, ci_lower, equivalence_upp, ci_upper)
  return(eq_interval)
} 


# linear models ----------------------------------------------------
lm(ac)

# Mixed effects models
DC_L_C_Hw_lmer <- lmer(C_unit ~ DataSource * Treatment + (1|Unit/timestep), DC_L_C_Hw_u_mod2)

###################################################################################################


#Live trees


#Dead trees


#CWD ----------------------------------------------------------------------------------

MS_cwd_dc
FS_cwd_dc

MS_cwd_sl
MS_cwd_sl[,unit:=as.numeric(unit)] #deal with this earlier
FS_cwd_sl

setkey(FS_cwd_sl,unit)
setkey(MS_cwd_sl,unit)

bias(FS_cwd_sl[timestep==29]$MgHa, MS_cwd_sl[timestep==29]$MgHa, 
     n = 19)
#positive value - actual is higher than predicted

rmse(FS_cwd_sl[timestep==29]$MgHa, MS_cwd_sl[timestep==29]$MgHa, 
     n = 19)
#positive value - actual is 8.2MgHa higher than predicted

cohen_d(FS_cwd_sl[timestep==29]$MgHa, MS_cwd_sl[timestep==29]$MgHa)
#effect size - 


equi_result(FS_cwd_sl[timestep==29]$MgHa, MS_cwd_sl[timestep==29]$MgHa,
            eq_margin = 0.5)
#not rejected == they are different

equi_boot(actual = FS_cwd_sl[timestep==29]$MgHa, 
          predicted = MS_cwd_sl[timestep==29]$MgHa,
          n_bootstraps = 1000,
          eq_margin = 0.5)

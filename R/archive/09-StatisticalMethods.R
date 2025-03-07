# A.Clason

library(equivalence)
#MF_trees_sl_m[,`:=`(unit_f = as.factor(unit),
#                   treatment = as.factor(treatment),
#                    State = as.factor(State))]
#str(MF_trees_sl_m)

dat_sl <- MF_trees_sl_m[val_type == "MgHa"]

model1 <- lmer(val_ha ~ Treatment * TSH + (1 | Unit), 
               data = dat_sl)

model2 <- lmer(val_ha ~ obs_preds * Treatment + TSH + (1 | Unit), 
               data = dat_sl)

model3 <- lmer(val_ha ~ Treatment * TSH + obs_preds + (1 | Unit), 
               data = dat_sl)

model4 <- lmer(val_ha ~ obs_preds * TSH + Treatment  + (1 | Unit), 
               data = dat_sl)

model5 <- lmer(val_ha ~ obs_preds + TSH + Treatment  + (1 | Unit),
               data = dat_sl)

# this estimates the slope effects and contrasts:
emt <- emtrends(model1, tukey ~ obs_preds, var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)

emt <- emtrends(model1, tukey ~ Treatment, var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)

#MgHa ~ Type * treatment * Species * TSH + (1 | unit)
AIC(model1, model2, model3, model4, model5)
dat_sl <- MF_trees_sl_sp_m[Species == "Bl" |Species == "Sx"][val_type == "MgHa"]

model1 <- lmer(val_ha ~ obs_preds * Treatment * Species * TSH + (1 | Unit), 
               data = dat_sl)

model2 <- lmer(val_ha ~ obs_preds * Treatment * Species + TSH + (1 | Unit), 
               data = dat_sl)

model3 <- lmer(val_ha ~ obs_preds * Treatment * TSH + Species + (1 | Unit), 
               data = dat_sl)

model4 <- lmer(val_ha ~ obs_preds * Species * TSH + Treatment + (1 | Unit), 
               data = dat_sl)

model5 <- lmer(val_ha ~ Treatment * Species * TSH + obs_preds + (1 | Unit), 
               data = dat_sl)

model6 <- lmer(val_ha ~ obs_preds * Treatment + Species * TSH + (1 | Unit), 
               data = dat_sl)

model7 <- lmer(val_ha ~ obs_preds * Species + Treatment * TSH + (1 | Unit), 
               data = dat_sl)

model8 <- lmer(val_ha ~ obs_preds * TSH + Treatment * Species + (1 | Unit), 
               data = dat_sl)

model9 <- lmer(val_ha ~ obs_preds + TSH + Treatment * Species + (1 | Unit),
               data = dat_sl)

model10 <- lmer(val_ha ~ Treatment + TSH + obs_preds * Species + (1 | Unit), 
                data = dat_sl)

model11 <- lmer(val_ha ~ Species + TSH + obs_preds * Treatment + (1 | Unit), 
                data = dat_sl)

#MgHa ~ Type * treatment * Species * TSH + (1 | unit)
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
    model11)
MF_trees_dc_m[,`:=`(Unit = as.factor(Unit),
                    TSH = Year - 1992)]
dat_dc <- MF_trees_dc_m[Measure == "MgHa"][TSH > 0]

model1 <- lmer(val_ha ~ Type * Treatment * TSH + (1 | Unit), 
               data = dat_dc)

model2 <- lmer(val_ha ~ Type * Treatment + TSH + (1 | Unit), 
               data = dat_dc)

model3 <- lmer(val_ha ~ Treatment * TSH + Type + (1 | Unit), 
               data = dat_dc)

model4 <- lmer(val_ha ~ Type * TSH + Treatment  + (1 | Unit), 
               data = dat_dc)

model5 <- lmer(val_ha ~ Type + TSH + Treatment  + (1 | Unit),
               data = dat_dc)

#MgHa ~ Type * treatment * Species * TSH + (1 | unit)
AIC(model1, model2, model3, model4, model5)
anova(model1)

contrast(emmeans(model1, ~ Type), method = "tukey")

# this estimates the slope effects and contrasts:
emt <- emtrends(model1, tukey ~ Treatment, var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)


#MF_trees_dc_m[,`:=`(unit_f = as.factor(unit),
#                   treatment = as.factor(treatment),
#                    State = as.factor(State))]
#str(MF_trees_dc_m)
MF_trees_dc_sp_m[,`:=`(Unit = as.factor(Unit),
                       TSH = Year - 1992)]
str(MF_trees_dc_sp_m)
sp_incl <- c("Cw","Sx","Ba", "Pl","Hw")

dat_dc <- MF_trees_dc_sp_m[Species %in% sp_incl][val_type == "MgHa"][TSH > 0]

model1 <- lmer(val_ha ~ obs_preds * Treatment * Species * TSH + (1 | Unit), 
               data = dat_dc)

model2 <- lmer(val_ha ~ obs_preds * Treatment * Species + TSH + (1 | Unit), 
               data = dat_dc)

model3 <- lmer(val_ha ~ obs_preds * Treatment * TSH + Species + (1 | Unit), 
               data = dat_dc)

model4 <- lmer(val_ha ~ obs_preds * Species * TSH + Treatment + (1 | Unit), 
               data = dat_dc)

model5 <- lmer(val_ha ~ Treatment * Species * TSH + obs_preds + (1 | Unit), 
               data = dat_dc)

model6 <- lmer(val_ha ~ obs_preds * Treatment + Species * TSH + (1 | Unit), 
               data = dat_dc)

model7 <- lmer(val_ha ~ obs_preds * Species + Treatment * TSH + (1 | Unit), 
               data = dat_dc)

model8 <- lmer(val_ha ~ obs_preds * TSH + Treatment * Species + (1 | Unit), 
               data = dat_dc)

model9 <- lmer(val_ha ~ obs_preds + TSH + Treatment * Species + (1 | Unit),
               data = dat_dc)

model10 <- lmer(val_ha ~ Treatment + TSH + obs_preds * Species + (1 | Unit), 
                data = dat_dc)

model11 <- lmer(val_ha ~ Species + TSH + obs_preds * Treatment + (1 | Unit), 
                data = dat_dc)

#MgHa ~ Type * treatment * Species * TSH + (1 | unit)
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
    model11)
anova(model1)
contrast(emmeans(model2, ~ Treatment), method = "tukey")
contrast(emmeans(model1, ~ Species), method = "tukey")
contrast(emmeans(model2, ~ Treatment| Species | obs_preds), method = "tukey")

# this estimates the slope effects and contrasts:
emt <- emtrends(model1, tukey ~ Treatment| Species , var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)

emt <- emtrends(model1, tukey ~ Treatment| Species, var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)

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

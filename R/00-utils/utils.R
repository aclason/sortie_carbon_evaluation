
Typical_SS <- data.table(SS=c(10, 9, 7, 5, 6, 1, 3, 2, 4),
                         SMR=c(6, 5.5, 5.5, 4.5, 4.5, 4, 4, 2, 3.5),
                         SNR=c(3.5, 4.8, 2, 4.8, 4.8, 3.6, 2, 2.33, 3.5))

#Functions to support analysis

#install.packages("extrafont")
#extrafont::font_import()
library(extrafont)
theme_set(theme_minimal(base_family = "Arial") +  # Change "Arial" to your desired font
  theme(
    text = element_text(family = "Arial"),  # Change "Arial" to your desired font
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14, face = "bold")
  ))



###### Functions #####################################################
# 1. Bias
    #What it tells you: On average, how much your model overestimates or 
      #underestimates the observed values.
    #Interpretation:
    # Positive → model tends to overpredict
    # Negative → model tends to underpredict

    ### This is a measure of systematic error.

# 2. RMSE (Root Mean Squared Error)
    #What it tells you: The typical size of a prediction error, taking all errors into account.
    #Interpretation: Lower is better; 
    
    ### combines bias and random variation.

# 3. R² (1:1 line)
    #Interpretation: Closer to 1 → predictions are very close to the 1:1 line; 
    #closer to 0 → predictions are far off.

    ### How much of the variation in observed values is “explained” by the model 
    ### if we expect a perfect 1:1 relationship (i.e., slope = 1, intercept = 0).

# 4. Cohen’s d
    # The size of the mean difference between predicted and observed values, 
    # expressed in standard deviations.
    # Small effect: ~0.2
    # Medium: ~0.5
    # Large: ~0.8+
    # Useful to contextualize the magnitude of bias relative to the natural variability of your data.

    ### “Is the average difference practically meaningful?”

# 5. Correlation & R² (correlation)
    # What it tells you: How well the pattern of variation in predictions 
    # matches the pattern in observations, regardless of bias.
   
    ### High correlation → predictions go up and down in step with observations, 
    ### even if they are systematically too high or too low.

# 6. Intercept & Slope
    # From lm(obs ~ pred)
    # If Intercept ≈ 0 and slope ≈ 1 → near perfect 1:1 relationship
    # Deviations → systematic under- or overestimation, especially at high or low values

    ### Intercept → constant offset from 0
    ### Slope → how sensitive predictions are to changes in observed values

# 7. p-value 1:1
    # From linearHypothesis test of (Intercept = 0, Slope = 1)
    # p < 0.05 → model significantly deviates from perfect predictions
    # p > 0.05 → no statistical evidence of deviation

    ### Whether the model is statistically significantly different from a perfect 1:1 relationship.

evaluate_model <- function(obs, pred) {
  mod <- lm(obs ~ pred)
  test <- car::linearHypothesis(mod, c("(Intercept) = 0", "pred = 1"))
  mean_diff <- mean(obs) - mean(pred)
  pooled_sd <- sqrt((sd(obs)^2 + sd(pred)^2) / 2)
  r <- cor(obs, pred, use = "complete.obs")
  
  list(
    # Agreement metrics
    bias = mean(pred - obs, na.rm = TRUE),
    rmse = sqrt(mean((pred - obs)^2, na.rm = TRUE)),
    r2_1to1 = 1 - sum((obs - pred)^2, na.rm = TRUE) /
      sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE),
    cohen_d = mean_diff / pooled_sd,
    
    # Pattern metric
    correlation = r,
    r2_correlation = r^2,
    
    # Diagnostics
    intercept = coef(mod)[1],
    slope = coef(mod)[2],
    
    # Formal test of 1:1
    p_value_1to1 = test$`Pr(>F)`[2]
  )
}

# TOST for bias and slope -------------------------------------------------------------------------
# Combined TOST allowing separate margins for bias and slope

### Bias - average error ---------
# The margin is expressed as a percentage of the mean observed value.
# Example: If your mean observed MgHa = 78, 
# and the bias margin = 0.10 (10%), then your acceptable bias is ±7.8 units.

# FALSE means the bias is too large to meet that equivalence threshold.
# TRUE means the bias is small enough to be considered practically negligible at that threshold.

### Slope - responsiveness of model to changes in reality ---------
# FALSE means the slope is outside the ±margin around 1.
# TRUE means the slope is close enough to 1 to be considered practically equivalent.
  # The margin is expressed as a proportion around 1, e.g., ±0.05 for ±5% (e.g. Acceptable slope 
  # range = 1 ± 0.05 → 0.95 to 1.05.
  # if slope < bottom value, slope_Equivalent = FALSE (too shallow; model under-responds 
  # to changes in observed values)

combined_tost <- function(obs, pred, bias_bounds = c(0.01, 0.05, 0.1),
                          slope_margins = c(0.05, 0.1, 0.15)) {
  
  mean_obs <- mean(obs, na.rm = TRUE)
  
  #  Bias TOST
  bias_results <- data.table(
    Bias_Bound_Percent = bias_bounds,
    Bias_Bound = bias_bounds * mean_obs,
    Bias_p = sapply(bias_bounds, function(b) bias_tost(obs, pred, b * mean_obs))
  )
  
  # Add bias equivalence column
  bias_results[, Bias_Equivalent := Bias_p < 0.05]
  
  #  Slope equivalence
  slope_mod <- lm(obs ~ pred)
  slope <- coef(slope_mod)[2]
  slope_ci <- confint(slope_mod, 'pred', level = 0.90)
  
  # Map slope margins to bias bounds (recycle if lengths differ)
  slope_mapped <- rep(slope_margins, length.out = nrow(bias_results))
  
  bias_results[, `:=`(
    Slope = slope,
    Slope_CI_Lower = slope_ci[1],
    Slope_CI_Upper = slope_ci[2],
    Slope_Equiv = mapply(function(lower, upper, margin) {
      (lower >= 1 - margin) & (upper <= 1 + margin)
    }, slope_ci[1], slope_ci[2], slope_mapped)
  )]
  
  return(bias_results)
}

# Function to get t-test and TOST results for a single year
year_tost_summary <- function(year_data, bounds = equivalence_bounds) {
  
  # Simple t-test of observed vs predicted
  ttest_res <- t.test(year_data$MgHa_obs, year_data$MgHa_pred)
  
  # Run your combined TOST function
  tost_res <- combined_tost(
    obs = year_data$MgHa_obs,
    pred = year_data$MgHa_pred,
    bias_bounds = bounds,
    slope_margins = bounds
  )
  
  # Add the t-test results to the TOST table
  tost_res[, `:=`(
    Year = unique(year_data$Year),
    t_test_p = ttest_res$p.value,
    t_test_mean_diff = mean(year_data$MgHa_obs) - mean(year_data$MgHa_pred)
  )]
  
  return(tost_res)
}
#select_years <- function(year, data, meas_obs, meas_pred) {
#  if (year == "All Years") {
#    obs <- data$meas_obs
#    pred <- data$meas_pred
 #   n_value <- nrow(data)
#  } else {
#    dt <- data
 #   obs <- dt[Year == year]$meas_obs
#    pred <- dt[Year == year]$meas_pred
#   n_value <- nrow(dt[Year == year])
#  }
#  list(obs = obs, pred = pred, n_value = n_value)
#}

select_years <- function(year, data, meas_obs, meas_pred) {
  # Ensure the column names are interpreted correctly
  meas_obs <- enquo(meas_obs)
  meas_pred <- enquo(meas_pred)
  
  if (year == "All Years") {
    obs <- data %>% pull(!!meas_obs)
    pred <- data %>% pull(!!meas_pred)
    n_value <- nrow(data)
  } else {
    dt <- data %>% filter(Year == year)
    obs <- dt %>% pull(!!meas_obs)
    pred <- dt %>% pull(!!meas_pred)
    n_value <- nrow(dt)
  }
  
  list(obs = obs, pred = pred, n_value = n_value)
}
#select_years <- function(year, data, meas) {
 # if (year == "All Years") {
#    obs <- data$MgHa_obs
#    pred <- data$MgHa_pred
#    n_value <- nrow(data)
#  } else {
#    dt <- data
#    obs <- dt[Year == year]$MgHa_obs
#    pred <- dt[Year == year]$MgHa_pred
#    n_value <- nrow(dt[Year == year])
 # }
#  list(obs = obs, pred = pred, n_value = n_value)
#}

select_sp <- function(sp, data) {
  obs <- data[Species == sp]$MgHa_obs
  pred <- data[Species == sp]$MgHa_pred
  n_value <- nrow(data[Species == sp])
  list(obs = obs, pred = pred, n_value = n_value)
}

select_sp_yr <- function(sp, year, data) {
  obs <- data[data$Species == sp & data$Year == year, ]$MgHa_obs
  pred <- data[data$Species == sp & data$Year == year, ]$MgHa_pred
  n_value <- nrow(data[data$Species == sp, ])
  list(obs = obs, pred = pred, n_value = n_value)
}

select_ts <- function(ts, data) {
  obs <- data[treatment == ts]$MgHa_obs
  pred <- data[treatment == ts]$MgHa_pred
  n_value <- nrow(data[treatment == ts])
  list(obs = obs, pred = pred, n_value = n_value)
}

select_all <- function(ts, sp, data) {
  if (year == "All Years") {
    obs <- data$MgHa_obs
    pred <- data$MgHa_pred
    n_value <- nrow(data)
  } else {
    dt <- data
    obs <- dt[Year == year]$MgHa_obs
    pred <- dt[Year == year]$MgHa_pred
    n_value <- nrow(dt[Year == year])
  }
  list(obs = obs, pred = pred, n_value = n_value)
}


# OLD RMSE and Bias --------------------------------------------------
#bias <- function(actual_i, predicted_i, n){
#  sum(actual_i - predicted_i)/n
#}

#rmse <- function(actual_i, predicted_i, n){
#  sqrt(sum(((actual_i - predicted_i)^2)/n))
#}

bias <- function(obs, pred) {mean(pred - obs)}
rmse <- function(obs, pred) {sqrt(mean((obs - pred)^2))}
rsquared <- function(obs, pred) {cor(obs, pred)^2} # spearman rank correlations? - cloud through points
rsquared <- function(obs, pred) {1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)}

#simultaneous F-test
sim_F <- function(obs, pred){
  # Calculate residuals (y_i)
  residuals <- obs - pred
  # Sample size (n)
  n <- length(obs)
  
  # Hypothesized coefficients
  b0 <- 0  # Intercept
  b1 <- 1  # Slope
  
  # Calculate numerator and denominator for the F-statistic
  numerator <- sum((residuals - b0)^2 + (residuals - b1)^2) / 2
  denominator <- sum((residuals - mean(residuals))^2) / (n - 2)
  
  # F-statistic  testing whether b0 = 0 and b1 = 1 simultaneously
  F_statistic <- numerator / denominator
  
  # Degrees of freedom
  df1 <- 2  # For b0 and b1
  df2 <- n - 2  # Residual degrees of freedom
  
  # P-value
  p_value <- pf(F_statistic, df1, df2, lower.tail = FALSE)
  return(data.frame(F_statistic = F_statistic, p_value = p_value))
  
}


# Cohen's D ------------------------------------------------------
#cohen_d <- function(obs, pred){
#  mean_diff <- mean(obs) - mean(pred)
#  pooled_sd <- sqrt((sd(obs)^2 + sd(pred)^2) / 2)
#  d <- mean_diff / pooled_sd
#  return(d)
#}
#effect size - small effect: d = 0.2, med effect: d = 0.5, large effect: d= 0.8
#positive means actual mean is greater than predicted


# Equivalence tests ----------------------------------------------
#equi_result <- function(obs, pred, eq_margin){
#  tost(x = obs, 
#       y = pred, 
#      epsilon = eq_margin,
#       paired = TRUE)
#}

# Bias TOST (paired)
#bias_tost <- function(obs, pred, eq_margin) {
# tost_result <- tost(
#    x = obs,
#   y = pred,
#  epsilon = eq_margin,
#   paired = TRUE
#  )
#  tost_result$tost.p.value
#}

#stat_functions <- list(
 # Bias = function(data) bias(data$obs, data$pred),
#  RMSE = function(data) rmse(data$obs, data$pred),
#  R_squared = function(data) rsquared(data$obs, data$pred),
 # cohenD = function(data) cohen_d(data$obs, data$pred),
#  f_test = function(data) sim_F(data$obs, data$pred)
  #equi <- function(data) equi_result(data$obs, data$pred, eq_margin = 0.5)
#)

# OLD equivalence with bootstrapping --------
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





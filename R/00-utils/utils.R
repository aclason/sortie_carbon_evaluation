
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
# RMSE and Bias --------------------------------------------------
#bias <- function(actual_i, predicted_i, n){
#  sum(actual_i - predicted_i)/n
#}

#rmse <- function(actual_i, predicted_i, n){
#  sqrt(sum(((actual_i - predicted_i)^2)/n))
#}

bias <- function(obs, pred) {mean(pred - obs)}
rmse <- function(obs, pred) {sqrt(mean((obs - pred)^2))}
rsquared <- function(obs, pred) {cor(obs, pred)^2}

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
# Example observed and predicted values
observed <- c(10, 12, 15, 18, 20)  # Replace with your data
predicted <- c(11, 12.5, 14.8, 17.5, 19.9)  # Replace with your model predictions




# Cohen's D ------------------------------------------------------
cohen_d <- function(obs, pred){
  mean_diff <- mean(obs) - mean(pred)
  pooled_sd <- sqrt((sd(obs)^2 + sd(pred)^2) / 2)
  d <- mean_diff / pooled_sd
  return(d)
}
#effect size - small effect: d = 0.2, med effect: d = 0.5, large effect: d= 0.8
#positive means actual mean is greater than predicted


# Equivalence tests ----------------------------------------------
equi_result <- function(obs, pred, eq_margin){
  tost(x = obs, 
       y = pred, 
       epsilon = eq_margin,
       paired = TRUE)
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

stat_functions <- list(
  Bias = function(data) bias(data$obs, data$pred),
  RMSE = function(data) rmse(data$obs, data$pred),
  R_squared = function(data) rsquared(data$obs, data$pred),
  cohenD = function(data) cohen_d(data$obs, data$pred),
  f_test = function(data) sim_F(data$obs, data$pred)
  #equi <- function(data) equi_result(data$obs, data$pred, eq_margin = 0.5)
)


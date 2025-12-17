library(ggplot2)
library(data.table)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
source(file.path("R","00-utils","utils.R"))


in_path <- "04_out_carbon"
out_path <- "05_out_analysis"

# Import data ------------------------------------------------------------------------------
# SBS ------
MSL_trees_sl <- readRDS(file.path(in_path,"MSL_trees_sl.RDS"))
FSL_trees_sl <- readRDS(file.path(in_path,"FSL_trees_sl.RDS"))

# ICH -------
MSL_trees_dc <- readRDS(file.path(in_path,"MSL_trees_dc.RDS"))
FSL_trees_dc <- readRDS(file.path(in_path,"FSL_trees_dc.RDS"))


# Table 1 convergence - linear models of difference between treatments over time ----------
# SBS ------
MSL_trees_sl[,`:=`(Unit = as.factor(Unit),
                   TSH = Year - 1992)]
model1 <- lmer(MgHa ~ Treatment * TSH + (1 | Unit), 
               data = MSL_trees_sl)
anova(model1)
#when do they converge - Post-hoc comparisons at each year
years_to_test <- unique(MSL_trees_sl$TSH)
results <- data.frame(Year = numeric(), 
                      heavy_no = numeric(),
                      heavy_med = numeric(),
                      light_med = numeric())

for (yr in years_to_test) {
  emmeans_res <- emmeans(model1, pairwise ~ Treatment | TSH, at = list(TSH = yr))
  hn <- summary(emmeans_res$contrasts)$p.value[1]  #heavy vs light/no
  hm <- summary(emmeans_res$contrasts)$p.value[2]  #heavy vs med
  lmd <- summary(emmeans_res$contrasts)$p.value[3]  #light/no vs.med
  results <- rbind(results, data.frame(Year = yr, heavy_no = hn,
                                       heavy_med = hm,
                                       light_med = lmd))
}
results_dt <- data.table(results)
setkey(results_dt, Year)

# Find the first year where p > 0.05
heavy_light_same <- results_dt[heavy_no > 0.05, min(Year)] 
heavy_med_same <- results_dt[heavy_med > 0.05, min(Year)] 
light_med_same <- results_dt[light_med > 0.05, min(Year)] 


# ICH -----
MSL_trees_dc[,`:=`(Unit = as.factor(Unit),
                   TSH = Year - 1992)]
model1 <- lmer(MgHa ~ Treatment * TSH + (1 | Unit), 
               data = MSL_trees_dc)
anova(model1)

#when do they converge - Post-hoc comparisons at each year
years_to_test <- unique(MSL_trees_dc$TSH)
results <- data.frame(Year = numeric(),
                      cc_hr = numeric(),
                      cc_lr = numeric(),
                      cc_nh = numeric(),
                      hr_lr = numeric(),
                      hr_nh = numeric(),
                      lr_nh = numeric())

for (yr in years_to_test) {
  emmeans_res <- emmeans(model1, pairwise ~ Treatment | TSH, at = list(TSH = yr))
  ch <- summary(emmeans_res$contrasts)$p.value[1]  #cc v hr
  cl <- summary(emmeans_res$contrasts)$p.value[2]  #
  cn <- summary(emmeans_res$contrasts)$p.value[3]  #
  hl <- summary(emmeans_res$contrasts)$p.value[4]  #
  hn <- summary(emmeans_res$contrasts)$p.value[5]  #
  ln <- summary(emmeans_res$contrasts)$p.value[6]  #
  results <- rbind(results, data.frame(Year = yr, cc_hr = ch,
                                       cc_lr = cl,
                                       cc_nh = cn,
                                       hr_lr = hl,
                                       hr_nh = hn,
                                       lr_nh = ln))
}
results_dt <- data.table(results)
setkey(results_dt, Year)
# Find the first year where p > 0.05
columns_to_check <- setdiff(colnames(results_dt), "Year")
lapply(columns_to_check, function(col) {
  results_dt[get(col) > 0.05, min(Year, na.rm = TRUE)]
})




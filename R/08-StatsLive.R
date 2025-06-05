# A.Clason
library(equivalence)
library(ggplot2)
library(data.table)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
source(file.path("R","00-utils","utils.R"))


in_path <- "04_out_carbon"
out_path <- "05_out_analysis"

# SBS -------------------------------------------------------------------------------------------
MSL_trees_sl <- readRDS(file.path(in_path,"MSL_trees_sl.RDS"))
FSL_trees_sl <- readRDS(file.path(in_path,"FSL_trees_sl.RDS"))

MSD_trees_sl <- readRDS(file.path(in_path,"MSD_trees_sl.RDS"))
FSD_trees_sl <- readRDS(file.path(in_path,"FSD_trees_sl.RDS"))

MSL_trees_sl_sp <- readRDS(file.path(in_path,"MSL_trees_sl_sp.RDS"))
FSL_trees_sl_sp <- readRDS(file.path(in_path,"FSL_trees_sl_sp.RDS"))
SL_preharvest <- fread(file.path("01_data","SL_preHarvest_BA.csv"))
setnames(SL_preharvest, "Plot","Unit")

MFL_trees_sl <- merge(FSL_trees_sl, MSL_trees_sl, by = c("Unit","Treatment","Year","State"),
                     all.x = TRUE)
setnames(MFL_trees_sl, c("MgHa.x","MgHa.y","BaHa.x","BaHa.y"), 
         c("MgHa_obs","MgHa_pred","BaHa_obs","BaHa_pred"))
MFD_trees_sl <- merge(FSD_trees_sl, MSD_trees_sl, by = c("Unit","Treatment","Year","State"),
                     all.x = TRUE)
setnames(MFD_trees_sl, c("MgHa.x","MgHa.y","BaHa.x","BaHa.y"), 
         c("MgHa_obs","MgHa_pred","BaHa_obs","BaHa_pred"))
MFD_trees_sl[is.na(MgHa_pred), `:=`(MgHa_pred = 0, BaHa_pred = 0)]#in 1992 - no dead in model

MF_trees_sl_sp <- merge(FSL_trees_sl_sp, MSL_trees_sl_sp, 
                        by = c("Unit","Treatment","Year","State","Species"),
                     all.x = TRUE)
setnames(MF_trees_sl_sp, c("MgHa.x","MgHa.y","BaHa.x","BaHa.y"), 
         c("MgHa_obs","MgHa_pred","BaHa_obs","BaHa_pred"))
MF_trees_sl_sp[is.na(MgHa_pred), MgHa_pred := 0]

# summary by treatment and year
MSL_trees_sl_sum_ty <- Rmisc::summarySE(data = MSL_trees_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Treatment","Year"))
MSL_trees_sl_sum_ty <- MSL_trees_sl_sum[MSL_trees_sl_sum$Year < 2093,]
FSL_trees_sl_sum_ty <- Rmisc::summarySE(data = FSL_trees_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Treatment","Year"))
# summary by year
MSL_trees_sl_sum <- Rmisc::summarySE(data = MSL_trees_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Year"))
FSL_trees_sl_sum <- Rmisc::summarySE(data = FSL_trees_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Year"))

# summary by treatment, year and species for Bl and Sx
MSL_trees_sl_sp_sum_blsx <- Rmisc::summarySE(MSL_trees_sl_sp[Species == "Bl" |
                                                          Species == "Sx" ],
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year", "Species"))
FSL_trees_sl_sp_sum_blsx <- Rmisc::summarySE(FSL_trees_sl_sp[Species == "Bl"|
                                                               Species == "Sx"],
                                             measurevar = "MgHa", 
                                             groupvars = c("Treatment","Year", "Species"))

# summary by treatment, year and species for all species
MSL_trees_sl_sp_sum <- Rmisc::summarySE(MSL_trees_sl_sp,
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year", "Species"))
MSL_trees_sl_sp_sum <- data.table(MSL_trees_sl_sp_sum)



# Figures -----------------------------------------------------------------------------------------
ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSL_trees_sl_sum_ty, aes(col = Treatment), size = 1.2) +
  scale_colour_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  coord_cartesian(ylim = c(0, 160),xlim = c(1990,2087)) +
  labs(
    x = "Year",
    y = "Live carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSL_trees_sl_sum_ty,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  geom_point(
    data = FSL_trees_sl,
    aes(shape = Treatment),
    size = 1 ,
    position = position_dodge(width = 3)
  )+
  scale_shape_manual(
    values = c(24, 23, 21),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  )+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "SBS_live_trees.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)

#SBS:
#Live carbon figure with carbon baseline added 
model_C_est <- lm(data = MFL_trees_sl, MgHa_obs ~ BaHa_obs)
#using the slope and intercept from the above linear model to estimate pre-harvest carbon
SL_baseline_fnct <- function(BasalArea) {-3.2 + 2.33 * BasalArea}

SL_preharvest[, MgHa_pre := SL_baseline_fnct(pre_harv_BA)]
LineValue_SL <- mean(SL_preharvest$MgHa_pre)

ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSL_trees_sl_sum_ty, aes(col = Treatment), size = 1.2) +
  #scale_x_continuous(breaks = seq(0, 28, by = 2), expand = expansion(mult = c(0, 0.05))) +
  scale_colour_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  coord_cartesian(ylim = c(0, 160), xlim = c(1990,2087)) +
  labs(
    x = "Year",
    y = "Live carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSL_trees_sl_sum_ty,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  geom_point(
    data = FSL_trees_sl,
    aes(shape = Treatment),
    size = 1 ,
    position = position_dodge(width = 3)
  )+
  scale_shape_manual(
    values = c(24, 23, 21),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  )+
  annotate("segment", x = 1992, xend = 2092, y = LineValue_SL, 
           yend = LineValue_SL, color = "gray", lty = 2, size = 1.2)+
  theme(legend.position = "bottom")

ggsave(filename = "SBS_live_trees_carbonRecovery.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)


ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Species)) +
  geom_line(data = MSL_trees_sl_sp_sum_blsx, aes(col = Treatment), size = 1.2) +
  scale_colour_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  coord_cartesian(ylim = c(0, 120)) +
  labs(
    x = "Year",
    y = "Carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSL_trees_sl_sp_sum_blsx,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  )+
  facet_grid(c("Species","Treatment"))+
  geom_point(
    data = FSL_trees_sl_sp_sum_blsx,
    aes(shape = Treatment),
    size = 3,
    position = position_dodge(width = 0.5)
  ) +
  geom_jitter(
    data = FSL_trees_sl_sp[Species == "Bl"|Species == "Sx"],
    aes(shape = Treatment),
    size = 1 ,
    position = position_dodge(width = 0.5)
  )+
  scale_shape_manual(
    values = c(24, 23, 21),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "SBS_live_trees_sp_yr.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)


sp_incl <- c("Sx","Bl")
ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSL_trees_sl_sp_sum[Species %in% sp_incl], 
            aes(col = Treatment), size = 1.8) +
  scale_colour_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  coord_cartesian(ylim = c(0, 120)) +
  labs(
    x = "Year",
    y = "Live carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment",
    linetype = "Treatment"
  ) +
  facet_wrap(c("Species"), 
             labeller = as_labeller(c("Bl" = "Subalpine fir",
                                      "Sx" = "Hybrid spruce")))+
  geom_jitter(
    data = FSL_trees_sl_sp[Species %in% sp_incl],
    aes(shape = Treatment),
    size = 2 ,
    position = position_dodge(width = 1)
  )+
  geom_line(
    data = MSL_trees_sl_sp[Species %in% sp_incl],
    aes(group = Unit, color = Treatment),
    alpha = 0.3,
    size = 0.8
  ) +
  scale_shape_manual(
    values = c(24, 23, 21),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  )+
  theme(legend.position = "bottom")+
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(size = 5)),
    shape = guide_legend(override.aes = list(size = 5)),
    linetype = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "SBS_live_trees_sp_tr_bl_sx.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)




#Stats -------------------------------------------------------------------------
#goodness of fit----------------------------------------------
years <- c(1992, 1994, 1997, 2009, 2019, "All Years")

results <- lapply(years, function(year) {
  data <- select_years(year,meas_obs = MgHa_obs, meas_pred = MgHa_pred,
                       data = MFL_trees_sl)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Year = years, results_df)
results_df
results <- as.data.table(lapply(results_df, function(col) {
  if (all(sapply(col, length) == 1)) unlist(col) else col
}))
results[, f_test := NULL]

lm(MgHa_pred ~ MgHa_obs, data = MFL_trees_sl)
t.test(MFL_trees_sl[Year == 2019]$MgHa_obs,
       MFL_trees_sl[Year == 2019]$MgHa_pred)

MFL_trees_summary <- MFL_trees_sl[, .(
  MgHa_pred_mean = mean(MgHa_pred, na.rm = TRUE),
  MgHa_pred_sd   = sd(MgHa_pred, na.rm = TRUE),
  MgHa_obs_mean  = mean(MgHa_obs, na.rm = TRUE),
  MgHa_obs_sd    = sd(MgHa_obs, na.rm = TRUE)
), by = .(Year)]
MFL_trees_summary <- merge(results[Year != "All Years",.(Year = as.numeric(Year), Bias, RMSE, R_squared)], 
                           MFL_trees_summary, by = c("Year"))
MFL_trees_summary[, per_bias := round((Bias/MgHa_obs_mean)*100,0)]
MFL_trees_summary[, Ecosystem := "SBS"]
final_table <- MFL_trees_summary[, .(
  Ecosystem,
  Year,
  `Bias – MgHa (% of mean)` = sprintf("%.2f (%.0f %%)", round(Bias, 2), per_bias),
  RMSE = round(RMSE, 2),
  R2 = round(R_squared, 2),
  `Mean ± (SD) predicted MgHa` = paste0(round(MgHa_pred_mean, 1),
                                        " ± (", round(MgHa_pred_sd, 1), ")"),
  `Mean ± (SD) observed MgHa` = paste0(round(MgHa_obs_mean, 1),
                                       " ± (", round(MgHa_obs_sd, 1), ")")
)]

# ---------------
fwrite(final_table, "Table 3_SBS.csv", encoding = "UTF-8")  
# ---------------


sp <- c("Bl","Sx")
results <- lapply(sp, function(sp) {
  data <- select_sp(sp, data = MF_trees_sl_sp)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df

results <- as.data.table(lapply(results, function(col) {
  if (all(sapply(col, length) == 1)) unlist(col) else col
}))
results[, f_test := NULL]
MFL_trees_summary <- MF_trees_sl_sp[, .(
  MgHa_pred_mean = mean(MgHa_pred, na.rm = TRUE),
  MgHa_pred_sd   = sd(MgHa_pred, na.rm = TRUE),
  MgHa_obs_mean  = mean(MgHa_obs, na.rm = TRUE),
  MgHa_obs_sd    = sd(MgHa_obs, na.rm = TRUE)
), by = .(Year, Species)]
MFL_trees_summary <- merge(results[,.(Species, Year, Bias, RMSE, R_squared)], 
                           MFL_trees_summary, by = c("Species","Year"))
MFL_trees_summary[, per_bias := round((Bias/MgHa_obs_mean)*100,0)]
MFL_trees_summary[, Ecosystem := "SBS"]
final_table <- MFL_trees_summary[, .(
  Ecosystem,
  Species,
  Year,
  `Bias – MgHa (% of mean)` = sprintf("%.2f (%.0f %%)", round(Bias, 2), per_bias),
  RMSE = round(RMSE, 2),
  R2 = round(R_squared, 2),
  `Mean ± (SD) predicted MgHa` = paste0(round(MgHa_pred_mean, 1),
                                        " ± (", round(MgHa_pred_sd, 1), ")"),
  `Mean ± (SD) observed MgHa` = paste0(round(MgHa_obs_mean, 1),
                                       " ± (", round(MgHa_obs_sd, 1), ")")
)]




t.test(MFL_trees_sl[Year == 2019]$MgHa_obs,
       MFL_trees_sl[Year == 2019]$MgHa_pred)
equi_result(MFL_trees_sl[Year == 2019]$MgHa_obs, 
            MFL_trees_sl[Year == 2019]$MgHa_pred, 7.6) # 10% of the mean observed

equivalence_bounds <- c(0.01,0.05, 0.10, 0.15, 0.18, 0.2, 0.25, 0.3,
                        0.35, 0.4, 0.45, 0.5, 0.55,0.6, 0.65)
 # Subset data for the specific year
  yr_subset <- MFL_trees_sl[Year == 2019, ]
  
  # Calculate the mean of observed values for the species
  mean_obs <- mean(yr_subset$MgHa_obs)
  
  # Loop through each equivalence bound and calculate the TOST p-value
  results_table <- data.table(
    Bound_Percentage = equivalence_bounds,
    Bound_Value = equivalence_bounds * mean_obs,
    TOST_p_value = sapply(equivalence_bounds, function(bound) {
      tost_result <- equi_result(
        yr_subset$MgHa_obs,
        yr_subset$MgHa_pred,
        mean_obs * bound
      )
      round(tost_result$tost.p.value, 2)
    })
  )

results_table


#species - year combo
sp <- c("Bl","Sx")
years <- c(1992, 1994, 1997, 2009, 2019)

select_sp_yr <- function(sp, year, data) {
  obs <- data[data$Species == sp & data$Year == year, ]$MgHa_obs
  pred <- data[data$Species == sp & data$Year == year, ]$MgHa_pred
  n_value <- nrow(data[data$Species == sp, ])
  list(obs = obs, pred = pred, n_value = n_value)
}

results <- do.call(rbind, lapply(sp, function(sp) {
  data.frame(do.call(rbind, lapply(years, function(year) {
    data <- select_sp_yr(sp, year, data = MF_trees_sl_sp)
    stats <- sapply(stat_functions, function(f) f(data))
    c(Species = sp, Year = year, stats)
  })))
}))
numeric_columns <- colnames(results)[-which(colnames(results) %in% c("Species"))] # All except "Species"
#results[numeric_columns] <- lapply(results[numeric_columns], as.numeric)
results <- as.data.table(lapply(results, function(col) {
  if (all(sapply(col, length) == 1)) unlist(col) else col
}))
results[, f_test := NULL]
MFL_trees_summary <- MF_trees_sl_sp[, .(
  MgHa_pred_mean = mean(MgHa_pred, na.rm = TRUE),
  MgHa_pred_sd   = sd(MgHa_pred, na.rm = TRUE),
  MgHa_obs_mean  = mean(MgHa_obs, na.rm = TRUE),
  MgHa_obs_sd    = sd(MgHa_obs, na.rm = TRUE)
), by = .(Year, Species)]
MFL_trees_summary <- merge(results[,.(Species, Year, Bias, RMSE, R_squared)], 
                           MFL_trees_summary, by = c("Species","Year"))
MFL_trees_summary[, per_bias := round((Bias/MgHa_obs_mean)*100,0)]
MFL_trees_summary[, Ecosystem := "SBS"]
final_table <- MFL_trees_summary[, .(
  Ecosystem,
  Species,
  Year,
  `Bias – MgHa (% of mean)` = sprintf("%.2f (%.0f %%)", round(Bias, 2), per_bias),
  RMSE = round(RMSE, 2),
  R2 = round(R_squared, 2),
  `Mean ± (SD) predicted MgHa` = paste0(round(MgHa_pred_mean, 1),
                                        " ± (", round(MgHa_pred_sd, 1), ")"),
  `Mean ± (SD) observed MgHa` = paste0(round(MgHa_obs_mean, 1),
                                       " ± (", round(MgHa_obs_sd, 1), ")")
)]

# ---------------
fwrite(final_table, "Table 3_SBS_sp.csv", encoding = "UTF-8")  
# ---------------
treatments <- unique(MF_trees_sl_sp$Treatment)
sp <- c("Bl","Sx")
#species - year - treatment
results <- lapply(sp, function(sp_i) {
  lapply(treatments, function(trt) {
    data <- select_sp(sp_i, data = MF_trees_sl_sp[Treatment == trt &
                                                    Year == 2019])
    
    stats <- sapply(stat_functions, function(f) f(data))
    
    data.frame(Species = sp_i, Treatment = trt, t(stats))
  })
})
results_df <- do.call(rbind, unlist(results, recursive = FALSE))
results_dt <- as.data.table(results_df)
results_dt






t.test(MF_trees_sl_sp[Year == 2019 & Species == "Bl"]$MgHa_obs,
       MF_trees_sl_sp[Year == 2019 & Species == "Bl"]$MgHa_pred)

t.test(MF_trees_sl_sp[Year == 2019 & Species == "Sx"]$MgHa_obs,
       MF_trees_sl_sp[Year == 2019 & Species == "Sx"]$MgHa_pred)

equi_result(MF_trees_sl_sp[Year == 2019 & Species == "Bl"]$MgHa_obs, 
            MF_trees_sl_sp[Year == 2019 & Species == "Bl"]$MgHa_pred, 5) # 10% of the mean observed

equi_result(MF_trees_sl_sp[Year == 2019 & Species == "Sx"]$MgHa_obs, 
            MF_trees_sl_sp[Year == 2019 & Species == "Sx"]$MgHa_pred, 2) # 10% of the mean observed


species_list <- sp
equivalence_bounds <- c(0.05, 0.10, 0.15, 0.18, 0.2, 0.25, 0.3,
                        0.35, 0.4, 0.45, 0.5, 0.55,0.6, 0.65)
mean_sp_obs <- c()
# Initialize a results table
results_table <- data.table(
  Species = character(),
  Bound_Percentage = equivalence_bounds,
  Bound_Value = equivalence_bounds * mean_sp_obs,
  TOST_p_value = numeric(length(equivalence_bounds))
)

for (species in species_list) {
  # Subset data for the specific year and species
  sp_yr_subset <- MF_trees_sl_sp[Year == 2019 & Species == species, ]
  
  # Calculate the mean of observed values for the species
  mean_sp_obs <- mean(sp_yr_subset$MgHa_obs)
  
  # Loop through each equivalence bound and calculate the TOST p-value
  for (bound in equivalence_bounds) {
    # Perform TOST
    tost_result <- equi_result(
      sp_yr_subset$MgHa_obs,
      sp_yr_subset$MgHa_pred,
      mean_sp_obs * bound
    )
    
    # Append the results to the results table
    results_table <- rbind(
      results_table,
      data.table(
        Species = species,
        Bound_Percentage = bound,
        Bound_Value = mean_sp_obs * bound,
        TOST_p_value = round(tost_result$tost.p.value,2)
      ),
      fill = TRUE
    )
  }
}
results_table



# linear models for effects of modelled treatment over time ---------------------
ggplot(MSL_trees_sl)+
  geom_bar(aes(x = Treatment))
  

MSL_trees_sl
MSL_trees_sl[,`:=`(Unit = as.factor(Unit),
                       TSH = Year - 1992)]
model1 <- lmer(MgHa ~ Treatment * TSH + (1 | Unit), 
               data = MSL_trees_sl)
anova(model1)
contrast(emmeans(model1, ~ Treatment | TSH), method = "tukey")

#when do they converge
# Post-hoc comparisons at each year
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
# Find the first year where p < 0.05
heavy_light_diff <- results_dt[heavy_no < 0.05, min(Year)] #0
heavy_med_diff <- results_dt[heavy_med < 0.05, min(Year)] #0
light_med_diff <- results_dt[light_med < 0.05, min(Year)] #0

# Find the first year where p >= 0.05
heavy_light_same <- results_dt[heavy_no >= 0.05, min(Year)] #
heavy_med_same <- results_dt[heavy_med >= 0.05, min(Year)] #
light_med_same <- results_dt[light_med >= 0.05, min(Year)] #



### Including species
MF_trees_sl_sp_m[,`:=`(Unit = as.factor(Unit),
                       TSH = Year - 1991)]
str(MF_trees_sl_sp_m)

anova(model1)
contrast(emmeans(model1, ~ Treatment), method = "tukey")
contrast(emmeans(model6, ~ Species | TSH), method = "tukey")

# this estimates the slope effects and contrasts:
emt <- emtrends(model1, tukey ~ Species, var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)

emt <- emtrends(model1, tukey ~ Species|obs_preds, var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)

#i still think Spruce isn't growing fast enough in the heavy removal. Tried a bunch of things - 
#go back to 05/06
#heavy vs light is significantly different for Bl, but not Sx, and same for med vs. light/no
mean(MSL_trees_sl[Year ==1992 & Treatment == "light/no"]$MgHa)
#59 MgHa at 1992
mean(MSL_trees_sl[Year == 2009 & Treatment == "med"]$MgHa)
#by 20 years after harvest, med treatment regained carbon
mean(MSL_trees_sl[Year == 2022 & Treatment == "heavy"]$MgHa)
#by 30 years after harvest, heavy treatment regained carbon

#just modelled - all 100 years
model1 <- lm(val_ha ~ Treatment * Species * TSH, dat_sl)
anova(model1)
contrast(emmeans(model5, ~ Treatment|Species), method = "tukey")
contrast(emmeans(model5, ~ Species | TSH), method = "tukey")

#at 100 years - what is the difference in carbon in treatments
summary(lm(val_ha ~ treatment-1, MSL_trees_sl[Year == 2091]))
anova(lm(val_ha ~ treatment-1, MSL_trees_sl[Year == 2091]))

ggplot()+
  geom_boxplot(aes(y = val_ha, x =treatment, fill = Treatment), 
               data = MSL_trees_sl[Year == 2091])
ggplot()+
  geom_boxplot(aes(y = val_ha, x =treatment, fill = Treatment), 
               data = MSL_trees_sl[Year == 2051])
ggplot()+
  geom_boxplot(aes(y = val_ha, x =treatment, fill = Treatment), 
               data = MSL_trees_sl[Year == 1992])
# after 100 years, the carbon in the no/light treatment remains significantly
# higher than med & heavy treatments, but the heavy and medium treatments are
# no longer significantly different? 
# looks like at about 50 years, the medium and heavy harvests merge



# ICH ----------------------------------------------------------------------------------------
MSL_trees_dc <- readRDS(file.path(in_path,"MSL_trees_dc.RDS"))
FSL_trees_dc <- readRDS(file.path(in_path,"FSL_trees_dc.RDS"))

MSL_trees_dc_sp <- readRDS(file.path(in_path,"MSL_trees_dc_sp.RDS"))
FSL_trees_dc_sp <- readRDS(file.path(in_path,"FSL_trees_dc_sp.RDS"))

MFL_trees_dc <- merge(FSL_trees_dc, MSL_trees_dc, by = c("Unit","Treatment","Year","State"),
                     all.x = TRUE)
setnames(MFL_trees_dc, c("MgHa.x","MgHa.y", "BAHa.x", "BAHa.y"),
         c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred"))

MF_trees_dc_sp <- merge(FSL_trees_dc_sp, MSL_trees_dc_sp, 
                        by = c("Unit","Treatment","Year","State","Species"),
                        all.x = TRUE)
setnames(MF_trees_dc_sp, c("MgHa.x","MgHa.y", "BAHa", "BaHa"),
         c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred"))
MF_trees_dc_sp[is.na(MgHa_obs)]

MF_trees_dc_m <- melt(MFL_trees_dc, 
                      id.vars = c("Unit", "Treatment", "Year", "State"),
                      measure.vars = c("MgHa_obs", "MgHa_pred", "BAHa_obs", "BAHa_pred"),
                      variable.name = "Type", 
                      value.name = "val_ha")
MF_trees_dc_m[, c("Measure", "Type") := tstrsplit(Type, "_")]
#MF_trees_dc_m[, obs_preds := ifelse(Type == "MgHa_obs" | Type == "BaHa_obs", "obs", "pred")]
#MF_trees_dc_m[, val_type := tstrsplit(Type, "_", fixed = TRUE)[[1]]]

MF_trees_dc_sp_m <- melt(MF_trees_dc_sp, 
                         id.vars = c("Unit", "Treatment", "Year", "State", "Species"),
                         measure.vars = c("MgHa_obs", "MgHa_pred", "BAHa_obs", "BAHa_pred"),
                         variable.name = "Type", 
                         value.name = "val_ha")

MF_trees_dc_sp_m[, obs_preds := ifelse(Type == "MgHa_obs" | Type == "BaHa_obs", "obs", "pred")]
MF_trees_dc_sp_m[, val_type := tstrsplit(Type, "_", fixed = TRUE)[[1]]]

#Add TASS projections to live carbon figure
TASS <- read.csv(file.path("01_data","TASS_carbon_projections_DateCreek.csv"))
TASS$MgHa <- rowMeans(TASS[, 2:5])
TASS$MgHa_OAFs <- rowMeans(TASS[, 6:9])
TASS$Year <- TASS$Age + 1992
TASS$Treatment <- rep("TASS", length(TASS$Age))
TASS$N <- rep(4, length(TASS$Age))
TASS$sd <- rep(NA, length(TASS$Age))
TASS$se <- rep(NA, length(TASS$Age))
TASS$ci <- rep(NA, length(TASS$Age))
#plot(TASS$Year, TASS$MgHa_OAFs)
#points(TASS$Year, TASS$MgHa)

MSL_trees_dc_sum <- Rmisc::summarySE(data = MSL_trees_dc, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Year", "Treatment"))

MSL_trees_dc_sp_sum <- Rmisc::summarySE(MSL_trees_dc_sp,
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year", "Species"))
MSL_trees_dc_sp_sum <- data.table(MSL_trees_dc_sp_sum)


MSL_trees_dc_sum_TASS <-
  rbind(MSL_trees_dc_sum, TASS[c("Year", "Treatment", "N", "MgHa",  "sd", "se", "ci")])
TASS_OAFs <-
  TASS[c("Year", "Treatment", "N", "MgHa_OAFs",  "sd", "se", "ci")]
TASS_OAFs$MgHa <- TASS_OAFs$MgHa_OAFs
TASS_OAFs$Treatment <- rep("TASS_OAFs", length(TASS_OAFs$Year))
MSL_trees_dc_sum_TASS <-
  rbind(MSL_trees_dc_sum_TASS, TASS_OAFs[c("Year", "Treatment", "N", "MgHa",  "sd", "se", "ci")])



## Figures -------------------------------------------

ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(Unit), 
                 color = Treatment), alpha = 0.9, 
             data = MFL_trees_dc)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  xlim(c(0,250))+
  ylim(c(0,250))+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )  +
  labs(
    x = "Carbon (Mg/ha) predicted",
    y = "Carbon (Mg/ha) observed",
    col = NULL
  ) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 12))+
  facet_wrap(~Year)+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "ICH_live_fit_yr.png", width = 7.91, height = 5.61,
       path = file.path(out_path, "Supplementary"), device='png', dpi=1200)



sp_incl <- c("Cw","Sx","Ba", "Pl")
ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSL_trees_dc_sp_sum[Species %in% sp_incl], 
            aes(col = Treatment), size = 1.8) +
  #scale_x_continuous(breaks = seq(0, 28, by = 2), expand = expansion(mult = c(0, 0.05))) +
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  scale_fill_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    x = "Year",
    y = "Live carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment",
    linetype = "Treatment"
  ) +
  facet_wrap(c("Species"), 
             # labeller = as_labeller(c("Ba" = "Amabilis fir",
             #                         "Bl" = "Subalpine fir",
             #                        "Sx" = "Hybrid spruce",
             #                       "Pl" = "Lodgepole pine",
             #                      "Cw" = "Western cedar",
             #                     "Hw" = "Western hemlock"))
             labeller = as_labeller(c("Ba" = "Amabilis fir",
                                      "Cw" = "Western cedar",
                                      "Pl" = "Lodgepole pine",
                                      "Sx" = "Hybrid spruce")))+
  geom_jitter(
    data = FSL_trees_dc_sp[Species %in% sp_incl],
    aes(shape = Treatment),
    size = 2 ,
    position = position_dodge(width = 1)
  )+
  geom_line(
    data = MSL_trees_dc_sp[Species %in% sp_incl],
    aes(group = Unit, color = Treatment),
    alpha = 0.3,
    size = 0.8
  ) +
  scale_shape_manual(
    values = c(24, 23, 21, 25),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  #scale_linetype_manual(
   # values = c("twodash","solid", "dashed", "dotted"),
  #  breaks = c("NH","LR", "HR", "CC"),
  #  labels = c("No harvest","High retention", "Medium retention", "No retention")
  #)+
  theme(legend.position = "bottom")+
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(size = 5)),
    shape = guide_legend(override.aes = list(size = 5)),
    linetype = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "ICH_live_trees_sp_tr_cw_sx_pl_ba.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)

sp_incl <- c("Hw")
ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSL_trees_dc_sp_sum[Species %in% sp_incl], 
            aes(col = Treatment), size = 1.8) +
  #scale_x_continuous(breaks = seq(0, 28, by = 2), expand = expansion(mult = c(0, 0.05))) +
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  scale_fill_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  coord_cartesian(ylim = c(0, 225)) +
  labs(
    x = "Year",
    y = "Live carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment",
    linetype = "Treatment"
  ) +
  facet_wrap(c("Species"), 
             # labeller = as_labeller(c("Ba" = "Amabilis fir",
             #                         "Bl" = "Subalpine fir",
             #                        "Sx" = "Hybrid spruce",
             #                       "Pl" = "Lodgepole pine",
             #                      "Cw" = "Western cedar",
             #                     "Hw" = "Western hemlock"))
             labeller = as_labeller(c("Hw" = "Western hemlock")))+
  geom_jitter(
    data = FSL_trees_dc_sp[Species %in% sp_incl],
    aes(shape = Treatment),
    size = 2 ,
    position = position_dodge(width = 1)
  )+
  geom_line(
    data = MSL_trees_dc_sp[Species %in% sp_incl],
    aes(group = Unit, color = Treatment),
    alpha = 0.3,
    size = 0.8
  ) +
  scale_shape_manual(
    values = c(24, 23, 21, 25),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  scale_linetype_manual(
    values = c("twodash","solid", "dashed", "dotted"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  theme(legend.position = "bottom")+
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(size = 5)),
    shape = guide_legend(override.aes = list(size = 5)),
    linetype = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "ICH_live_trees_sp_tr_hw.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)



ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSL_trees_dc_sum, aes(col = Treatment), size = 1.2) +
  #scale_x_continuous(breaks = seq(0, 28, by = 2), expand = expansion(mult = c(0, 0.05))) +
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  scale_fill_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  coord_cartesian(ylim = c(0, 250)) +
  labs(
    x = "Year",
    y = "Live carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSL_trees_dc_sum,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  #geom_point(
  #  data = FSL_trees_sl_sum,
  #  aes(shape = Treatment),
  #  size = 5 ,
  #  position = position_dodge(width = 3)
  #) +
  geom_point(
    data = FSL_trees_dc,
    aes(shape = Treatment),
    size = 1 ,
    position = position_dodge(width = 3)
  )+
  #geom_line(
  #  data = MSL_trees_sl,
  # aes(group = unit, color = treatment),
  # alpha = 0.7,
  # linetype = "dotted"
  #color = "grey"
  # ) +
  scale_shape_manual(
    values = c(24, 23, 21, 25),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave(filename = "ICH_live_trees.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)


#ICH -------------------------------------------------
LineValue_DC <- mean(RecoveryLine$MgHa)

ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSL_trees_dc_sum, aes(col = Treatment), size = 1.2) +
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  scale_fill_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  coord_cartesian(ylim = c(0, 250)) +
  labs(
    x = "Year",
    y = "Live carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSL_trees_dc_sum,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  geom_point(
    data = FSL_trees_dc,
    aes(shape = Treatment),
    size = 1 ,
    position = position_dodge(width = 3)
  )+
  scale_shape_manual(
    values = c(24, 23, 21, 25),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  annotate("segment", x = 1992, xend = 2092, y = LineValue_DC, 
           yend = LineValue_DC, color = "gray", lty = 2, size = 1.2)+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave(filename = "ICH_live_trees_carbonRecovery.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)


ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSL_trees_dc_sum_TASS, aes(col = Treatment), size = 1.2) +
  #geom_line(data = TASS,  aes(x = Year, y = MgHa, col = "gray"), lty = 2)+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444", "grey", "black"),
    breaks = c("NH","LR", "HR", "CC", "TASS", "TASS_OAFs"),
    labels = c("No harvest","High retention", "Medium retention", "No retention", "TASS", "TASS_OAFs")
  ) +
  scale_fill_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444", "grey", "black"),
    breaks = c("NH","LR", "HR", "CC", "TASS", "TASS_OAFs"),
    labels = c("No harvest","High retention", "Medium retention", "No retention", "TASS", "TASS_OAFs")
  ) +
  coord_cartesian(ylim = c(0, 250), xlim = c(1999, 2092)) +
  labs(
    x = "Year",
    y = "Live carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSL_trees_dc_sum,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  geom_point(
    data = FSL_trees_dc,
    aes(shape = Treatment),
    size = 1 ,
    position = position_dodge(width = 3)
  )+
  scale_shape_manual(values = c(24, 23, 21, 25, 26),
                     breaks = c("NH","LR", "HR", "CC", "TASS"), 
                     labels = c("No harvest","High retention", "Medium retention", "No retention",
                                "TASS"))+
  annotate("segment", x = 1992, xend = 2092, y = LineValue_DC, 
           yend = LineValue_DC, color = "gray", lty = 2, size = 1.2)+
  #theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave(filename = "ICH_live_trees_TASS.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)


#Stats -------------------------------------------------

# Goodness of fit
years <- c(1992, 1993, 2010, 2018, 2022, "All Years")

results <- lapply(years, function(year) {
  data <- select_years(year,meas_obs = MgHa_obs, meas_pred = MgHa_pred,
                       data = MFL_trees_dc)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Year = years, results_df)
results_df
results <- as.data.table(lapply(results_df, function(col) {
  if (all(sapply(col, length) == 1)) unlist(col) else col
}))
results[, f_test := NULL]

#is there a significant difference between predicted and observed by 2022?
equi_result(MFL_trees_dc[Year == 2022]$MgHa_obs, 
            MFL_trees_dc[Year == 2022]$MgHa_pred, 14) # 10% of the mean observed
equi_boot(MFL_trees_dc$MgHa_obs,
          MFL_trees_dc$MgHa_pred, n_bootstraps = 10000, eq_margin = 14)
t.test(FSL_trees_dc[Year == 2022]$MgHa, 
            MSL_trees_dc[Year == 2022]$MgHa) #not diff
summary(lm(MgHa_pred ~ MgHa_obs, MFL_trees_dc))

equivalence_bounds <- c(0.01,0.05, 0.10, 0.15, 0.18, 0.2, 0.25, 0.3,
                        0.35, 0.4, 0.45, 0.5, 0.55,0.6, 0.65)
# Subset data for the specific year
yr_subset <- MFL_trees_dc[Year == 2022, ]

# Calculate the mean of observed values for the species
mean_obs <- mean(yr_subset$MgHa_obs)

# Loop through each equivalence bound and calculate the TOST p-value
results_table <- data.table(
  Bound_Percentage = equivalence_bounds,
  Bound_Value = equivalence_bounds * mean_obs,
  TOST_p_value = sapply(equivalence_bounds, function(bound) {
    tost_result <- equi_result(
      yr_subset$MgHa_obs,
      yr_subset$MgHa_pred,
      mean_obs * bound
    )
    round(tost_result$tost.p.value, 2)
  })
)

results_table

MFL_trees_summary <- MFL_trees_dc[, .(
  MgHa_pred_mean = mean(MgHa_pred, na.rm = TRUE),
  MgHa_pred_sd   = sd(MgHa_pred, na.rm = TRUE),
  MgHa_obs_mean  = mean(MgHa_obs, na.rm = TRUE),
  MgHa_obs_sd    = sd(MgHa_obs, na.rm = TRUE)
), by = .(Year)]
MFL_trees_summary <- merge(results[Year != "All Years",.(Year = as.numeric(Year), Bias, RMSE, R_squared)], 
                           MFL_trees_summary, by = c("Year"))
MFL_trees_summary[, per_bias := round((Bias/MgHa_obs_mean)*100,0)]
MFL_trees_summary[, Ecosystem := "ICH"]
final_table <- MFL_trees_summary[, .(
  Ecosystem,
  Year,
  `Bias – MgHa (% of mean)` = sprintf("%.2f (%.0f %%)", round(Bias, 2), per_bias),
  RMSE = round(RMSE, 2),
  R2 = round(R_squared, 2),
  `Mean ± (SD) predicted MgHa` = paste0(round(MgHa_pred_mean, 1),
                                        " ± (", round(MgHa_pred_sd, 1), ")"),
  `Mean ± (SD) observed MgHa` = paste0(round(MgHa_obs_mean, 1),
                                       " ± (", round(MgHa_obs_sd, 1), ")")
)]

# ---------------
fwrite(final_table, "Table 3_ICH.csv", encoding = "UTF-8")  
# ---------------





#by species ---------------------------------------------------------------
#species - year combo
sp <- c("Hw","Cw","Ba","Sx","Pl")
years <- c(1992, 1993, 2010, 2018, 2022)

select_sp_yr <- function(sp, year, data) {
  obs <- data[data$Species == sp & data$Year == year, ]$MgHa_obs
  pred <- data[data$Species == sp & data$Year == year, ]$MgHa_pred
  n_value <- nrow(data[data$Species == sp, ])
  list(obs = obs, pred = pred, n_value = n_value)
}


results <- do.call(rbind, lapply(sp, function(sp) {
  data.frame(do.call(rbind, lapply(years, function(year) {
    data <- select_sp_yr(sp, year, data = MF_trees_dc_sp)
    stats <- sapply(stat_functions, function(f) f(data))
    c(Species = sp, Year = year, stats)
  })))
}))
#numeric_columns <- colnames(results)[-which(colnames(results) %in% c("Species"))] # All except "Species"
#results[numeric_columns] <- lapply(results[numeric_columns], as.numeric)
results <- as.data.table(lapply(results, function(col) {
  if (all(sapply(col, length) == 1)) unlist(col) else col
}))
results[, f_test := NULL]
MFL_trees_summary <- MF_trees_dc_sp[, .(
  MgHa_pred_mean = mean(MgHa_pred, na.rm = TRUE),
  MgHa_pred_sd   = sd(MgHa_pred, na.rm = TRUE),
  MgHa_obs_mean  = mean(MgHa_obs, na.rm = TRUE),
  MgHa_obs_sd    = sd(MgHa_obs, na.rm = TRUE)
), by = .(Year, Species)]

MFL_trees_summary <- merge(results[,.(Species, Year, Bias, RMSE, R_squared)], 
                           MFL_trees_summary, by = c("Species","Year"))
MFL_trees_summary[, per_bias := round((Bias/MgHa_obs_mean)*100,0)]
MFL_trees_summary[, Ecosystem := "ICH"]
final_table <- MFL_trees_summary[, .(
  Ecosystem,
  Species,
  Year,
  `Bias – MgHa (% of mean)` = sprintf("%.2f (%.0f %%)", round(Bias, 2), per_bias),
  RMSE = round(RMSE, 2),
  R2 = round(R_squared, 2),
  `Mean ± (SD) predicted MgHa` = paste0(round(MgHa_pred_mean, 1),
                                        " ± (", round(MgHa_pred_sd, 1), ")"),
  `Mean ± (SD) observed MgHa` = paste0(round(MgHa_obs_mean, 1),
                                       " ± (", round(MgHa_obs_sd, 1), ")")
)]

# ---------------
fwrite(final_table, "Table 3_ICH_sp.csv", encoding = "UTF-8")  
# ---------------







t.test(MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$MgHa_obs,
       MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$MgHa_pred)

t.test(MF_trees_dc_sp[Year == 2022 & Species == "Cw"]$MgHa_obs,
       MF_trees_dc_sp[Year == 2022 & Species == "Cw"]$MgHa_pred)

t.test(MF_trees_dc_sp[Year == 2022 & Species == "Sx"]$MgHa_obs,
       MF_trees_dc_sp[Year == 2022 & Species == "Sx"]$MgHa_pred)

t.test(MF_trees_dc_sp[Year == 2022 & Species == "Pl"]$MgHa_obs,
       MF_trees_dc_sp[Year == 2022 & Species == "Pl"]$MgHa_pred)

t.test(MF_trees_dc_sp[Year == 2022 & Species == "Ba"]$MgHa_obs,
       MF_trees_dc_sp[Year == 2022 & Species == "Ba"]$MgHa_pred)

#Hemlock
equi_result(MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$MgHa_obs, 
            MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$MgHa_pred, 9) # 10% of the mean observed

equi_result(MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$MgHa_obs, 
            MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$MgHa_pred, 
            mean(MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$MgHa_obs)*0.05)
equi_result(MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$MgHa_obs, 
            MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$MgHa_pred, 
            mean(MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$MgHa_obs)*0.1)
equi_result(MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$MgHa_obs, 
            MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$MgHa_pred, 
            mean(MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$MgHa_obs)*0.15)



equi_result(MF_trees_dc_sp[Year == 2022 & Species == "Cw"]$MgHa_obs, 
            MF_trees_dc_sp[Year == 2022 & Species == "Cw"]$MgHa_pred, 3) # 10% of the mean observed


equi_result(MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$MgHa_obs, 
            MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$MgHa_pred, 9) # 10% of the mean observed

equi_result(MF_trees_dc_sp[Year == 2022 & Species == "Cw"]$MgHa_obs, 
            MF_trees_dc_sp[Year == 2022 & Species == "Cw"]$MgHa_pred, 3) # 10% of the mean observed

species_list <- sp
sp_yr_subset <- MF_trees_dc_sp[Year == 2022 & Species == "Hw", ]

# Define the equivalence bounds as percentages of the mean observed value
equivalence_bounds <- c(0.05, 0.10, 0.15, 0.17, 0.2, 0.25, 0.3,
                        0.35, 0.4, 0.45, 0.5, 0.55,0.6, 0.65)

# Initialize a results table
#results_table <- data.table(
 # Species = character(),
#  Bound_Percentage = equivalence_bounds,
#  Bound_Value = equivalence_bounds * mean_sp_obs,
#  TOST_p_value = numeric(length(equivalence_bounds))
#)
results <- c()
for (species in species_list) {
  # Subset data for the specific year and species
  sp_yr_subset <- MF_trees_dc_sp[Year == 2022 & Species == species, ]
  
  # Calculate the mean of observed values for the species
  mean_sp_obs <- mean(sp_yr_subset$MgHa_obs)
  
  # Loop through each equivalence bound and calculate the TOST p-value
  for (bound in equivalence_bounds) {
    # Perform TOST
    tost_result <- equi_result(
      sp_yr_subset$MgHa_obs,
      sp_yr_subset$MgHa_pred,
      mean_sp_obs * bound
    )
    
    # Append the results to the results table
    results_table <- rbind(
      results_table,
      data.table(
        Species = species,
        Bound_Percentage = bound,
        Bound_Value = mean_sp_obs * bound,
        TOST_p_value = round(tost_result$tost.p.value,2)
      ),
      fill = TRUE
    )
  }
}
results_table

MFL_trees_summary <- MF_trees_dc_sp %>%
  filter(Treatment == "CC" & Species == "Pl")%>%
  group_by(Year, Species) %>%
  summarise(
    MgHa_obs_mean = mean(MgHa_obs, na.rm = TRUE),
    MgHa_obs_sd   = sd(MgHa_obs, na.rm = TRUE),
    MgHa_pred_mean = mean(MgHa_pred, na.rm = TRUE),
    MgHa_pred_sd   = sd(MgHa_pred, na.rm = TRUE)
  )%>% 
  mutate(across(where(is.numeric), ~ format(., nsmall = 1)))

t.test(MF_trees_dc_sp[Year == 2022 & Species == "Pl" & Treatment == "CC"]$MgHa_obs,
       MF_trees_dc_sp[Year == 2022 & Species == "Pl" & Treatment == "CC"]$MgHa_pred)

sp <- c("Pl")
years <- c(1992, 1993, 2010, 2018, 2022)

results <- do.call(rbind, lapply(sp, function(sp) {
  data.frame(do.call(rbind, lapply(years, function(year) {
    data <- select_sp_yr(sp, year, data = MF_trees_dc_sp[Treatment == "CC"])
    stats <- sapply(stat_functions, function(f) f(data))
    c(Species = sp, Year = year, stats)
  })))
}))
numeric_columns <- colnames(results)[-which(colnames(results) %in% c("Species"))] # All except "Species"
results[numeric_columns] <- lapply(results[numeric_columns], as.numeric)




# linear models for effects of modelled treatment over time ---------------------
MSL_trees_dc
MSL_trees_dc[,`:=`(Unit = as.factor(Unit),
                   TSH = Year - 1992)]
model1 <- lmer(MgHa ~ Treatment * TSH + (1 | Unit), 
               data = MSL_trees_dc)
anova(model1)
contrast(emmeans(model1, ~ Treatment | TSH), method = "tukey")

#when do they converge
# Post-hoc comparisons at each year
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
# Find the first year where p < 0.05
columns_to_check <- setdiff(colnames(results_dt), "Year")
lapply(columns_to_check, function(col) {
  results_dt[get(col) >= 0.05, min(Year, na.rm = TRUE)]
})

# Find the first year where p >= 0.05
heavy_light_same <- results_dt[heavy_no >= 0.05, min(Year)]
heavy_med_same <- results_dt[heavy_med >= 0.05, min(Year)]
light_med_same <- results_dt[light_med >= 0.05, min(Year)]



#just modelled - all 100 years
MSL_trees_dc[,`:=`(Unit = as.factor(Unit),
                    TSH = Year - 1992)]
MSL_trees_dc_sp[,`:=`(Unit = as.factor(Unit),
                      TSH = Year - 1992)]
dat_dc <- MSL_trees_dc[TSH > 0]

model1 <- lm(MgHa ~ Treatment * Species * TSH, dat_dc)
anova(model1)

contrast(emmeans(model1, ~ Treatment|Species), method = "tukey")
contrast(emmeans(model1, ~ Species | TSH), method = "tukey")

#at 100 years - what is the difference in carbon in treatments
summary(lm(MgHa ~ Treatment-1, dat_dc[Year == 2092]))
anova(lm(MgHa ~ Treatment-1, dat_dc[Year == 2092]))

ggplot()+
  geom_boxplot(aes(y = val_ha, x =treatment, fill = Treatment), 
               data = MSL_trees_sl[Year == 2091])
ggplot()+
  geom_boxplot(aes(y = val_ha, x =treatment, fill = Treatment), 
               data = MSL_trees_sl[Year == 2051])
ggplot()+
  geom_boxplot(aes(y = val_ha, x =treatment, fill = Treatment), 
               data = MSL_trees_sl[Year == 1992])
# after 100 years, the carbon in the no/light treatment remains significantly
# higher than med & heavy treatments, but the heavy and medium treatments are
# no longer significantly different? 
# looks like at about 50 years, the medium and heavy harvests merge

MSL_trees_sl
MSL_trees_dc

MSL_trees_sl[Year == 2091,.(mn_carb = mean(MgHa),
                sd_carb = sd(MgHa)), by = Treatment]

MSL_trees_dc[Year == 2092,.(mn_carb = mean(MgHa),
                           sd_carb = sd(MgHa)), by = Treatment]











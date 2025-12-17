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


MSD_trees_sl <- readRDS(file.path(in_path,"MSD_trees_sl.RDS"))
FSD_trees_sl <- readRDS(file.path(in_path,"FSD_trees_sl.RDS"))


# SBS -------------------------------------------------------------------------------------------
MFD_trees_sl <- merge(FSD_trees_sl, MSD_trees_sl, by = c("Unit","Treatment","Year","State"),
                      all.x = TRUE)
setnames(MFD_trees_sl, c("MgHa.x","MgHa.y","BaHa.x","BaHa.y"), 
         c("MgHa_obs","MgHa_pred","BaHa_obs","BaHa_pred"))
MFD_trees_sl[is.na(MgHa_pred), `:=`(MgHa_pred = 0, BaHa_pred = 0)]#in 1992 - no dead in model

MFD_trees_sl <- merge(FSD_trees_sl, MSD_trees_sl, by = c("Unit","Treatment","Year","State"),
                     all.x = TRUE)
MFD_trees_sl[is.na(MFD_trees_sl)] <- 0
setnames(MFD_trees_sl, c("MgHa.x","MgHa.y"), c("MgHa_obs","MgHa_pred"))

MFD_trees_sl_m <- melt(MFD_trees_sl, 
                      id.vars = c("Unit", "Treatment", "Year", "State"),
                      measure.vars = c("MgHa_obs", "MgHa_pred"),
                      variable.name = "Type", 
                      value.name = "MgHa")
MFD_trees_sl_m[, Type := ifelse(Type == "MgHa_obs", "obs", "pred")]

ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(Unit), 
                 color = Treatment), 
             alpha = 0.9,
             size = 4,
             data = MFD_trees_sl)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  coord_cartesian() +
  labs(
    x = "Dead carbon (Mg·ha⁻¹) predicted",
    y = "Dead carbon (Mg·ha⁻¹) observed",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,35))+
  ylim(c(0,35))+
  theme(legend.position = "bottom")+
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
    guides(color = guide_legend(override.aes = list(size = 5)))

ggsave(filename = "SBS_dead_fit.png", width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)

ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(Unit), 
                 color = Treatment), 
             alpha = 0.9,
             size = 2,
             data = MFD_trees_sl)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  coord_cartesian() +
  labs(
    x = "Carbon (Mg·ha⁻¹) predicted",
    y = "Carbon (Mg·ha⁻¹) observed",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,35))+
  ylim(c(0,35))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  facet_wrap(~Year)

ggsave(filename = "SBS_dead_fit_yr.png",
       path = file.path(out_path, "Supplementary"), device='png', dpi=1200)

MSD_trees_sl_sum <- Rmisc::summarySE(data = MSD_trees_sl[Year <2091], 
                                     measurevar = "MgHa", 
                                     groupvars = c("Treatment","Year"),
                                     na.rm = TRUE)
MSD_trees_sl_sum$Treatment <- factor(MSD_trees_sl_sum$Treatment, 
                                     levels = c("light/no", "med", "heavy"))
FSD_trees_sl$Treatment <- factor(FSD_trees_sl$Treatment, 
                                     levels = c("light/no", "med", "heavy"))


ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSD_trees_sl_sum, aes(col = Treatment), size = 1.2) +
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
  coord_cartesian(ylim = c(0, 35)) +
  labs(
    x = "Year",
    y = "Carbon (Mg·ha⁻¹)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSD_trees_sl_sum,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
 # facet_wrap(~Treatment)+
  geom_jitter(
    data = FSD_trees_sl,
    aes(shape = Treatment),
    size = 1 ,
    position = position_dodge(width = 3)
  )+
  scale_shape_manual(
    values = c(24, 23, 21),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  )+
  facet_wrap(~Treatment)+
  theme(legend.position = "bottom", strip.text = element_blank())+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))

ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSD_trees_sl_sum, aes(col = Treatment), size = 1.2) +
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
  coord_cartesian(ylim = c(0, 35)) +
  labs(
    x = "Year",
    y = "Dead standing carbon (Mg·ha⁻¹)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSD_trees_sl_sum,
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
    data = FSD_trees_sl,
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
    values = c(24, 23, 21),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  )+
  #xlim(c(1990, 2022))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave(filename = "SBS_dead_tr_yr_overtime.png",plot = last_plot(), width = 7.91, height = 5.61,
       units = "in", path = file.path(out_path), device='png', dpi=1200)

  #FSD_trees_sl_sp_sum <- Rmisc::summarySE(FSD_trees_sl_sp[Species == "Bl"|
 #                                                         Species == "Sx"],
  #                                      measurevar = "MgUnit", 
   #                                     groupvars = c("treatment","Year", "Species"))

#MSD_trees_sl_sp_sum <- Rmisc::summarySE(MSD_trees_sl_sp[Species == "Bl" & Year < 2092|
 #                                                         Species == "Sx"& Year < 2092],
  #                                      measurevar = "MgUnit", 
   #                                     groupvars = c("treatment","Year", "Species"))
#MSD_trees_sl_sp_sum <- data.table(MSD_trees_sl_sp_sum)



#Stats -------------------------------------------------------------------------
years <- c(1997, 2009, 2019, "All Years")

results <- lapply(years, function(year) {
  data <- select_years(year,meas_obs = MgHa_obs, meas_pred = MgHa_pred,
                       data = MFD_trees_sl)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Year = years, results_df)
results_df
results <- as.data.table(lapply(results_df, function(col) {
  if (all(sapply(col, length) == 1)) unlist(col) else col
}))
results[, f_test := NULL]

t.test(MFD_trees_sl[Year == 2019]$MgHa_obs,
       MFD_trees_sl[Year == 2019]$MgHa_pred)

equi_result(MFD_trees_sl[Year == 2019]$MgHa_obs, 
            MFD_trees_sl[Year == 2019]$MgHa_pred, 0.5) # 10% of the mean observed

tost(x = MFD_trees_sl[Year == 2019]$MgHa_obs, 
     y = MFD_trees_sl[Year == 2019]$MgHa_pred,
     epsilon = 0.5,
     paired = FALSE,
     conf.level = 0.95) # 10% of the mean observed


MFD_trees_summary <- MFD_trees_sl %>%
  group_by(Year) %>%
  summarise(
    MgHa_obs_mean = mean(MgHa_obs, na.rm = TRUE),
    MgHa_obs_sd   = sd(MgHa_obs, na.rm = TRUE),
    MgHa_pred_mean = mean(MgHa_pred, na.rm = TRUE),
    MgHa_pred_sd   = sd(MgHa_pred, na.rm = TRUE)
  )

MSD_trees_sl[, TSH := ifelse(Unit == 4, Year - 1994,
                            ifelse(Unit == 15, Year - 1994,
                                   Year - 1992))]

# Define the equivalence bounds as percentages of the mean observed value
equivalence_bounds <- c(0.05, 0.10, 0.15, 0.17, 0.2, 0.25, 0.3,
                        0.35, 0.4, 0.45, 0.5, 0.55,0.6, 0.65)

# Subset data for the specific year
yr_subset <- MFD_trees_sl[Year == 2019, ]

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


MFD_trees_summary <- merge(results[Year != "All Years",.(Year = as.numeric(Year), Bias, RMSE, R_squared)], 
                           MFD_trees_summary, by = c("Year"))
MFD_trees_summary[, per_bias := round((Bias/MgHa_obs_mean)*100,0)]
MFD_trees_summary[, Ecosystem := "SBS"]
final_table <- MFD_trees_summary[, .(
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
final_table  
# ---------------





# ICH ----------------------------------------------------------------------------------------
MSD_trees_dc <- readRDS(file.path(in_path,"MSD_trees_dc.RDS"))
FSD_trees_dc <- readRDS(file.path(in_path,"FSD_trees_dc.RDS")) 
FSD_trees_dc[, State := "Dead"]

MFD_trees_dc <- merge(FSD_trees_dc, MSD_trees_dc, by = c("Unit","Year","State"),
                      all.x = TRUE)
setnames(MFD_trees_dc, c("MgHa.x","MgHa.y", "BAHa.x", "BAHa.y"),
         c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred"))

MF_trees_dc_m <- melt(MFD_trees_dc, 
                      id.vars = c("Unit", "Treatment", "Year", "State"),
                      measure.vars = c("MgHa_obs", "MgHa_pred", "BAHa_obs", "BAHa_pred"),
                      variable.name = "Type", 
                      value.name = "val_ha")
MF_trees_dc_m[, c("Measure", "Type") := tstrsplit(Type, "_")]

MSD_trees_dc_sum <- Rmisc::summarySE(data = MSD_trees_dc, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Treatment","Year"),
                                     na.rm = TRUE)
FSD_trees_dc <- merge(FSD_trees_dc, DateCreekData::Treatments, by = "Unit")

MSD_trees_dc_sum$Treatment <- factor(MSD_trees_dc_sum$Treatment, 
                                     levels = c("NH","LR", "HR", "CC"))
FSD_trees_dc$Treatment <- factor(FSD_trees_dc$Treatment, 
                                 levels = c("NH","LR", "HR", "CC"))


# Carbon predicted vs observed ----------
ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, 
                 color = Treatment), 
             alpha = 0.9,
             size = 4,
             data = MFD_trees_dc)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  coord_cartesian() +
  labs(
    x = "Dead carbon (Mg·ha⁻¹) predicted",
    y = "Dead carbon (Mg·ha⁻¹) observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,70))+
  ylim(c(0,70))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))#+
ggsave(filename = "ICH_dead_fit.png",plot = last_plot(), width = 7.91, height = 5.61,
       units = "in", path = file.path(out_path), device='png', dpi=1200)

#Dead tree carbon over time ---------------
ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSD_trees_dc_sum, aes(col = Treatment), size = 1.2) +
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
  coord_cartesian(ylim = c(0, 70)) +
  labs(
    x = "Year",
    y = "Dead standing carbon (Mg·ha⁻¹)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSD_trees_dc_sum,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  geom_point(
    data = FSD_trees_dc,
    aes(shape = Treatment),
    size = 1 ,
    position = position_dodge(width = 3)
  )+
  geom_line(
    data = MSD_trees_dc,
   aes(group = Unit, color = Treatment),
   alpha = 0.7,
   linetype = "dotted",
  color = "grey"
   ) +
  scale_shape_manual(
    values = c(24, 23, 21, 25),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))


ggsave(filename = "ICH_dead_tr_yr_overtime.png",plot = last_plot(), width = 7.91, height = 5.61,
       units = "in", path = file.path(out_path), device='png', dpi=1200)

#Stats -------------------------------------------------------------------------
years <- c(1992, 1993, 2010, 2018, 2022, "All Years")

results <- lapply(years, function(year) {
  data <- select_years(year,meas_obs = MgHa_obs, meas_pred = MgHa_pred,
                       data = MFD_trees_dc)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Year = years, results_df)
results_df
results <- as.data.table(lapply(results_df, function(col) {
  if (all(sapply(col, length) == 1)) unlist(col) else col
}))
results[, f_test := NULL]

t.test(MFD_trees_dc[Year == 2022]$MgHa_obs,
       MFD_trees_dc[Year == 2022]$MgHa_pred)

tost(x = MFD_trees_dc[Year == 2022]$MgHa_obs, 
     y = MFD_trees_dc[Year == 2022]$MgHa_pred,
     epsilon = 0.9,
     paired = FALSE,
     conf.level = 0.95) # 10% of the mean observed

MFD_trees_summary <- MFD_trees_dc %>%
  group_by(Year) %>%
  summarise(
    MgHa_obs_mean = mean(MgHa_obs, na.rm = TRUE),
    MgHa_obs_sd   = sd(MgHa_obs, na.rm = TRUE),
    MgHa_pred_mean = mean(MgHa_pred, na.rm = TRUE),
    MgHa_pred_sd   = sd(MgHa_pred, na.rm = TRUE)
  )

MSD_trees_sl[, TSH := ifelse(Unit == 4, Year - 1994,
                             ifelse(Unit == 15, Year - 1994,
                                    Year - 1992))]
# Define the equivalence bounds as percentages of the mean observed value
equivalence_bounds <- c(0.05, 0.10, 0.15, 0.17, 0.2, 0.25, 0.3,
                        0.35, 0.4, 0.45, 0.5, 0.55,0.6, 0.65)

# Subset data for the specific year
yr_subset <- MFD_trees_sl[Year == 2019, ]

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

MFD_trees_summary <- merge(results[Year != "All Years",.(Year = as.numeric(Year), Bias, RMSE, R_squared)], 
                           MFD_trees_summary, by = c("Year"))
MFD_trees_summary[, per_bias := round((Bias/MgHa_obs_mean)*100,0)]
MFD_trees_summary[, Ecosystem := "ICH"]
final_table <- MFD_trees_summary[, .(
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
final_table
# ---------------


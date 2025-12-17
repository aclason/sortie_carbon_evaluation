# A.Clason
library(equivalence)
library(ggplot2)
library(data.table)
library(dplyr)
source(file.path("R","00-utils","utils.R"))

in_path <- "04_out_carbon"
out_path <- "05_out_analysis"

# data -----------------------------------------------------------------------------------------
#SBS
MSL_trees_sl <- readRDS(file.path(in_path,"MSL_trees_sl.RDS"))
FSL_trees_sl <- readRDS(file.path(in_path,"FSL_trees_sl.RDS"))

MSL_trees_sl_sp <- readRDS(file.path(in_path,"MSL_trees_sl_sp.RDS"))
FSL_trees_sl_sp <- readRDS(file.path(in_path,"FSL_trees_sl_sp.RDS"))

#ICH
MSL_trees_dc <- readRDS(file.path(in_path,"MSL_trees_dc.RDS"))
FSL_trees_dc <- readRDS(file.path(in_path,"FSL_trees_dc.RDS"))

MSL_trees_dc_sp <- readRDS(file.path(in_path,"MSL_trees_dc_sp.RDS"))
FSL_trees_dc_sp <- readRDS(file.path(in_path,"FSL_trees_dc_sp.RDS"))


# SBS -------------------------------------------------------------------------------------------
MFL_trees_sl <- merge(FSL_trees_sl, MSL_trees_sl, by = c("Unit","Treatment","Year","State"),
                     all.x = TRUE)
setnames(MFL_trees_sl, c("MgHa.x","MgHa.y","BaHa.x","BaHa.y"), 
         c("MgHa_obs","MgHa_pred","BaHa_obs","BaHa_pred"))

MF_trees_sl_sp <- merge(FSL_trees_sl_sp, MSL_trees_sl_sp, 
                        by = c("Unit","Treatment","Year","State","Species"),
                     all.x = TRUE)
setnames(MF_trees_sl_sp, c("MgHa.x","MgHa.y","BaHa.x","BaHa.y"), 
         c("MgHa_obs","MgHa_pred","BaHa_obs","BaHa_pred"))
MF_trees_sl_sp[is.na(MgHa_pred), MgHa_pred := 0]
# summary by treatment, year and species for all species
MSL_trees_sl_sp_sum <- Rmisc::summarySE(MSL_trees_sl_sp,
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year", "Species"))
MSL_trees_sl_sp_sum <- data.table(MSL_trees_sl_sp_sum)



MFL_trees_dc <- merge(FSL_trees_dc, MSL_trees_dc, by = c("Unit","Treatment","Year","State"),
                      all.x = TRUE)
setnames(MFL_trees_dc, c("MgHa.x","MgHa.y", "BAHa.x", "BAHa.y"),
         c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred"))

MF_trees_dc_sp <- merge(FSL_trees_dc_sp, MSL_trees_dc_sp, 
                        by = c("Unit","Treatment","Year","State","Species"),
                        all.x = TRUE)
setnames(MF_trees_dc_sp, c("MgHa.x","MgHa.y", "BAHa", "BaHa"),
         c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred"))

MSL_trees_dc_sp_sum <- Rmisc::summarySE(MSL_trees_dc_sp,
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year", "Species"))
MSL_trees_dc_sp_sum <- data.table(MSL_trees_dc_sp_sum)



# Figures -----------------------------------------------------------------------------------------
# Figure 3A -----
ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(Unit), 
                 color = Treatment), alpha = 0.9, size =4, 
             data = MFL_trees_sl)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  xlim(c(0,130))+
  ylim(c(0,130))+
  scale_colour_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  )  +
  labs(
    x = "Live carbon (Mg·ha⁻¹) predicted",
    y = "Live carbon (Mg·ha⁻¹) observed",
    col = NULL
  ) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 12))+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "SBS_live_fit.png", width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)

#Figure 4 -------
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
# Table 2 bias, RMSE, R2, Means -----------------------
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
final_table #Table 2 Live Carbon - SBS
# ---------------
# Table 2 T-test and TOST on final year -----------------------
t.test(MFL_trees_sl[Year == 2019]$MgHa_obs,
       MFL_trees_sl[Year == 2019]$MgHa_pred)

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

# Table 3 bias, RMSE, R2, Means -----------------------
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
final_table #Table 3 - SBS live by species
# ---------------

# Table 3 T-test and TOST on final year -----------------------
t.test(MF_trees_sl_sp[Year == 2019 & Species == "Bl"]$MgHa_obs,
       MF_trees_sl_sp[Year == 2019 & Species == "Bl"]$MgHa_pred)

t.test(MF_trees_sl_sp[Year == 2019 & Species == "Sx"]$MgHa_obs,
       MF_trees_sl_sp[Year == 2019 & Species == "Sx"]$MgHa_pred)

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


# ICH ----------------------------------------------------------------------------------------
## Figures -------------------------------------------
# Figure 1B
ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(Unit), 
                 color = Treatment), alpha = 0.9, size =4,
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
    x = "Live carbon (Mg·ha⁻¹) predicted",
    y = "Live carbon (Mg·ha⁻¹) observed",
    col = NULL
  ) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 14))+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "ICH_live_fit.png", width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)

#Figure 5
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


# Stats ----
# Table 2 bias, RMSE, R2, Means ---------------
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
final_table #Table 2 - Live trees
# ---------------

# Table 2 T-test and TOST on final year ----------------
t.test(FSL_trees_dc[Year == 2022]$MgHa, 
            MSL_trees_dc[Year == 2022]$MgHa) #not diff

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

#by species ---------------------------------------------------------------
# Table 3 bias, RMSE, R2, Means ---------------
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
final_table #Table 3 - ICH by species  
# ---------------

# Table 2 T-test and TOST on final year ----------------
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

# Define the equivalence bounds as percentages of the mean observed value
species_list <- sp
equivalence_bounds <- c(0.05, 0.10, 0.15, 0.17, 0.2, 0.25, 0.3,
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


# Supplementary Materials 3: Table S2-1 (pine in clearcuts)
sp <- c("Pl")
years <- c(1992, 1993, 2010, 2018, 2022)

results <- do.call(rbind, lapply(sp, function(sp) {
  data.frame(do.call(rbind, lapply(years, function(year) {
    data <- select_sp_yr(sp, year, data = MF_trees_dc_sp[Treatment == "CC"])
    stats <- sapply(stat_functions, function(f) f(data))
    c(Species = sp, Year = year, stats)
  })))
}))
results <- as.data.table(lapply(results, function(col) {
  if (all(sapply(col, length) == 1)) unlist(col) else col
}))
results[,f_test := NULL]

MFL_trees_summary <- MF_trees_dc_sp %>%
  filter(Treatment == "CC" & Species == "Pl")%>%
  group_by(Year, Species) %>%
  summarise(
    MgHa_obs_mean = mean(MgHa_obs, na.rm = TRUE),
    MgHa_obs_sd   = sd(MgHa_obs, na.rm = TRUE),
    MgHa_pred_mean = mean(MgHa_pred, na.rm = TRUE),
    MgHa_pred_sd   = sd(MgHa_pred, na.rm = TRUE)
  )
MFL_trees_summary <- as.data.table(MFL_trees_summary)
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

final_table
t.test(MF_trees_dc_sp[Year == 2022 & Species == "Pl" & Treatment == "CC"]$MgHa_obs,
       MF_trees_dc_sp[Year == 2022 & Species == "Pl" & Treatment == "CC"]$MgHa_pred)






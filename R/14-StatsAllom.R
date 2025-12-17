library(ggplot2)
library(data.table)
library(dplyr)
source(file.path("R","00-utils","utils.R"))

in_path <- "04_out_carbon"
out_path <- "05_out_analysis"

F_trees_sl <- readRDS(file.path(in_path,"F_trees_sl.RDS")) #need to re-create these
F_trees_sl <- merge(F_trees_sl, SummitLakeData::Treatments, by = "unit")
setnames(F_trees_sl, c("unit","treatment"), c("Unit","Treatment"))
F_trees_dc <- readRDS(file.path(in_path,"F_trees_dc.RDS"))

# SBS -------------------------------------------------------------------------------------------
#(Appendix A1:height by species and treatment)
F_trees_sl[, Treatment := factor(Treatment, 
                                 levels = c("light/no", "med", "heavy"))]

ggplot()+
  geom_point(aes(x = Height, y = meas_hgt, 
                 color = Treatment), 
             alpha = 0.8,
             size = 2,
             data = F_trees_sl[!is.na(meas_hgt) & Class <3][Species == "Bl"|Species == "Sx"])+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  coord_cartesian() +
  labs(
    x = "Height predicted (m) ",
    y = "Height observed (m)",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  scale_color_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  )+
  xlim(c(0,40))+
  ylim(c(0,40))+
  theme(legend.position = "none")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  facet_grid(c("Treatment","Species"),
             labeller = labeller(Species = c("Bl" = "Subalpine fir", "Sx" = "Spruce"),
                                 Treatment = c("light/no" = "High retention",
                                               "med" = "Medium retention",
                                               "heavy" = "Low retention")))
ggsave(filename = "Hgt_sl_pred_obs.png",plot = last_plot(),
       units = "in", path = file.path(out_path,"Supplementary"), device='png', dpi=1200)
##stats to go with the above:
select_sp_h <- function(sp, data) {
  obs <- data[Species == sp]$meas_hgt
  pred <- data[Species == sp]$Height
  n_value <- nrow(data[Species == sp])
  list(obs = obs, pred = pred, n_value = n_value)
}
n_vals <-  F_trees_sl[!is.na(meas_hgt) & Class <3, .N, by = "Species"]
sp <- unique(F_trees_sl[!is.na(meas_hgt) & Class <3]$Species)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data =  F_trees_sl[!is.na(meas_hgt) & Class <3])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
merge(results_df, n_vals, by.x = "Species", by.y = "Species")

rsquared(F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Bl" & Treatment == "light/no"]$meas_hgt,
         F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Bl"& Treatment == "light/no"]$Height)
rsquared(F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Bl" & Treatment == "med"]$meas_hgt,
         F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Bl"& Treatment == "med"]$Height)
rsquared(F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Bl" & Treatment == "heavy"]$meas_hgt,
         F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Bl"& Treatment == "heavy"]$Height)

rsquared(F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Sx" & Treatment == "light/no"]$meas_hgt,
         F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Sx"& Treatment == "light/no"]$Height)
rsquared(F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Sx" & Treatment == "med"]$meas_hgt,
         F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Sx"& Treatment == "med"]$Height)
rsquared(F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Sx" & Treatment == "heavy"]$meas_hgt,
         F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Sx"& Treatment == "heavy"]$Height)

# mean and descriptive statistics for basal area
years <- c(1992, 1994, 1997, 2009, 2019, "All Years")

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
                                        measurevar = "BaHa", 
                                        groupvars = c("Treatment","Year"))
MSL_trees_sl_sum_ty <- MSL_trees_sl_sum_ty[MSL_trees_sl_sum_ty$Year < 2093,]
FSL_trees_sl_sum_ty <- Rmisc::summarySE(data = FSL_trees_sl, 
                                        measurevar = "BaHa", 
                                        groupvars = c("Treatment","Year"))
# summary by year
MSL_trees_sl_sum <- Rmisc::summarySE(data = MSL_trees_sl, 
                                     measurevar = "BaHa", 
                                     groupvars = c("Year"))
FSL_trees_sl_sum <- Rmisc::summarySE(data = FSL_trees_sl, 
                                     measurevar = "BaHa", 
                                     groupvars = c("Year"))

# summary by treatment, year and species for Bl and Sx
MSL_trees_sl_sp_sum_blsx <- Rmisc::summarySE(MSL_trees_sl_sp[Species == "Bl" |
                                                               Species == "Sx" ],
                                             measurevar = "BaHa", 
                                             groupvars = c("Treatment","Year", "Species"))
FSL_trees_sl_sp_sum_blsx <- Rmisc::summarySE(FSL_trees_sl_sp[Species == "Bl"|
                                                               Species == "Sx"],
                                             measurevar = "BaHa", 
                                             groupvars = c("Treatment","Year", "Species"))

# summary by treatment, year and species for all species
MSL_trees_sl_sp_sum <- Rmisc::summarySE(MSL_trees_sl_sp,
                                        measurevar = "BaHa", 
                                        groupvars = c("Treatment","Year", "Species"))
MSL_trees_sl_sp_sum <- data.table(MSL_trees_sl_sp_sum)

results <- lapply(years, function(year) {
  data <- select_years(year,meas_obs = BAHa_obs, meas_pred = BAHa_pred,
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

t.test(MFL_trees_sl[Year == 2019]$BAHa_obs,
       MFL_trees_sl[Year == 2019]$BAHa_pred)

MFL_trees_summary <- MFL_trees_sl[, .(
  BaHa_pred_mean = mean(BAHa_pred, na.rm = TRUE),
  BaHa_pred_sd   = sd(BAHa_pred, na.rm = TRUE),
  BaHa_obs_mean  = mean(BAHa_obs, na.rm = TRUE),
  BaHa_obs_sd    = sd(BAHa_obs, na.rm = TRUE)
), by = .(Year)]
MFL_trees_summary <- merge(results[Year != "All Years",.(Year = as.numeric(Year), Bias, RMSE, R_squared)], 
                           MFL_trees_summary, by = "Year")
MFL_trees_summary[, per_bias := round((Bias/BaHa_obs_mean)*100,0)]
MFL_trees_summary[, Ecosystem := "SBS"]
final_table <- MFL_trees_summary[, .(
  Ecosystem,
  Year,
  `Bias – MgHa (% of mean)` = sprintf("%.2f (%.0f %%)", round(Bias, 2), per_bias),
  RMSE = round(RMSE, 2),
  R2 = round(R_squared, 2),
  `Mean ± (SD) predicted BAHa` = paste0(round(BaHa_pred_mean, 1),
                                        " ± (", round(BaHa_pred_sd, 1), ")"),
  `Mean ± (SD) observed BAHa` = paste0(round(BaHa_obs_mean, 1),
                                       " ± (", round(BaHa_obs_sd, 1), ")")
)]

# ---------------
final_table 
# ---------------

#species - year combo
sp <- c("Bl","Sx")
years <- c(1992, 1994, 1997, 2009, 2019)
MF_trees_sl_sp[is.na(BaHa_pred), BaHa_pred := 0]

select_sp_yr <- function(sp, year, data) {
  obs <- data[data$Species == sp & data$Year == year, ]$BaHa_obs
  pred <- data[data$Species == sp & data$Year == year, ]$BaHa_pred
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
  BaHa_pred_mean = mean(BaHa_pred, na.rm = TRUE),
  BaHa_pred_sd   = sd(BaHa_pred, na.rm = TRUE),
  BaHa_obs_mean  = mean(BaHa_obs, na.rm = TRUE),
  BaHa_obs_sd    = sd(BaHa_obs, na.rm = TRUE)
), by = .(Year, Species)]
MFL_trees_summary <- merge(results[,.(Species, Year, Bias, RMSE, R_squared)], 
                           MFL_trees_summary, by = c("Species","Year"))
MFL_trees_summary[, per_bias := round((Bias/BaHa_obs_mean)*100,0)]
MFL_trees_summary[, Ecosystem := "SBS"]
final_table <- MFL_trees_summary[, .(
  Ecosystem,
  Species,
  Year,
  `Bias – BAHa (% of mean)` = sprintf("%.2f (%.0f %%)", round(Bias, 2), per_bias),
  RMSE = round(RMSE, 2),
  R2 = round(R_squared, 2),
  `Mean ± (SD) predicted BAHa` = paste0(round(BaHa_pred_mean, 1),
                                        " ± (", round(BaHa_pred_sd, 1), ")"),
  `Mean ± (SD) observed BAHa` = paste0(round(BaHa_obs_mean, 1),
                                       " ± (", round(BaHa_obs_sd, 1), ")")
)]

# ---------------
final_table
# ---------------
t.test(MF_trees_sl_sp[Year == 2019 & Species == "Bl"]$BaHa_obs,
       MF_trees_sl_sp[Year == 2019 & Species == "Bl"]$BaHa_pred)

t.test(MF_trees_sl_sp[Year == 2019 & Species == "Sx"]$BaHa_obs,
       MF_trees_sl_sp[Year == 2019 & Species == "Sx"]$BaHa_pred)



# ICH -------------------------------------------------------------------------------------------

ggplot(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" & Tree.Class <3 & Spp == "Pl"])+
  geom_point(aes(x = Height, y = cruise_hgt))

F_trees_dc[, Treatment := factor(Treatment, 
                                 levels = c("NH", "LR", "HR","CC"))]
#with the plantation and residual allom applied:
ggplot()+
  geom_point(aes(x = Height, y = cruise_hgt, 
                 color = Treatment), 
             alpha = 0.4,
             size = 2,
             data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" & Tree.Class <3][Spp == "Hw"|
                                                                                     Spp == "Cw"|
                                                                                     Spp == "Pl"|
                                                                                     Spp == "Ba"|
                                                                                     Spp == "Sx"])+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  coord_cartesian() +
  labs(
    x = "Height predicted (m) ",
    y = "Height observed (m)",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,50))+
  ylim(c(0,50))+
  theme(legend.position = "none")+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  #guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
 # guides(color = guide_legend(override.aes = list(size = 5)))+
  facet_grid(c("Treatment","Spp"),
             labeller = labeller(Spp = c("Hw" = "Hemlock",
                                             "Sx" = "Spruce",
                                             "Pl" = "Pine",
                                             "Cw" = "Cedar",
                                             "Ba" = "Fir"),
                                 Treatment = c("NH" = "No harvest",
                                                "LR" = "High retention",
                                               "HR" = "Medium retention",
                                               "CC" = "No retention")))
ggsave(filename = "Hgt_dc_pred_obs_adj.png",plot = last_plot(),
       units = "in", path = file.path(out_path,"Supplementary"), device='png', dpi=1200)

#stats to go with the above:
select_sp_h <- function(sp, data) {
  obs <- data[Spp == sp]$cruise_hgt
  pred <- data[Spp == sp]$Height
  n_value <- nrow(data[Spp == sp])
  list(obs = obs, pred = pred, n_value = n_value)
}

#CC
n_vals <- F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                     & Tree.Class <3 & Treatment == "CC", .N, by = "Spp"]
sp <- unique(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                        & Tree.Class <3 & Treatment == "CC"]$Spp)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                                            & Tree.Class <3 & Treatment == "CC"])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df



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

MF_trees_dc_sp_m <- melt(MF_trees_dc_sp, 
                         id.vars = c("Unit", "Treatment", "Year", "State", "Species"),
                         measure.vars = c("MgHa_obs", "MgHa_pred", "BAHa_obs", "BAHa_pred"),
                         variable.name = "Type", 
                         value.name = "val_ha")

MF_trees_dc_sp_m[, obs_preds := ifelse(Type == "MgHa_obs" | Type == "BaHa_obs", "obs", "pred")]
MF_trees_dc_sp_m[, val_type := tstrsplit(Type, "_", fixed = TRUE)[[1]]]

MSL_trees_dc_sum <- Rmisc::summarySE(data = MSL_trees_dc, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Year", "Treatment"))

MSL_trees_dc_sp_sum <- Rmisc::summarySE(MSL_trees_dc_sp,
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year", "Species"))
MSL_trees_dc_sp_sum <- data.table(MSL_trees_dc_sp_sum)



# Goodness of fit
years <- c(1992, 1993, 2010, 2018, 2022, "All Years")

results <- lapply(years, function(year) {
  data <- select_years(year,meas_obs = BAHa_obs, meas_pred = BAHa_pred,
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
t.test(FSL_trees_dc[Year == 2022]$BAHa, 
       MSL_trees_dc[Year == 2022]$BAHa) #not diff
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
  BaHa_pred_mean = mean(BAHa_pred, na.rm = TRUE),
  BaHa_pred_sd   = sd(BAHa_pred, na.rm = TRUE),
  BaHa_obs_mean  = mean(BAHa_obs, na.rm = TRUE),
  BaHa_obs_sd    = sd(BAHa_obs, na.rm = TRUE)
), by = .(Year)]
MFL_trees_summary <- merge(results[Year != "All Years",.(Year = as.numeric(Year), Bias, RMSE, R_squared)], 
                           MFL_trees_summary, by = c("Year"))
MFL_trees_summary[, per_bias := round((Bias/BaHa_obs_mean)*100,0)]
MFL_trees_summary[, Ecosystem := "ICH"]
final_table <- MFL_trees_summary[, .(
  Ecosystem,
  Year,
  `Bias – MgHa (% of mean)` = sprintf("%.2f (%.0f %%)", round(Bias, 2), per_bias),
  RMSE = round(RMSE, 2),
  R2 = round(R_squared, 2),
  `Mean ± (SD) predicted MgHa` = paste0(round(BaHa_pred_mean, 1),
                                        " ± (", round(BaHa_pred_sd, 1), ")"),
  `Mean ± (SD) observed MgHa` = paste0(round(BaHa_obs_mean, 1),
                                       " ± (", round(BaHa_obs_sd, 1), ")")
)]

# ---------------
fwrite(final_table, "Table 3_ICH.csv", encoding = "UTF-8")  
# ---------------

#species - year combo
sp <- c("Hw","Cw","Ba","Sx","Pl")
years <- c(1992, 1993, 2010, 2018, 2022)

select_sp_yr <- function(sp, year, data) {
  obs <- data[data$Species == sp & data$Year == year, ]$BAHa_obs
  pred <- data[data$Species == sp & data$Year == year, ]$BAHa_pred
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
results <- as.data.table(lapply(results, function(col) {
  if (all(sapply(col, length) == 1)) unlist(col) else col
}))
results[, f_test := NULL]
MFL_trees_summary <- MF_trees_dc_sp[, .(
  BaHa_pred_mean = mean(BAHa_pred, na.rm = TRUE),
  BaHa_pred_sd   = sd(BAHa_pred, na.rm = TRUE),
  BaHa_obs_mean  = mean(BAHa_obs, na.rm = TRUE),
  BaHa_obs_sd    = sd(BAHa_obs, na.rm = TRUE)
), by = .(Year, Species)]

MFL_trees_summary <- merge(results[,.(Species, Year, Bias, RMSE, R_squared)], 
                           MFL_trees_summary, by = c("Species","Year"))
MFL_trees_summary[, per_bias := round((Bias/BaHa_obs_mean)*100,0)]
MFL_trees_summary[, Ecosystem := "ICH"]
final_table <- MFL_trees_summary[, .(
  Ecosystem,
  Species,
  Year,
  `Bias – MgHa (% of mean)` = sprintf("%.2f (%.0f %%)", round(Bias, 2), per_bias),
  RMSE = round(RMSE, 2),
  R2 = round(R_squared, 2),
  `Mean ± (SD) predicted MgHa` = paste0(round(BaHa_pred_mean, 1),
                                        " ± (", round(BaHa_pred_sd, 1), ")"),
  `Mean ± (SD) observed MgHa` = paste0(round(BaHa_obs_mean, 1),
                                       " ± (", round(BaHa_obs_sd, 1), ")")
)]

# ---------------
final_table 
# ---------------
t.test(MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$BAHa_obs,
       MF_trees_dc_sp[Year == 2022 & Species == "Hw"]$BAHa_pred)

t.test(MF_trees_dc_sp[Year == 2022 & Species == "Cw"]$BAHa_obs,
       MF_trees_dc_sp[Year == 2022 & Species == "Cw"]$BAHa_pred)

t.test(MF_trees_dc_sp[Year == 2022 & Species == "Sx"]$BAHa_obs,
       MF_trees_dc_sp[Year == 2022 & Species == "Sx"]$BAHa_pred)

t.test(MF_trees_dc_sp[Year == 2022 & Species == "Pl"]$BAHa_obs,
       MF_trees_dc_sp[Year == 2022 & Species == "Pl"]$BAHa_pred)

t.test(MF_trees_dc_sp[Year == 2022 & Species == "Ba"]$BAHa_obs,
       MF_trees_dc_sp[Year == 2022 & Species == "Ba"]$BAHa_pred)




#unadjusted:
tree_dt_92 <- DateCreekData::trees_1992(cruise_data = "D:/Github/DateCreekData/data-raw/Trees/1992data.csv",
                                        fixed_data = "D:/Github/DateCreekData/data-raw/Trees/1992fixed_radius_data_fromTable20_DateCkHandbook.csv",
                                        calc_height = TRUE)
tree_dt_92 <- tree_dt_92[SPH != 0]
tree_dt_93 <- DateCreekData::trees_1993(data =  "D:/Github/DateCreekData/data-raw/Trees/SS93forR.csv",
                                        sm_tree = "D:/Github/DateCreekData/data-raw/Trees/1993_under1.3m_tree_tallies.csv",
                                        calc_height = TRUE,
                                        tree_cl_stub = "8") #tree class 8 considered stubin 1993

tree_dt_10_no <- DateCreekData::trees_2010(lrg_trees = "D:/Github/DateCreekData/data-raw/Trees/Data Creek 2010 Data large trees.csv",
                                        cc_trees = "D:/Github/DateCreekData/data-raw/Trees/Trees 10cm and above in clearcut 2010.csv",
                                        small_trees = "D:/Github/DateCreekData/data-raw/Trees/Date Creek 2010 Trees less than 10 cm tallies.csv",
                                        snag_heights = "D:/Github/DateCreekData/data-raw/Trees/SnagHeights2010.csv",
                                        measured_heights2022 = "D:/Github/DateCreekData/data-raw/Trees/StandStructureData_Nov2022_Final.xlsx",
                                        calc_height = TRUE,
                                        use_standard = TRUE, #TRUE = standard, FALSE = residual 
                                        id_gap_trees = FALSE, #use gaps to trigger plantation allom
                                        use_size = FALSE, #use tree size to trigger plantation allom
                                        use_cc = FALSE) #use cc to trigger plantation allom


tree_dt_18 <- DateCreekData::trees_201x(data_file = "D:/Github/DateCreekData/data-raw/Trees/Date Creek 2018 Data large trees_re-entered.xlsx",
                                        data_2018 = "DataCk re-entry 2018 largeTrees",
                                        data_2019 = "D:/Github/DateCreekData/data-raw/Trees/Data Creek 2019 Data large trees.csv",
                                        inter_trees = "D:/Github/DateCreekData/data-raw/Trees/2018-19intermediatetrees.csv",
                                        small_trees = "D:/Github/DateCreekData/data-raw/Trees/Small trees 2018 2019 KHP.csv",
                                        lrg_trees_2010 = "D:/Github/DateCreekData/data-raw/Trees/Data Creek 2010 Data large trees.csv",
                                        measured_heights2022 = "D:/Github/DateCreekData/data-raw/Trees/StandStructureData_Nov2022_Final.xlsx",
                                        snag_hgts_18 = "D:/Github/DateCreekData/data-raw/Trees/SnagHeights2018.csv",
                                        calc_height = TRUE,
                                        use_standard = TRUE, #TRUE = standard, FALSE = residual
                                        id_gap_trees = FALSE, #use gaps to trigger plantation allom
                                        use_size = FALSE, #use tree size to trigger plantation allom
                                        use_cc = FALSE)#use cc to trigger plantation allom

tree_dt_22 <- DateCreekData::trees_2022(data_file = "D:/Github/DateCreekData/data-raw/Trees/StandStructureData_Nov2022_Final.xlsx",
                                        large_trees = "Large",
                                        inter_trees = "Inter",
                                        small_trees = "Small",
                                        calc_height = TRUE)
F_trees_dc_no <- rbind(tree_dt_92, tree_dt_93, tree_dt_10, tree_dt_18, tree_dt_22, fill = TRUE)
F_trees_dc_no <- merge(F_trees_dc_no, DateCreekData::Treatments)

F_trees_dc_no[, Treatment := factor(Treatment, 
                                 levels = c("NH", "LR", "HR","CC"))]
ggplot(tree_dt_10_no[!is.na(cruise_hgt) & StubYN == "N" & Tree.Class <3 & Spp == "Pl"])+
  geom_point(aes(x = Height, y = cruise_hgt))

#with the plantation and residual allom applied:
ggplot()+
  geom_point(aes(x = Height, y = cruise_hgt, 
                 color = Treatment), 
             alpha = 0.4,
             size = 2,
             data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" & Tree.Class <3][Spp == "Hw"|
                                                                                     Spp == "Cw"|
                                                                                     Spp == "Pl"|
                                                                                     Spp == "Ba"|
                                                                                     Spp == "Sx"])+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  coord_cartesian() +
  labs(
    x = "Height predicted (m) ",
    y = "Height observed (m)",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,50))+
  ylim(c(0,50))+
  theme(legend.position = "none")+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  #guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  # guides(color = guide_legend(override.aes = list(size = 5)))+
  facet_grid(c("Treatment","Spp"),
             labeller = labeller(Spp = c("Hw" = "Hemlock",
                                         "Sx" = "Spruce",
                                         "Pl" = "Pine",
                                         "Cw" = "Cedar",
                                         "Ba" = "Fir"),
                                 Treatment = c("NH" = "No harvest",
                                               "LR" = "High retention",
                                               "HR" = "Medium retention",
                                               "CC" = "No retention")))
ggsave(filename = "Hgt_dc_pred_obs_no_adj.png",plot = last_plot(),
       units = "in", path = file.path(out_path,"Supplementary"), device='png', dpi=1200)

#stats to go with the above:
select_sp_h <- function(sp, data) {
  obs <- data[Spp == sp]$cruise_hgt
  pred <- data[Spp == sp]$Height
  n_value <- nrow(data[Spp == sp])
  list(obs = obs, pred = pred, n_value = n_value)
}

#CC
n_vals <- F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                     & Tree.Class <3 & Treatment == "CC", .N, by = "Spp"]
sp <- unique(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                        & Tree.Class <3 & Treatment == "CC"]$Spp)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                                            & Tree.Class <3 & Treatment == "CC"])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df

#HR
n_vals <- F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                     & Tree.Class <3 & Treatment == "HR", .N, by = "Spp"]
sp <- unique(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                        & Tree.Class <3 & Treatment == "HR"]$Spp)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                                            & Tree.Class <3 & Treatment == "HR"])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df

#LR
n_vals <- F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                     & Tree.Class <3 & Treatment == "LR", .N, by = "Spp"]
sp <- unique(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                        & Tree.Class <3 & Treatment == "LR"]$Spp)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                                            & Tree.Class <3 & Treatment == "LR"])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df

#NH
n_vals <- F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                     & Tree.Class <3 & Treatment == "NH", .N, by = "Spp"]
sp <- unique(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                        & Tree.Class <3 & Treatment == "NH"]$Spp)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                                            & Tree.Class <3 & Treatment == "NH"])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df

comp_allom <-  F_trees_dc[, no_adj := F_trees_dc_no$Height]

#how different are they?
ggplot(comp_allom)+
  geom_point(aes(x = no_adj, y = Height, color = Treatment))


comp_allom[, diff := Height - no_adj, by = seq_len(nrow(comp_allom))]

ggplot(comp_allom)+
  geom_boxplot(aes(x = Treatment, y = diff))

#shorter in the clearcut
#no diff in HR
#taller in LR & NH
anova(lm(diff ~ Treatment, data = comp_allom))

hist(na.omit(comp_allom$diff))
length(na.omit(comp_allom$diff))
nrow(comp_allom[diff !=0])
(2840/20780)*100 #13% of trees had a different height with the adjustments.





#most species, the bias is observed is greater than (taller than) the predicted
#what about growth?

  
  
  
  
  
  

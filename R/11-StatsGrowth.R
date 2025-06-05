library(equivalence)
library(ggplot2)
library(data.table)
library(lme4)
library(lmerTest)
library(emmeans)
source(file.path("R","00-utils","utils.R"))

in_path <- "04_out_carbon"
out_path <- "05_out_analysis"

MSL_trees_sl <- readRDS(file.path(in_path,"MSL_trees_sl.RDS"))
FSL_trees_sl <- readRDS(file.path(in_path,"FSL_trees_sl.RDS"))

MFL_trees_sl <- merge(FSL_trees_sl, MSL_trees_sl, by = c("Unit","Treatment","Year","State"),
                      all.x = TRUE)
setnames(MFL_trees_sl, c("MgHa.x","MgHa.y", "BaHa.x", "BaHa.y"),
         c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred"))

MF_trees_sl_m <- melt(MFL_trees_sl, 
                      id.vars = c("Unit", "Treatment", "Year", "State"),
                      measure.vars = c("MgHa_obs", "MgHa_pred", 
                                       "BAHa_obs", "BAHa_pred"),
                      variable.name = "Type", 
                      value.name = "val_ha")
MF_trees_sl_m[, c("Measure", "Type") := tstrsplit(Type, "_")]

#### Basal area recovery analysis
#modelled basal area
MSL_trees_sl <- readRDS(file.path(in_path,"MSL_trees_sl.RDS"))
MSL_trees_dc <- readRDS(file.path(in_path,"MSL_trees_dc.RDS"))

#pre-harvest basal area baseline
FPH_ba_sl <- fread(file.path("01_data","SL_preharvest_BA.csv"))
FPH_ba_dc <- fread(file.path("01_data","DC_preharvest_BA.csv"))
setnames(FPH_ba_sl, "Plot", "Unit")
FPH_ba_sl <- merge(FPH_ba_sl, SummitLakeData::Treatments, by.x = "Unit", by.y = "unit")
setnames(FPH_ba_sl, "treatment", "Treatment")
FPH_ba_dc <- merge(FPH_ba_dc, DateCreekData::Treatments)





MSL_trees_sl_sum <- Rmisc::summarySE(data = MSL_trees_sl, 
                                     measurevar = "BaHa", 
                                     groupvars = c("Treatment","Year"))
MSL_trees_sl_sum <- MSL_trees_sl_sum[MSL_trees_sl_sum$Year < 2093,]

FPH_ba_sl_sum <- Rmisc::summarySE(data = FPH_ba_sl,
                              measurevar = "pre_harv_BA",
                              groupvars = "Treatment")

ggplot(NULL,
       aes(x = Year, y = BaHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSL_trees_sl_sum, aes(col = Treatment), size = 1.2) +
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
  coord_cartesian(ylim = c(0, 80)) +
  labs(
    x = "Year",
    y = expression(paste("Live basal area ( ", m^2, "/ha)")),
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSL_trees_sl_sum,
    aes(ymin = BaHa - ci, ymax = BaHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  geom_hline(
    data = FPH_ba_sl_sum,
    aes(yintercept = pre_harv_BA, col = Treatment),
    linetype = "dashed",
    size = 1
  ) +
  geom_rect(
    data = FPH_ba_sl_sum,
    aes(xmin = -Inf, xmax = Inf, ymin = pre_harv_BA - ci, ymax = pre_harv_BA + ci, fill = Treatment),
    alpha = 0.15,
    inherit.aes = FALSE
  )+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "SBS_live_BA_baseline.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)


MSL_trees_dc_sum <- Rmisc::summarySE(data = MSL_trees_dc, 
                                     measurevar = "BAHa", 
                                     groupvars = c("Year", "Treatment"))
FPH_ba_dc_sum <- Rmisc::summarySE(data = FPH_ba_dc,
                                  measurevar = "pre_harv_BA",
                                  groupvars = "Treatment")
ggplot(NULL,
       aes(x = Year, y = BAHa, fill = Treatment, group = Treatment)) +
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
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    x = "Year",
    y = expression(paste("Live basal area ( ", m^2, "/ha)")),
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSL_trees_dc_sum,
    aes(ymin = BAHa - ci, ymax = BAHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  geom_hline(
    data = FPH_ba_dc_sum,
    aes(yintercept = pre_harv_BA, col = Treatment),
    linetype = "dashed",
    size = 1
  ) +
  geom_rect(
    data = FPH_ba_dc_sum,
    aes(xmin = -Inf, xmax = Inf, ymin = pre_harv_BA - ci, ymax = pre_harv_BA + ci, fill = Treatment),
    alpha = 0.05,
    inherit.aes = FALSE
  )+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave(filename = "ICH_live_BA_baseline.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)


years_to_test <- unique(MSL_trees_sl[Year != 1992 & Year < 2093, Year])
treatments <- unique(MSL_trees_sl$Treatment)

results_list <- list()

for (yr in years_to_test) {
  for (trt in treatments) {
    
    # Data for this year-treatment combo
    current <- MSL_trees_sl[Year == yr & Treatment == trt, BaHa]
    baseline <- FPH_ba_sl[Treatment == trt, pre_harv_BA]
    
    # Basic Welch's t-test
    if (length(current) > 1 & length(baseline) > 1) {
      ttest <- t.test(current, baseline, var.equal = FALSE)
      results_list[[paste(yr, trt)]] <- data.table(
        Year = yr,
        Treatment = trt,
        p_val = round(ttest$p.value, 3)
      )
    }
  }
}
results_dt <- rbindlist(results_list)
# Find the first year when each treatment is NOT sig different from the baseline
results_dt[, TSH := Year - 1992]
setkey(results_dt, Year)
results_dt[Treatment == "light/no"][1:90]
results_dt[, .SD[which.min(p_val >= 0.05)], by = Treatment]

# Join baseline values onto yearly dataset
FPH_ba_sl_sum <- as.data.table(FPH_ba_sl_sum)
MSL_trees_sl_sum <- data.table(MSL_trees_sl_sum)
merged <- merge(MSL_trees_sl_sum, FPH_ba_sl_sum[, .(Treatment, pre_harv_BA)], by = "Treatment")
merged[, diff := abs(BaHa - pre_harv_BA)]
merged[, perc_diff := abs(BaHa - pre_harv_BA) / pre_harv_BA]
recovery_years <- merged[perc_diff <= 0.05, .SD[which.min(Year)], by = Treatment]
recovery_years[, TSH := Year - 1992]




#ICH --------
years_to_test <- unique(MSL_trees_dc[Year != 1992 & Year < 2093, Year])
treatments <- unique(MSL_trees_dc$Treatment)

results_list <- list()

for (yr in years_to_test) {
  for (trt in treatments) {
    
    # Data for this year-treatment combo
    current <- MSL_trees_dc[Year == yr & Treatment == trt, BAHa]
    baseline <- FPH_ba_dc[Treatment == trt, pre_harv_BA]
    
    # Basic Welch's t-test
    if (length(current) > 1 & length(baseline) > 1) {
      ttest <- t.test(current, baseline, var.equal = FALSE)
      results_list[[paste(yr, trt)]] <- data.table(
        Year = yr,
        Treatment = trt,
        p_val = round(ttest$p.value, 3)
      )
    }
  }
}
results_dt <- rbindlist(results_list)

# Find the first year when each treatment is NOT sig different from the baseline
results_dt[, TSH := Year - 1992]
setkey(results_dt, Year)
results_dt[, .SD[which.min(p_val < 0.05)], by = Treatment]
results_dt[Treatment == "CC"][1:90]


# Join baseline values onto yearly dataset
FPH_ba_dc_sum <- as.data.table(FPH_ba_dc_sum)
MSL_trees_dc_sum <- data.table(MSL_trees_dc_sum)
merged <- merge(MSL_trees_dc_sum, FPH_ba_dc_sum[, .(Treatment, pre_harv_BA)], by = "Treatment")
merged[, diff := abs(BAHa - pre_harv_BA)]
merged[, perc_diff := abs(BAHa - pre_harv_BA) / pre_harv_BA]
recovery_years <- merged[perc_diff <= 0.05, .SD[which.min(Year)], by = Treatment]
recovery_years[, TSH := Year - 1992]

















#earlier Basal area figures -------------------------------------------------------------
MSL_trees_sl_sp_sum <- Rmisc::summarySE(MSL_trees_sl_sp[Species == "Bl" |
                                                          Species == "Sx"],
                                        measurevar = "BaHa", 
                                        groupvars = c("Treatment","Year", "Species"))
MSL_trees_sl_sp_sum <- data.table(MSL_trees_sl_sp_sum)


ggplot()+
  geom_point(aes(x = BAHa_pred, y = BAHa_obs, group = as.factor(Unit), 
                 color = Treatment), 
             alpha = 0.9,
             size = 2,
             data = MFL_trees_sl)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  coord_cartesian() +
  labs(
    x = "Live basal area (m3/ha) predicted",
    y = "Live basal area (m3/ha) observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,60))+
  ylim(c(0,60))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))
  facet_wrap(~Year)

ggsave(filename = "SBS_BA_fit.png",plot = last_plot(), width = 7.91, height = 5.61,
       units = "in", path = file.path(out_path), device='png', dpi=1200)
years <- c(1992, 1994, 1997, 2009, 2019, "All Years")

results <- lapply(years, function(year) {
  data <- select_years(year,meas_obs = BAHa_obs, meas_pred = BAHa_pred,
                       data = MFL_trees_sl)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Year = years, results_df)
results_df
MFL_trees_summary <- MFL_trees_sl %>%
  group_by(Year) %>%
  summarise(
    MgHa_obs_mean = mean(BAHa_obs, na.rm = TRUE),
    MgHa_obs_sd   = sd(BAHa_obs, na.rm = TRUE),
    MgHa_pred_mean = mean(BAHa_pred, na.rm = TRUE),
    MgHa_pred_sd   = sd(BAHa_pred, na.rm = TRUE)
  )

ggplot(NULL,
       aes(x = Year, y = BaHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSL_trees_sl_sp_sum[Species == "Bl"|Species == "Sx"], 
            aes(col = Treatment), size = 1.8) +
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
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    x = "Year",
    y = "Carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment",
    linetype = "Treatment"
  ) +
  # geom_ribbon(
  #    data = MSL_trees_sl_sp_sum[Species == "Bl"|Species == "Sx"],
  #   aes(ymin = MgHa - ci, ymax = MgHa + ci),
  #    alpha = 0.25,
  #   linetype = "solid",
  #    color = "grey"
  #  )+
  facet_wrap(c("Species"), labeller = as_labeller(c("Bl" = "Fir",
                                                    "Sx" = "Spruce")))+
  #geom_point(
  #  data = FSL_trees_sl_sp_sum,
  #  aes(shape = Treatment),
  #  size = 3,
  #  position = position_dodge(width = 0.5)
  #)+ 
  geom_jitter(
    data = FSL_trees_sl_sp[Species == "Bl"|Species == "Sx"],
    aes(shape = Treatment),
    size = 2 ,
    position = position_dodge(width = 1)
  )+
  #geom_point(
  #  data = MSL_trees_sl_sp[Species == "Bl" & Year < 2022|Species == "Sx"& Year < 2022],
  #  aes(group = interaction(Species, Treatment), color = Treatment),
  #  alpha = 0.1#,
  #linetype = "dotted"
  #color = "grey"
  #) + 
  geom_line(
    data = MSL_trees_sl_sp[Species == "Bl"|Species == "Sx"],
    aes(group = Unit, color = Treatment, linetype = Treatment),
    alpha = 0.3,
    size = 0.8
    #color = "grey"
  ) +
  scale_shape_manual(
    values = c(24, 23, 21),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  )+
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  )+
  theme(legend.position = "bottom")+
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(size = 5)),
    shape = guide_legend(override.aes = list(size = 5)),
    linetype = guide_legend(override.aes = list(size = 5)))
# +
#geom_errorbar(
#  data = FSL_trees_sl_sp_sum,
#  aes(ymin = MgUnit - ci, ymax = MgUnit + ci),
#  position = position_dodge(width = 1)
#) 
ggsave(filename = "SBS_live_trees_sp_tr.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)
ggplot()+
  geom_point(aes(x = BAHa_pred, 
                 y = BAHa_obs, 
                 group = as.factor(Unit), 
                 color = Treatment), 
             alpha = 0.9,
             size = 2,
             data = MFL_trees_sl)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  coord_cartesian() +
  labs(
    x = "Basal area (M2/ha) predicted",
    y = "Basal area (M2/ha) observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,50))+
  ylim(c(0,50))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "SBS_BA_fit.png",plot = last_plot(), width = 7.91, height = 5.61,
       units = "in", path = file.path(out_path), device='png', dpi=1200)

ggplot()+
  geom_point(aes(x = BAHa_pred, 
                 y = BAHa_obs, 
                 group = as.factor(Unit), 
                 color = Treatment), 
             alpha = 0.9,
             size = 2,
             data = MFL_trees_sl)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  coord_cartesian() +
  labs(
    x = "Basal area (M2/ha) predicted",
    y = "Basal area (M2/ha) observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,50))+
  ylim(c(0,50))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  facet_wrap(~Year)
ggsave(filename = "SBS_BA_fit_yr.png",plot = last_plot(), width = 7.91, height = 5.61,
       units = "in", path = file.path(out_path, "Supplementary"), device='png', dpi=1200)

years <- c(1992, 1994, 1997, 2009, 2019, "All Years")

results <- lapply(years, function(year) {
  data <- select_years(year, meas_obs = "BAHa_obs", meas_pred = "BAHa_pred", 
                       data = MFL_trees_sl)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Year = years, results_df)
results_df
res_m <- data.table(melt(results_df, id.vars = "Year"))

#is there a significant difference between predicted and observed by 2019?
equi_result(FSL_trees_sl[Year == 2019]$BaHa, 
            MSL_trees_sl[Year == 2019]$BaHa,
            eq_margin = 0.5)
#not rejected == they are not different
mean(MSL_trees_sl[Year == 2019]$BaHa)
sd(MSL_trees_sl[Year == 2019]$BaHa)
mean(FSL_trees_sl[Year == 2019]$BaHa)
sd(FSL_trees_sl[Year == 2019]$BaHa)

# ICH ----------------------------------------------------------------------------------------
MSL_trees_dc <- readRDS(file.path(in_path,"MSL_trees_dc.RDS"))
FSL_trees_dc <- readRDS(file.path(in_path,"FSL_trees_dc.RDS"))

MFL_trees_dc <- merge(FSL_trees_dc, MSL_trees_dc, by = c("Unit","Treatment","Year","State"),
                      all.x = TRUE)
setnames(MFL_trees_dc, c("MgHa.x","MgHa.y", "BAHa.x", "BAHa.y","QMD.x","QMD.y"),
         c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred","QMD_obs","QMD_pred"))

# MF_trees_dc_sp <- merge(FSL_trees_dc_sp, MSL_trees_dc_sp, 
#  by = c("Unit","Treatment","Year","State","Species"),
#                       all.x = TRUE)
#setnames(MF_trees_dc_sp, c("MgHa.x","MgHa.y"), c("MgHa_obs","MgHa_pred"))

ggplot()+
  geom_line(aes(x = Year, y = BAHa, group = as.factor(Unit), color = Treatment), 
            data = MSL_trees_dc)+
  #geom_abline(slope = 0, intercept = 56.7754, linetype = "dashed", color = "grey")+
  geom_point(aes(x = Year, y = BAHa, color = Treatment), 
             data = FSL_trees_dc)
MF_trees_dc_m <- melt(MFL_trees_dc, 
                      id.vars = c("Unit", "Treatment", "Year", "State"),
                      measure.vars = c("MgHa_obs", "MgHa_pred", 
                                       "BAHa_obs", "BAHa_pred",
                                       "QMD_obs", "QMD_pred"),
                      variable.name = "Type", 
                      value.name = "val_ha")
MF_trees_dc_m[, c("Measure", "Type") := tstrsplit(Type, "_")]

ggplot()+
  geom_point(aes(x = BAHa_pred, 
                 y = BAHa_obs, 
                 group = as.factor(Unit), 
                 color = Treatment), 
             alpha = 0.9,
             size = 2,
             data = MFL_trees_dc)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  coord_cartesian() +
  labs(
    x = "Live basal area (m3/ha) predicted",
    y = "Live basal area (m3/ha) observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,100))+
  ylim(c(0,100))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "ICH_BA_fit.png",plot = last_plot(), width = 7.91, height = 5.61,
       units = "in", path = file.path(out_path), device='png', dpi=1200)

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

MFL_trees_summary <- MFL_trees_dc %>%
  group_by(Year) %>%
  summarise(
    MgHa_obs_mean = mean(BAHa_obs, na.rm = TRUE),
    MgHa_obs_sd   = sd(BAHa_obs, na.rm = TRUE),
    MgHa_pred_mean = mean(BAHa_pred, na.rm = TRUE),
    MgHa_pred_sd   = sd(BAHa_pred, na.rm = TRUE)
  ) %>% mutate(across(where(is.numeric), ~ format(., nsmall = 1)))

ggplot()+
  geom_point(aes(x = BAHa_pred, 
                 y = BAHa_obs, 
                 group = as.factor(Unit), 
                 color = Treatment), 
             alpha = 0.9,
             size = 2,
             data = MFL_trees_dc)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  coord_cartesian() +
  labs(
    x = "Basal area (M3/ha) predicted",
    y = "Basal area (M3/ha) observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  facet_wrap(~Year)+
  xlim(c(0,100))+
  ylim(c(0,100))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "ICH_BA_fit_yr.png", plot = last_plot(), width = 7.91, height = 5.61,
       units = "in", path = file.path(out_path), device='png', dpi=1200)


years <- c(1992, 1993, 2010, 2018, 2022, "All Years")

results <- lapply(years, function(year) {
  data <- select_years(year, meas_obs = "BAHa_obs", meas_pred = "BAHa_pred", data = MFL_trees_dc)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Year = years, results_df)
results_df

#is there a significant difference between predicted and observed by 2019?
equi_result(FSL_trees_dc[Year == 2022]$BAHa, 
            MSL_trees_dc[Year == 2022]$BAHa,
            eq_margin = 0.5)
#not rejected == they are not different
mean(MSL_trees_dc[Year == 1992]$BAHa)
sd(MSL_trees_dc[Year == 1992]$BAHa)
mean(FSL_trees_dc[Year == 1992]$BAHa)
sd(FSL_trees_dc[Year == 1992]$BAHa)

mean(MSL_trees_dc[Year == 1993]$BAHa)
sd(MSL_trees_dc[Year == 1993]$BAHa)
mean(FSL_trees_dc[Year == 1993]$BAHa)
sd(FSL_trees_dc[Year == 1993]$BAHa)

mean(MSL_trees_dc[Year == 2010]$BAHa)
sd(MSL_trees_dc[Year == 2010]$BAHa)
mean(FSL_trees_dc[Year == 2010]$BAHa)
sd(FSL_trees_dc[Year == 2010]$BAHa)

mean(MSL_trees_dc[Year == 2018]$BAHa)
sd(MSL_trees_dc[Year == 2018]$BAHa)
mean(FSL_trees_dc[Year == 2018]$BAHa)
sd(FSL_trees_dc[Year == 2018]$BAHa)

mean(MSL_trees_dc[Year == 2022]$BAHa)
sd(MSL_trees_dc[Year == 2022]$BAHa)
mean(FSL_trees_dc[Year == 2022]$BAHa)
sd(FSL_trees_dc[Year == 2022]$BAHa)


ggplot()+
  geom_point(aes(x = BAHa_pred, y = BAHa_obs, group = as.factor(Unit), 
                 color = Treatment), 
             alpha = 0.9,
             size = 2,
             data = MFL_trees_dc)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  coord_cartesian() +
  labs(
    x = "BA predicted",
    y = "BA observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,100))+
  ylim(c(0,100))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  facet_wrap(~Year)
ggsave(filename = "ICH_live_BA_by_year.png",
       path = file.path(out_path, "Supplementary"), device='png', dpi=1200)

sp_incl <- c("Cw","Sx","Ba", "Pl")

ggplot()+
  geom_point(aes(x = BAHa_pred, y = BAHa_obs, group = as.factor(Unit), 
                 color = Treatment), 
             alpha = 0.9,
             size = 2,
             data = MF_trees_dc_sp[Species %in% sp_incl])+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  coord_cartesian() +
  labs(
    x = "BA predicted",
    y = "BA observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,40))+
  ylim(c(0,40))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5, order = 1),  # Treatment legend first
         shape = guide_legend(title.position = "top", title.hjust = 0.5, order = 2)) +  # Species legend second
  guides(color = guide_legend(override.aes = list(size = 5)))+
  facet_wrap(~Year)
#facet_grid(c("Species", "Year"))
ggsave(filename = "ICH_live_BA_sp_tr_cw_sx_pl_Ba_grid.jpg",
       path = file.path(out_path), device='jpeg', dpi=1200)

sp_incl <- c("Hw")
ggplot()+
  geom_point(aes(x = BAHa_pred, y = BAHa_obs, group = as.factor(Unit), 
                 color = Treatment), 
             alpha = 0.9,
             size = 2,
             data = MF_trees_dc_sp[Species %in% sp_incl])+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  coord_cartesian() +
  labs(
    x = "BA predicted",
    y = "BA observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,75))+
  ylim(c(0,75))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5, order = 1),  # Treatment legend first
         shape = guide_legend(title.position = "top", title.hjust = 0.5, order = 2)) +  # Species legend second
  guides(color = guide_legend(override.aes = list(size = 5)))+
  facet_wrap(~Year)
#facet_grid(c("Species", "Year"))
ggsave(filename = "ICH_live_BA_sp_tr_hw_grid.jpg",
       path = file.path(out_path), device='jpeg', dpi=1200)













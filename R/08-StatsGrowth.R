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

#MF_trees_dc_sp <- merge(FSL_trees_dc_sp, MSL_trees_dc_sp, by = c("Unit","Treatment","Year","State","Species"),
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
    x = "Basal area (M3/ha) predicted",
    y = "Basal area (M3/ha) observed",
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
res_m <- data.table(melt(results_df, id.vars = "Year"))

#is there a significant difference between predicted and observed by 2019?
equi_result(FSL_trees_dc[Year == 2022]$BAHa, 
            MSL_trees_dc[Year == 2022]$BAHa,
            eq_margin = 0.5)
#not rejected == they are not different
mean(MSL_trees_dc[Year == 2022]$BAHa)
sd(MSL_trees_dc[Year == 2022]$BAHa)
mean(FSL_trees_dc[Year == 2022]$BAHa)
sd(FSL_trees_dc[Year == 2022]$BAHa)
















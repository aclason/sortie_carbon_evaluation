# A.Clason
library(equivalence)
library(ggplot2)
library(data.table)
library(lme4)
library(lmerTest)
library(emmeans)
source(file.path("R","00-utils","utils.R"))

in_path <- "04_out_carbon"
out_path <- "05_out_analysis"

# SBS -------------------------------------------------------------------------------------------
MSD_trees_sl <- readRDS(file.path(in_path,"MSD_trees_sl.RDS"))
FSD_trees_sl <- readRDS(file.path(in_path,"FSD_trees_sl.RDS"))

#MSD_trees_sl_sp <- readRDS(file.path(in_path,"MSD_trees_sl_sp.RDS"))
#FSD_trees_sl_sp <- readRDS(file.path(in_path,"FSD_trees_sl_sp.RDS"))

ggplot()+
  geom_line(aes(x = Year, y = MgHa, group = as.factor(Unit), color = Treatment), 
            data = MSD_trees_sl)+
  geom_point(aes(x = Year, y = MgHa, color = Treatment), 
             data = FSD_trees_sl)

MFD_trees_sl <- merge(FSD_trees_sl, MSD_trees_sl, by = c("Unit","Treatment","Year","State"),
                     all.x = TRUE)
MFD_trees_sl[is.na(MFD_trees_sl)] <- 0
setnames(MFD_trees_sl, c("MgHa.x","MgHa.y"), c("MgHa_obs","MgHa_pred"))

#MFD_trees_sl_sp <- merge(FSD_trees_sl_sp, MSD_trees_sl_sp, by = c("unit","treatment","Year","State","Species"),
#                        all.x = TRUE)
#setnames(MFD_trees_sl_sp, c("MgUnit.x","MgUnit.y"), c("MgHa_obs","MgHa_pred"))
#MFD_trees_sl_sp[is.na(MgHa_pred), MgHa_pred := 0]

MFD_trees_sl_m <- melt(MFD_trees_sl, 
                      id.vars = c("Unit", "Treatment", "Year", "State"),
                      measure.vars = c("MgHa_obs", "MgHa_pred"),
                      variable.name = "Type", 
                      value.name = "MgHa")
MFD_trees_sl_m[, Type := ifelse(Type == "MgHa_obs", "obs", "pred")]
#MFD_trees_sl_sp_m <- melt(MFD_trees_sl_sp, 
                         #id.vars = c("Unit", "Treatment", "Year", "State", "Species"),
 #                        measure.vars = c("MgHa_obs", "MgHa_pred"),
  #                       variable.name = "Type", 
   #                      value.name = "MgHa")
#MFD_trees_sl_sp_m[, Type := ifelse(Type == "MgHa_obs", "obs", "pred")]

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
    x = "Carbon (Mg/ha) predicted",
    y = "Carbon (Mg/ha) observed",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,35))+
  ylim(c(0,35))+
  theme(legend.position = "bottom")+
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
    guides(color = guide_legend(override.aes = list(size = 5)))

ggsave(filename = "SBS_dead_fit.png",
       path = file.path(out_path, "Supplementary"), device='png', dpi=1200)

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
    x = "Carbon (Mg/ha) predicted",
    y = "Carbon (Mg/ha) observed",
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

MSD_trees_sl_sum <- Rmisc::summarySE(data = MSD_trees_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Treatment","Year"),
                                     na.rm = TRUE)
FSD_trees_sl_sum <- Rmisc::summarySE(data = FSD_trees_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Treatment","Year"),
                                     na.rm = TRUE)

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
    y = "Carbon (Mg/ha)",
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
#  geom_point(
 #   data = FSD_trees_sl_sum,
#    aes(shape = Treatment),
#    size = 5 ,
 #   position = position_dodge(width = 3)
#  ) +
  geom_jitter(
    data = FSD_trees_sl,
    aes(shape = Treatment),
    size = 1 ,
    position = position_dodge(width = 3)
  )+
#  geom_line(
 #   data = MSD_trees_sl,
#    aes(group = Unit, color = Treatment),
#    alpha = 0.7,
#    linetype = "dotted"
    #color = "grey"
 # ) +
  scale_shape_manual(
    values = c(24, 23, 21),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) 
 # geom_errorbar(
  #  data = FSD_trees_sl_sum,
  #  aes(ymin = MgHa - ci, ymax = MgHa + ci),
   # position = position_dodge(width = 1)
  #) 
ggsave(filename = "SBS_dead_trees.jpg",
       path = file.path(out_path, "Sumpplementary"), device='jpeg', dpi=1000)

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
years <- c(1992, 1994, 1997, 2009, 2019, "All Years")

results <- lapply(years, function(year) {
  data <- select_years(year,meas_obs = MgHa_obs, meas_pred = MgHa_pred,
                       data = MFD_trees_sl)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Year = years, results_df)
results_df
res_m <- data.table(melt(results_df, id.vars = "Year"))

t.test(MSD_trees_sl[Year == 2019]$MgHa,
       FSD_trees_sl[Year == 2019]$MgHa)


#is there a significant difference between predicted and observed by 2019?
equi_result(FSD_trees_sl[Year == 2019]$MgHa, 
            MSD_trees_sl[Year == 2019]$MgHa,
            eq_margin = 0.5)
#not rejected == they are not d

mean(MSD_trees_sl[Year == 2019]$MgUnit)
sd(MSD_trees_sl[Year == 2019]$MgUnit)
mean(FSD_trees_sl[Year == 2019]$MgUnit)
sd(FSD_trees_sl[Year == 2019]$MgUnit)

MFD_trees_sl_sp_m[,`:=`(TSH = Year - 1992)]
model1 <- lmer(MgHa ~ treatment + Species + TSH + Type + (1 | unit), 
               data = MFD_trees_sl_sp_m[Species == "Bl" |Species == "Sx"])
anova(model1)


# ICH ----------------------------------------------------------------------------------------
MSD_trees_dc <- readRDS(file.path(in_path,"MSD_trees_dc.RDS"))
FSD_trees_dc <- readRDS(file.path(in_path,"FSD_trees_dc_mh.RDS")) 
#re-run dc dead field
FSD_trees_dc[, State := "Dead"]

ggplot()+
  geom_line(aes(x = Year, y = MgHa, group = as.factor(Unit), color = Treatment), 
            data = MSL_trees_dc)+
  #geom_abline(slope = 0, intercept = 56.7754, linetype = "dashed", color = "grey")+
  geom_point(aes(x = Year, y = MgHa, color = Treatment), 
             data = FSL_trees_dc)

MFD_trees_dc <- merge(FSD_trees_dc, MSD_trees_dc, by = c("Unit","Year","State"),
                      all.x = TRUE)
setnames(MFD_trees_dc, c("MgHa.x","MgHa.y", "BAHa.x", "BAHa.y"),
         c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred"))

#MF_trees_dc_sp <- merge(FSL_trees_dc_sp, MSL_trees_dc_sp, 
 #                       by = c("Unit","Treatment","Year","State","Species"),
  #                      all.x = TRUE)
#setnames(MF_trees_dc_sp, c("MgHa.x","MgHa.y", "BAHa", "BaHa"),
 #        c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred"))


MF_trees_dc_m <- melt(MFD_trees_dc, 
                      id.vars = c("Unit", "Treatment", "Year", "State"),
                      measure.vars = c("MgHa_obs", "MgHa_pred", "BAHa_obs", "BAHa_pred"),
                      variable.name = "Type", 
                      value.name = "val_ha")
MF_trees_dc_m[, c("Measure", "Type") := tstrsplit(Type, "_")]
#MF_trees_dc_m[, obs_preds := ifelse(Type == "MgHa_obs" | Type == "BaHa_obs", "obs", "pred")]
#MF_trees_dc_m[, val_type := tstrsplit(Type, "_", fixed = TRUE)[[1]]]

#MF_trees_dc_sp_m <- melt(MF_trees_dc_sp, 
 #                        id.vars = c("Unit", "Treatment", "Year", "State", "Species"),
  #                       measure.vars = c("MgHa_obs", "MgHa_pred", "BAHa_obs", "BAHa_pred"),
   #                      variable.name = "Type", 
    #                     value.name = "val_ha")

#MF_trees_dc_sp_m[, obs_preds := ifelse(Type == "MgHa_obs" | Type == "BaHa_obs", "obs", "pred")]
#MF_trees_dc_sp_m[, val_type := tstrsplit(Type, "_", fixed = TRUE)[[1]]]

# Carbon predicted vs observed 
ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, 
                 color = Treatment), 
             alpha = 0.9,
             size = 2,
             data = MFD_trees_dc)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  coord_cartesian() +
  labs(
    x = "Carbon (Mg/ha) predicted",
    y = "Carbon (Mg/ha) observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,50))+
  ylim(c(0,50))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))#+
#facet_wrap(~Year)
ggsave(filename = "ICH_dead_fit.png",plot = last_plot(), width = 7.91, height = 5.61,
       units = "in", path = file.path(out_path), device='png', dpi=1200)

# Carbon predicted vs observed 
ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, 
                 color = Treatment), 
             alpha = 0.9,
             size = 2,
             data = MFD_trees_dc)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  coord_cartesian() +
  labs(
    x = "Carbon (Mg/ha) predicted",
    y = "Carbon (Mg/ha) observed",
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
ggsave(filename = "ICH_dead_fit_yr.png",plot = last_plot(), width = 7.91, height = 5.61,
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

t.test(MSD_trees_sl[Year == 2019]$MgHa,
       FSD_trees_sl[Year == 2019]$MgHa)


#is there a significant difference between predicted and observed by 2019?
equi_result(FSD_trees_sl[Year == 2019]$MgHa, 
            MSD_trees_sl[Year == 2019]$MgHa,
            eq_margin = 0.5)
#not rejected == they are not d

mean(MSD_trees_sl[Year == 2019]$MgUnit)
sd(MSD_trees_sl[Year == 2019]$MgUnit)
mean(FSD_trees_sl[Year == 2019]$MgUnit)
sd(FSD_trees_sl[Year == 2019]$MgUnit)


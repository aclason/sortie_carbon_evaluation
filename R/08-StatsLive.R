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
MSL_trees_sl <- readRDS(file.path(in_path,"MSL_trees_sl.RDS"))
FSL_trees_sl <- readRDS(file.path(in_path,"FSL_trees_sl.RDS"))

MSD_trees_sl <- readRDS(file.path(in_path,"MSD_trees_sl.RDS"))
FSD_trees_sl <- readRDS(file.path(in_path,"FSD_trees_sl.RDS"))


MSL_trees_sl_sp <- readRDS(file.path(in_path,"MSL_trees_sl_sp.RDS"))
FSL_trees_sl_sp <- readRDS(file.path(in_path,"FSL_trees_sl_sp.RDS"))


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

MF_trees_sl_m <- melt(MFL_trees_sl, 
                         id.vars = c("Unit", "Treatment", "Year", "State"),
                         measure.vars = c("MgHa_obs", "MgHa_pred", "BaHa_obs", "BaHa_pred"),
                         variable.name = "Type", 
                         value.name = "val_ha")
MF_trees_sl_m[, obs_preds := ifelse(Type == "MgHa_obs" | Type == "BaHa_obs", "obs", "pred")]
MF_trees_sl_m[, val_type := tstrsplit(Type, "_", fixed = TRUE)[[1]]]
MF_trees_sl_sp_m <- melt(MF_trees_sl_sp, 
                              id.vars = c("Unit", "Treatment", "Year", "State", "Species"),
                              measure.vars = c("MgHa_obs", "MgHa_pred", "BaHa_obs", "BaHa_pred"),
                              variable.name = "Type", 
                              value.name = "val_ha")
MF_trees_sl_sp_m[, obs_preds := ifelse(Type == "MgHa_obs" | Type == "BaHa_obs", "obs", "pred")]
MF_trees_sl_sp_m[, val_type := tstrsplit(Type, "_", fixed = TRUE)[[1]]]

t.test(MF_trees_sl_sp_m[obs_preds == "obs" & val_type == "MgHa" & Species == "Bl"]$val_ha,
       MF_trees_sl_sp_m[obs_preds == "pred" & val_type == "MgHa" & Species == "Bl"]$val_ha)
#Bl not sig diff
t.test(MF_trees_sl_sp_m[obs_preds == "obs" & val_type == "MgHa" & Species == "Sx"]$val_ha,
       MF_trees_sl_sp_m[obs_preds == "pred" & val_type == "MgHa" & Species == "Sx"]$val_ha)
#Sx not sig diff
t.test(MF_trees_sl_sp_m[obs_preds == "obs" & val_type == "MgHa" & Species == "Bl" & TSH < 5]$val_ha,
       MF_trees_sl_sp_m[obs_preds == "pred" & val_type == "MgHa" & Species == "Bl" & TSH < 5]$val_ha)
#Bl not sig diff at the start
t.test(MF_trees_sl_sp_m[obs_preds == "obs" & val_type == "MgHa" & Species == "Sx"& TSH < 5]$val_ha,
       MF_trees_sl_sp_m[obs_preds == "pred" & val_type == "MgHa" & Species == "Sx"& TSH < 5]$val_ha)
#Sx not sig diff at the start

t.test(MF_trees_sl_sp_m[obs_preds == "obs" & val_type == "MgHa" & Species == "Bl" & TSH == 28]$val_ha,
       MF_trees_sl_sp_m[obs_preds == "pred" & val_type == "MgHa" & Species == "Bl" & TSH == 28]$val_ha)
#Bl not sig diff at the end - but trending towards significance
t.test(MF_trees_sl_sp_m[obs_preds == "obs" & val_type == "MgHa" & Species == "Sx"& TSH == 28]$val_ha,
       MF_trees_sl_sp_m[obs_preds == "pred" & val_type == "MgHa" & Species == "Sx"& TSH == 28]$val_ha)
#Sx not sig diff at the end


ggplot()+
  geom_line(aes(x = Year, y = MgHa, group = as.factor(Unit), color = Treatment), 
            data = MSL_trees_sl[Year < 2021])+
  geom_abline(slope = 0, intercept = 56.7754, linetype = "dashed", color = "grey")+
  geom_point(aes(x = Year, y = MgHa, color = Treatment), 
             data = FSL_trees_sl)

ggplot()+
  geom_line(aes(x = Year, y = MgHa, group = as.factor(Unit), color = as.factor(Unit)), 
            data = MSL_trees_sl[Year < 2021])+
  geom_abline(slope = 0, intercept = 56.7754, linetype = "dashed", color = "grey")+
  geom_point(aes(x = Year, y = MgHa, color = as.factor(Unit)), 
             data = FSL_trees_sl)+
  facet_wrap(~Treatment)

# Carbon predicted vs observed 
ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(Unit), 
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
    x = "Carbon (Mg/ha) predicted",
    y = "Carbon (Mg/ha) observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,125))+
  ylim(c(0,125))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "SBS_live_fit_mg.png",plot = last_plot(), width = 7.91, height = 5.61,
       units = "in", path = file.path(out_path), device='png', dpi=1200)

ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(Unit), 
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
    x = "Carbon (Mg/ha) predicted",
    y = "Carbon (Mg/ha) observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,125))+
  ylim(c(0,125))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  facet_wrap(~Year)
ggsave(filename = "SBS_live_fit_yr_measHt.png",plot = last_plot(), width = 7.91, height = 5.61,
       units = "in", path = file.path(out_path, "Supplementary"), device='png', dpi=1200)


MSL_trees_sl_sum <- Rmisc::summarySE(data = MSL_trees_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Treatment","Year"))
FSL_trees_sl_sum <- Rmisc::summarySE(data = FSL_trees_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Treatment","Year"))

ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
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
  coord_cartesian(ylim = c(0, 120)) +
  labs(
    x = "Year",
    y = "Carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSL_trees_sl_sum,
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
    data = FSL_trees_sl,
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
  xlim(c(1990, 2022))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))
  #+
 # geom_errorbar(
#    data = FSL_trees_sl_sum,
#    aes(ymin = MgHa - ci, ymax = MgHa + ci),
#    position = position_dodge(width = 1)
#  ) 
ggsave(filename = "SBS_live_trees.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)


ggplot()+
  geom_line(aes(x = Year, y = MgHa, group = as.factor(Unit), 
                color = as.factor(Unit)), 
            data = MSL_trees_sl_sp[Species == "Bl"|Species == "Sx"])+
  geom_jitter(aes(x = Year, y = MgHa, color = as.factor(Unit)), 
              data = FSL_trees_sl_sp[Species == "Bl"|Species == "Sx"])+
  facet_grid(c("Species","Treatment"))


MSL_trees_sl_sp_sum <- Rmisc::summarySE(MSL_trees_sl_sp[Species == "Bl" & Year < 2024|
                                                          Species == "Sx" & Year < 2024],
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year", "Species"))
                                        
FSL_trees_sl_sp_sum <- Rmisc::summarySE(FSL_trees_sl_sp[Species == "Bl"|
                                                          Species == "Sx"],
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year", "Species"))

ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Species)) +
  geom_line(data = MSL_trees_sl_sp_sum, aes(col = Treatment), size = 1.2) +
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
  coord_cartesian(ylim = c(0, 120)) +
  labs(
    x = "Year",
    y = "Carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSL_trees_sl_sp_sum,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  )+
  facet_grid(c("Species","Treatment"))+
  geom_point(
    data = FSL_trees_sl_sp_sum,
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
  geom_line(
    data = MSL_trees_sl_sp[Species == "Bl"|Species == "Sx"],
    aes(group = Unit, color = Treatment),
    alpha = 0.7,
    linetype = "dotted"
    #color = "grey"
  ) +
  scale_shape_manual(
    values = c(24, 23, 21),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  geom_errorbar(
    data = FSL_trees_sl_sp_sum,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    position = position_dodge(width = 1)
  )+
  xlim(c(1990, 2022))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave(filename = "SBS_live_trees_sp.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)

MSL_trees_sl_sp_sum <- Rmisc::summarySE(MSL_trees_sl_sp[Species == "Bl" & Year < 2022|
                                                          Species == "Sx"& Year < 2022],
                                        measurevar = "BaHa", 
                                        groupvars = c("Treatment","Year", "Species"))
MSL_trees_sl_sp_sum <- data.table(MSL_trees_sl_sp_sum)

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
    data = MSL_trees_sl_sp[Species == "Bl" & Year < 2022|Species == "Sx"& Year < 2022],
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

MSL_trees_sl_sum <- Rmisc::summarySE(data = MSL_trees_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Year"))
FSL_trees_sl_sum <- Rmisc::summarySE(data = FSL_trees_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Year"))
ggplot()+
  geom_line(data = MSL_trees_sl_sum, 
            aes(x = Year, y = MgHa), linetype = 2)+
  geom_line(data = FSL_trees_sl_sum, 
            aes(x = Year, y = MgHa), linetype = 1)+

ggplot(data = MF_trees_sl_m[val_type == "MgHa"]) +
  geom_line(
    aes(x = Year, y = val_ha, group = Unit, color = Treatment, linetype = Treatment),
    size = 0.8 ,
    position = position_dodge(width = 1)
  )+
  facet_wrap(~obs_preds)+
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

  geom_line(
    data = FSL_trees_sl,
    aes(group = Unit, color = Treatment, linetype = Treatment),
    size = 0.8 ,
    position = position_dodge(width = 1)
  )+
  geom_line(
    data = MSL_trees_sl,
    aes(group = Unit, color = Treatment, linetype = Treatment),
    alpha = 0.3,
    size = 0.8
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

#Stats -------------------------------------------------------------------------
years <- c(1992, 1994, 1997, 2009, 2019, "All Years")

results <- lapply(years, function(year) {
  data <- select_years(year,meas_obs = MgHa_obs, meas_pred = MgHa_pred,
                       data = MFL_trees_sl)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Year = years, results_df)
results_df
res_m <- data.table(melt(results_df, id.vars = "Year"))
#cohen d: positive value - actual is XX MgHa higher than predicted

#ggplot()+
#  geom_line(aes(x = as.factor(Year), y = as.numeric(value), 
#                group = as.factor(variable), color = variable), 
#            data = res_m[Year != "All Years" & variable != "RMSE"])+
#  geom_point(aes(x = as.factor(Year), y = as.numeric(value), 
#                group = as.factor(variable), color = variable, size =2), 
#            data = res_m[Year == "All Years" & variable != "RMSE"])
#sp <- c("Bl","Sx")

#results <- lapply(sp, function(sp) {
#  data <- select_sp(sp, data = MF_trees_sl_sp)
#  sapply(stat_functions, function(f) f(data))
#})
#results_df <- do.call(rbind, results)
#results_df <- data.frame(Species = sp, results_df)
#results_df

#ts <- c("light/no","med","heavy")

#results <- lapply(ts, function(ts) {
#  data <- select_ts(ts, data = MF_trees_sl)
#  sapply(stat_functions, function(f) f(data))
#})
#results_df <- do.call(rbind, results)
#results_df <- data.frame(Species = ts, results_df)
#results_df

#ts <- c("light/no","med","heavy")
#sp <- c("Bl","Sx")
#years <- c(1992, 1994, 1997, 2009, 2019, "All Years")

#results <- lapply(years, function(ts, sp, years) {
#  data <- select_all(ts, data = MF_trees_sl)
#  sapply(stat_functions, function(f) f(data))
#})
#results_df <- do.call(rbind, results)
#results_df <- data.frame(Species = ts, results_df)
#results_df

#is there a significant difference between predicted and observed by 2019?
equi_result(FSL_trees_sl[Year == 2019]$MgHa, 
            MSL_trees_sl[Year == 2019]$MgHa,
            eq_margin = 0.5)
#not rejected == they are not different
mean(MSL_trees_sl[Year == 2019]$MgHa)
sd(MSL_trees_sl[Year == 2019]$MgHa)
mean(FSL_trees_sl[Year == 2019]$MgHa)
sd(FSL_trees_sl[Year == 2019]$MgHa)

equi_boot(actual = FSL_trees_sl[Year == 2019]$MgUnit, 
          predicted = MSL_trees_sl[Year == 2019]$MgUnit,
          n_bootstraps = 1000,
          eq_margin = 0.5)

#MF_trees_sl_m[,`:=`(unit_f = as.factor(unit),
 #                   treatment = as.factor(treatment),
#                    State = as.factor(State))]
#str(MF_trees_sl_m)
MF_trees_sl_m[,`:=`(Unit = as.factor(Unit),
                       TSH = Year - 1991)]
dat_sl <- MF_trees_sl_m[val_type == "MgHa"]

model1 <- lmer(val_ha ~ obs_preds * Treatment * TSH + (1 | Unit), 
               data = dat_sl)

model2 <- lmer(val_ha ~ obs_preds * Treatment + TSH + (1 | Unit), 
               data = dat_sl)

model3 <- lmer(val_ha ~ Treatment * TSH + obs_preds + (1 | Unit), 
               data = dat_sl)

model4 <- lmer(val_ha ~ obs_preds * TSH + Treatment  + (1 | Unit), 
               data = dat_sl)

model5 <- lmer(val_ha ~ obs_preds + TSH + Treatment  + (1 | Unit),
               data = dat_sl)

#MgHa ~ Type * treatment * Species * TSH + (1 | unit)
AIC(model1, model2, model3, model4, model5)
anova(model1)
contrast(emmeans(model1, ~ Treatment), method = "tukey")
#contrast(emmeans(model6, ~ Species | TSH), method = "tukey")

# this estimates the slope effects and contrasts:
emt <- emtrends(model1, tukey ~ obs_preds, var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)

emt <- emtrends(model1, tukey ~ Treatment, var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)

### Including species

MF_trees_sl_sp_m[,`:=`(Unit = as.factor(Unit),
                       TSH = Year - 1991)]
str(MF_trees_sl_sp_m)
dat_sl <- MF_trees_sl_sp_m[Species == "Bl" |Species == "Sx"][val_type == "MgHa"]

model1 <- lmer(val_ha ~ obs_preds * Treatment * Species * TSH + (1 | Unit), 
               data = dat_sl)

model2 <- lmer(val_ha ~ obs_preds * Treatment * Species + TSH + (1 | Unit), 
               data = dat_sl)

model3 <- lmer(val_ha ~ obs_preds * Treatment * TSH + Species + (1 | Unit), 
               data = dat_sl)

model4 <- lmer(val_ha ~ obs_preds * Species * TSH + Treatment + (1 | Unit), 
               data = dat_sl)

model5 <- lmer(val_ha ~ Treatment * Species * TSH + obs_preds + (1 | Unit), 
               data = dat_sl)

model6 <- lmer(val_ha ~ obs_preds * Treatment + Species * TSH + (1 | Unit), 
               data = dat_sl)

model7 <- lmer(val_ha ~ obs_preds * Species + Treatment * TSH + (1 | Unit), 
               data = dat_sl)

model8 <- lmer(val_ha ~ obs_preds * TSH + Treatment * Species + (1 | Unit), 
               data = dat_sl)

model9 <- lmer(val_ha ~ obs_preds + TSH + Treatment * Species + (1 | Unit),
               data = dat_sl)

model10 <- lmer(val_ha ~ Treatment + TSH + obs_preds * Species + (1 | Unit), 
               data = dat_sl)

model11 <- lmer(val_ha ~ Species + TSH + obs_preds * Treatment + (1 | Unit), 
               data = dat_sl)

#MgHa ~ Type * treatment * Species * TSH + (1 | unit)
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
    model11)
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

ggplot()+
  geom_line(aes(x = Year, y = MgHa, group = as.factor(Unit), color = Treatment), 
            data = MSL_trees_dc)+
  #geom_abline(slope = 0, intercept = 56.7754, linetype = "dashed", color = "grey")+
  geom_point(aes(x = Year, y = MgHa, color = Treatment), 
             data = FSL_trees_dc)

MFL_trees_dc <- merge(FSL_trees_dc, MSL_trees_dc, by = c("Unit","Treatment","Year","State"),
                     all.x = TRUE)
setnames(MFL_trees_dc, c("MgHa.x","MgHa.y", "BAHa.x", "BAHa.y"),
         c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred"))

MF_trees_dc_sp <- merge(FSL_trees_dc_sp, MSL_trees_dc_sp, 
                        by = c("Unit","Treatment","Year","State","Species"),
                        all.x = TRUE)
setnames(MF_trees_dc_sp, c("MgHa.x","MgHa.y", "BAHa", "BaHa"),
         c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred"))


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

ggplot()+
  geom_line(aes(x = Year, y = MgHa, group = as.factor(Unit), color = Treatment), 
            data = MSL_trees_dc)+
  #geom_abline(slope = 0, intercept = 56.7754, linetype = "dashed", color = "grey")+
  geom_point(aes(x = Year, y = MgHa, color = Treatment), 
             data = FSL_trees_dc)
# Carbon predicted vs observed 
ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, 
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
    x = "Carbon (Mg/ha) predicted",
    y = "Carbon (Mg/ha) observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,275))+
  ylim(c(0,275))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))#+
  #facet_wrap(~Year)
ggsave(filename = "ICH_live_fit.png",plot = last_plot(), width = 7.91, height = 5.61,
       units = "in", path = file.path(out_path), device='png', dpi=1200)

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
ggsave(filename = "ICH_live_fit_by_year.png",
       path = file.path(out_path, "Supplementary"), device='png', dpi=1200)

years <- c(1992, 1993, 2010, 2018, 2022, "All Years")

results <- lapply(years, function(year) {
  data <- select_years(year,meas_obs = MgHa_obs, meas_pred = MgHa_pred,
                       data = MFL_trees_dc)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Year = years, results_df)
results_df
res_m <- data.table(melt(results_df, id.vars = "Year"))
#is there a significant difference between predicted and observed by 2022?
equi_result(FSL_trees_dc[Year == 2022]$MgHa, 
            MSL_trees_dc[Year == 2022]$MgHa,
            eq_margin = 10)

t.test(FSL_trees_dc[Year == 2022]$MgHa, 
            MSL_trees_dc[Year == 2022]$MgHa) #not diff

#not rejected == they are not different
mean(MSL_trees_dc[Year == 2022]$MgHa)
sd(MSL_trees_dc[Year == 2022]$MgHa)
mean(FSL_trees_dc[Year == 2022]$MgHa)
sd(FSL_trees_dc[Year == 2022]$MgHa)

#by species
MSL_trees_dc_sp_sum <- Rmisc::summarySE(MSL_trees_dc_sp,
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year", "Species"))
MSL_trees_dc_sp_sum <- data.table(MSL_trees_dc_sp_sum)

sp_incl <- c("Cw","Sx","Ba", "Pl")
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

ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(Unit), 
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
    x = "Carbon (Mg/ha) predicted",
    y = "Carbon (Mg/ha) observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,250))+
  ylim(c(0,250))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5, order = 1),  # Treatment legend first
         shape = guide_legend(title.position = "top", title.hjust = 0.5, order = 2)) +  # Species legend second
  guides(color = guide_legend(override.aes = list(size = 5)))+
  facet_wrap(~Year)
ggsave(filename = "ICH_live_FIT_sp_tr_hw.jpg",
       path = file.path(out_path), device='jpeg', dpi=1200)


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
    y = "Carbon (Mg/ha)",
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
    aes(group = Unit, color = Treatment, linetype = Treatment),
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
ggsave(filename = "ICH_live_trees_sp_tr_cw_sx_pl_Ba.jpg",
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
    y = "Carbon (Mg/ha)",
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
    aes(group = Unit, color = Treatment, linetype = Treatment),
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


MF_trees_dc_m[,`:=`(Unit = as.factor(Unit),
                    TSH = Year - 1992)]
dat_dc <- MF_trees_dc_m[Measure == "MgHa"][TSH > 0]

model1 <- lmer(val_ha ~ Type * Treatment * TSH + (1 | Unit), 
               data = dat_dc)

model2 <- lmer(val_ha ~ Type * Treatment + TSH + (1 | Unit), 
               data = dat_dc)

model3 <- lmer(val_ha ~ Treatment * TSH + Type + (1 | Unit), 
               data = dat_dc)

model4 <- lmer(val_ha ~ Type * TSH + Treatment  + (1 | Unit), 
               data = dat_dc)

model5 <- lmer(val_ha ~ Type + TSH + Treatment  + (1 | Unit),
               data = dat_dc)

#MgHa ~ Type * treatment * Species * TSH + (1 | unit)
AIC(model1, model2, model3, model4, model5)
anova(model2)

contrast(emmeans(model2, ~ Type), method = "tukey")

contrast(emmeans(model2, ~ Type | Treatment), method = "tukey")

# this estimates the slope effects and contrasts:
emt <- emtrends(model1, tukey ~ Type, var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)

emt <- emtrends(model1, tukey ~ Treatment, var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)

#MF_trees_dc_m[,`:=`(unit_f = as.factor(unit),
#                   treatment = as.factor(treatment),
#                    State = as.factor(State))]
#str(MF_trees_dc_m)
MF_trees_dc_sp_m[,`:=`(Unit = as.factor(Unit),
                       TSH = Year - 1992)]
str(MF_trees_dc_sp_m)
sp_incl <- c("Cw","Sx","Ba", "Pl","Hw")

dat_dc <- MF_trees_dc_sp_m[Species %in% sp_incl][val_type == "MgHa"][TSH > 0]

model1 <- lmer(val_ha ~ obs_preds * Treatment * Species * TSH + (1 | Unit), 
               data = dat_dc)

model2 <- lmer(val_ha ~ obs_preds * Treatment * Species + TSH + (1 | Unit), 
               data = dat_dc)

model3 <- lmer(val_ha ~ obs_preds * Treatment * TSH + Species + (1 | Unit), 
               data = dat_dc)

model4 <- lmer(val_ha ~ obs_preds * Species * TSH + Treatment + (1 | Unit), 
               data = dat_dc)

model5 <- lmer(val_ha ~ Treatment * Species * TSH + obs_preds + (1 | Unit), 
               data = dat_dc)

model6 <- lmer(val_ha ~ obs_preds * Treatment + Species * TSH + (1 | Unit), 
               data = dat_dc)

model7 <- lmer(val_ha ~ obs_preds * Species + Treatment * TSH + (1 | Unit), 
               data = dat_dc)

model8 <- lmer(val_ha ~ obs_preds * TSH + Treatment * Species + (1 | Unit), 
               data = dat_dc)

model9 <- lmer(val_ha ~ obs_preds + TSH + Treatment * Species + (1 | Unit),
               data = dat_dc)

model10 <- lmer(val_ha ~ Treatment + TSH + obs_preds * Species + (1 | Unit), 
                data = dat_dc)

model11 <- lmer(val_ha ~ Species + TSH + obs_preds * Treatment + (1 | Unit), 
                data = dat_dc)

#MgHa ~ Type * treatment * Species * TSH + (1 | unit)
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
    model11)
anova(model2)
contrast(emmeans(model2, ~ Treatment), method = "tukey")
contrast(emmeans(model1, ~ Species), method = "tukey")
contrast(emmeans(model2, ~ Treatment| Species | obs_preds), method = "tukey")

# this estimates the slope effects and contrasts:
emt <- emtrends(model1, tukey ~ Treatment| Species , var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)

emt <- emtrends(model1, tukey ~ Treatment| Species, var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)


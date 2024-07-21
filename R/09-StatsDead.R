# A.Clason
library(equivalence)
source(file.path("R","00-utils","utils.R"))
library(ggplot2)
library(data.table)
library(lme4)
library(lmerTest)
library(emmeans)

in_path <- "04_out_carbon"
out_path <- "05_out_analysis"

# SBS -------------------------------------------------------------------------------------------
MSD_trees_sl <- readRDS(file.path(in_path,"MSD_trees_sl.RDS"))
FSD_trees_sl <- readRDS(file.path(in_path,"FSD_trees_sl.RDS"))

MSD_trees_sl_sp <- readRDS(file.path(in_path,"MSD_trees_sl_sp.RDS"))
FSD_trees_sl_sp <- readRDS(file.path(in_path,"FSD_trees_sl_sp.RDS"))

ggplot()+
  geom_line(aes(x = Year, y = MgUnit, group = as.factor(unit), color = treatment), 
            data = MSD_trees_sl)+
  geom_point(aes(x = Year, y = MgUnit, color = treatment), 
             data = FSD_trees_sl)

MFD_trees_sl <- merge(FSD_trees_sl, MSD_trees_sl, by = c("unit","treatment","Year","State"),
                     all.x = TRUE)
MFD_trees_sl[is.na(MFD_trees_sl)] <- 0
setnames(MFD_trees_sl, c("MgUnit.x","MgUnit.y"), c("MgHa_obs","MgHa_pred"))

MFD_trees_sl_sp <- merge(FSD_trees_sl_sp, MSD_trees_sl_sp, by = c("unit","treatment","Year","State","Species"),
                        all.x = TRUE)
setnames(MFD_trees_sl_sp, c("MgUnit.x","MgUnit.y"), c("MgHa_obs","MgHa_pred"))
MFD_trees_sl_sp[is.na(MgHa_pred), MgHa_pred := 0]

MFD_trees_sl_m <- melt(MFD_trees_sl, 
                      id.vars = c("unit", "treatment", "Year", "State"),
                      measure.vars = c("MgHa_obs", "MgHa_pred"),
                      variable.name = "Type", 
                      value.name = "MgHa")
MFD_trees_sl_m[, Type := ifelse(Type == "MgHa_obs", "obs", "pred")]
MFD_trees_sl_sp_m <- melt(MFD_trees_sl_sp, 
                         id.vars = c("unit", "treatment", "Year", "State", "Species"),
                         measure.vars = c("MgHa_obs", "MgHa_pred"),
                         variable.name = "Type", 
                         value.name = "MgHa")
MFD_trees_sl_sp_m[, Type := ifelse(Type == "MgHa_obs", "obs", "pred")]

ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(unit), 
                 color = treatment), 
             alpha = 0.9,
             size = 2,
             data = MFD_trees_sl[Year == 2019])+
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
  xlim(c(0,40))+
  ylim(c(0,40))
ggsave(filename = "SBS_fit_dead.jpg",
       path = file.path(out_path, "Supplementary"), device='jpeg', dpi=1000)

MSD_trees_sl_sum <- Rmisc::summarySE(data = MSD_trees_sl[Year < 2092], 
                                     measurevar = "MgUnit", 
                                     groupvars = c("treatment","Year"))
FSD_trees_sl_sum <- Rmisc::summarySE(data = FSD_trees_sl, 
                                     measurevar = "MgUnit", 
                                     groupvars = c("treatment","Year"))

ggplot(NULL,
       aes(x = Year, y = MgUnit, fill = treatment, group = treatment)) +
  geom_line(data = MSD_trees_sl_sum, aes(col = treatment), size = 1.2) +
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
    aes(ymin = MgUnit - ci, ymax = MgUnit + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  geom_point(
    data = FSD_trees_sl_sum,
    aes(shape = treatment),
    size = 5 ,
    position = position_dodge(width = 3)
  ) +
  geom_jitter(
    data = FSD_trees_sl,
    aes(shape = treatment),
    size = 1 ,
    position = position_dodge(width = 3)
  )+
  geom_line(
    data = MSD_trees_sl,
    aes(group = unit, color = treatment),
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
    data = FSD_trees_sl_sum,
    aes(ymin = MgUnit - ci, ymax = MgUnit + ci),
    position = position_dodge(width = 1)
  ) 
ggsave(filename = "SBS_dead_trees.jpg",
       path = file.path(out_path, "Sumpplementary"), device='jpeg', dpi=1000)

FSD_trees_sl_sp_sum <- Rmisc::summarySE(FSD_trees_sl_sp[Species == "Bl"|
                                                          Species == "Sx"],
                                        measurevar = "MgUnit", 
                                        groupvars = c("treatment","Year", "Species"))

MSD_trees_sl_sp_sum <- Rmisc::summarySE(MSD_trees_sl_sp[Species == "Bl" & Year < 2092|
                                                          Species == "Sx"& Year < 2092],
                                        measurevar = "MgUnit", 
                                        groupvars = c("treatment","Year", "Species"))
MSD_trees_sl_sp_sum <- data.table(MSD_trees_sl_sp_sum)

ggplot(NULL,
       aes(x = Year, y = MgUnit, fill = treatment, group = treatment)) +
  geom_line(data = MSD_trees_sl_sp_sum[Species == "Bl"|Species == "Sx"], 
            aes(col = treatment), size = 1.2) +
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
  coord_cartesian(ylim = c(0, 11)) +
  labs(
    x = "Year",
    y = "Carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSD_trees_sl_sp_sum[Species == "Bl"|Species == "Sx"],
    aes(ymin = MgUnit - ci, ymax = MgUnit + ci),
    alpha = 0.25,
    linetype = "solid",
    color = "grey"
  )+
  facet_wrap(~Species)+
  #geom_point(
   # data = FSD_trees_sl_sp_sum,
    #aes(shape = treatment),
    #size = 3,
    #position = position_dodge(width = 0.5)
  #)+ 
 # geom_jitter(
  #  data = FSD_trees_sl_sp[Species == "Bl"|Species == "Sx"],
  #  aes(shape = treatment),
  #  size = 1 ,
  #  position = position_dodge(width = 0.5)
  #)+
  geom_point(
    data = MSD_trees_sl_sp[Species == "Bl"|Species == "Sx"],
    aes(group = interaction(Species, treatment), color = treatment),
    alpha = 0.1#,
    #linetype = "dotted"
    #color = "grey"
  ) +
  scale_shape_manual(
    values = c(24, 23, 21),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  )# +
#geom_errorbar(
#  data = FSL_trees_sl_sp_sum,
#  aes(ymin = MgUnit - ci, ymax = MgUnit + ci),
#  position = position_dodge(width = 1)
#) 
ggsave(filename = "SBS_dead_trees_sp_no_treat_all_yrs.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)

#Stats
#Stats -------------------------------------------------------------------------
years <- c(1992, 1994, 1997, 2009, 2019, "All Years")

results <- lapply(years, function(year) {
  data <- select_years(year, data = MFD_trees_sl)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Year = years, results_df)
results_df

#is there a significant difference between predicted and observed by 2019?
equi_result(FSD_trees_sl[Year == 2019]$MgUnit, 
            MSD_trees_sl[Year == 2019]$MgUnit,
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



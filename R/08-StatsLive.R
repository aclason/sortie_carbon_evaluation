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

MSL_trees_sl_sp <- readRDS(file.path(in_path,"MSL_trees_sl_sp.RDS"))
FSL_trees_sl_sp <- readRDS(file.path(in_path,"FSL_trees_sl_sp.RDS"))

ggplot()+
  geom_line(aes(x = Year, y = MgUnit, group = as.factor(unit), color = treatment), 
            data = MSL_trees_sl[Year < 2021])+
  geom_abline(slope = 0, intercept = 56.7754, linetype = "dashed", color = "grey")+
  geom_point(aes(x = Year, y = MgUnit, color = treatment), 
             data = FSL_trees_sl)

ggplot()+
  geom_line(aes(x = Year, y = MgUnit, group = as.factor(unit), color = as.factor(unit)), 
            data = MSL_trees_sl[Year < 2021])+
  geom_abline(slope = 0, intercept = 56.7754, linetype = "dashed", color = "grey")+
  geom_point(aes(x = Year, y = MgUnit, color = as.factor(unit)), 
             data = FSL_trees_sl)+
  facet_wrap(~treatment)

ggplot()+
  geom_line(aes(x = Year, y = MgUnit, group = as.factor(unit), 
                color = treatment), 
            data = MSL_trees_sl_sp[Species == "Bl"|Species == "Sx"])+
  geom_jitter(aes(x = Year, y = MgUnit, color = treatment), 
              data = FSL_trees_sl_sp[Species == "Bl"|Species == "Sx"])+
  facet_wrap(~Species)

ggplot()+
  geom_line(aes(x = Year, y = MgUnit, group = as.factor(unit), 
                color = as.factor(unit)), 
            data = MSL_trees_sl_sp[Species == "Bl"|Species == "Sx"])+
  geom_jitter(aes(x = Year, y = MgUnit, color = as.factor(unit)), 
              data = FSL_trees_sl_sp[Species == "Bl"|Species == "Sx"])+
  facet_grid(c("Species","treatment"))


MF_trees_sl <- merge(FSL_trees_sl, MSL_trees_sl, by = c("unit","treatment","Year","State"),
                     all.x = TRUE)
setnames(MF_trees_sl, c("MgUnit.x","MgUnit.y"), c("MgHa_obs","MgHa_pred"))

MF_trees_sl_sp <- merge(FSL_trees_sl_sp, MSL_trees_sl_sp, by = c("unit","treatment","Year","State","Species"),
                     all.x = TRUE)
setnames(MF_trees_sl_sp, c("MgUnit.x","MgUnit.y"), c("MgHa_obs","MgHa_pred"))
MF_trees_sl_m <- melt(MF_trees_sl, 
                         id.vars = c("unit", "treatment", "Year", "State"),
                         measure.vars = c("MgHa_obs", "MgHa_pred"),
                         variable.name = "Type", 
                         value.name = "MgHa")
MF_trees_sl_m[, Type := ifelse(Type == "MgHa_obs", "obs", "pred")]
MF_trees_sl_sp_m <- melt(MF_trees_sl_sp, 
                              id.vars = c("unit", "treatment", "Year", "State", "Species"),
                              measure.vars = c("MgHa_obs", "MgHa_pred"),
                              variable.name = "Type", 
                              value.name = "MgHa")
MF_trees_sl_sp_m[, Type := ifelse(Type == "MgHa_obs", "obs", "pred")]

# Carbon predicted vs observed 
ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(unit), 
                 color = treatment), 
             alpha = 0.9,
             size = 2,
            data = MF_trees_sl)+
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
  xlim(c(0,120))+
  ylim(c(0,120))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "SBS_live_fit.png",
       path = file.path(out_path), device='png', dpi=1200)

ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(unit), 
                 color = treatment), alpha = 0.9, 
             data = MF_trees_sl)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  xlim(c(0,120))+
  ylim(c(0,120))+
  scale_color_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
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
ggsave(filename = "SBS_live_fit_by_year.png",
       path = file.path(out_path, "Supplementary"), device='png', dpi=1200)

MSL_trees_sl_sum <- Rmisc::summarySE(data = MSL_trees_sl[Year < 2092], 
                                     measurevar = "MgUnit", 
                                     groupvars = c("treatment","Year"))
FSL_trees_sl_sum <- Rmisc::summarySE(data = FSL_trees_sl, 
                                     measurevar = "MgUnit", 
                                     groupvars = c("treatment","Year"))

ggplot(NULL,
       aes(x = Year, y = MgUnit, fill = treatment, group = treatment)) +
  geom_line(data = MSL_trees_sl_sum, aes(col = treatment), size = 1.2) +
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
  coord_cartesian(ylim = c(0, 180)) +
  labs(
    x = "Year",
    y = "Carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSL_trees_sl_sum,
    aes(ymin = MgUnit - ci, ymax = MgUnit + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  geom_point(
    data = FSL_trees_sl_sum,
    aes(shape = treatment),
    size = 5 ,
    position = position_dodge(width = 3)
  ) +
  geom_jitter(
    data = FSL_trees_sl,
    aes(shape = treatment),
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
  ) +
  geom_errorbar(
    data = FSL_trees_sl_sum,
    aes(ymin = MgUnit - ci, ymax = MgUnit + ci),
    position = position_dodge(width = 1)
  ) 
ggsave(filename = "SBS_live_trees.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)


ggplot()+
  geom_line(aes(x = Year, y = MgUnit, group = as.factor(unit), 
                color = as.factor(unit)), 
            data = MSL_trees_sl_sp[Species == "Bl"|Species == "Sx"])+
  geom_jitter(aes(x = Year, y = MgUnit, color = as.factor(unit)), 
              data = FSL_trees_sl_sp[Species == "Bl"|Species == "Sx"])+
  facet_grid(c("Species","treatment"))


MSL_trees_sl_sp_sum <- Rmisc::summarySE(MSL_trees_sl_sp[Species == "Bl" & Year < 2092|
                                                          Species == "Sx" & Year < 2092],
                                        measurevar = "MgUnit", 
                                        groupvars = c("treatment","Year", "Species"))
                                        
FSL_trees_sl_sp_sum <- Rmisc::summarySE(FSL_trees_sl_sp[Species == "Bl"|
                                                          Species == "Sx"],
                                        measurevar = "MgUnit", 
                                        groupvars = c("treatment","Year", "Species"))

ggplot(NULL,
       aes(x = Year, y = MgUnit, fill = treatment, group = Species)) +
  geom_line(data = MSL_trees_sl_sp_sum, aes(col = treatment), size = 1.2) +
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
    aes(ymin = MgUnit - ci, ymax = MgUnit + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  )+
  facet_grid(c("Species","treatment"))+
  geom_point(
    data = FSL_trees_sl_sp_sum,
    aes(shape = treatment),
    size = 3,
    position = position_dodge(width = 0.5)
  ) +
  geom_jitter(
    data = FSL_trees_sl_sp[Species == "Bl"|Species == "Sx"],
    aes(shape = treatment),
    size = 1 ,
    position = position_dodge(width = 0.5)
  )+
  geom_line(
    data = MSL_trees_sl_sp[Species == "Bl"|Species == "Sx"],
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
    data = FSL_trees_sl_sp_sum,
    aes(ymin = MgUnit - ci, ymax = MgUnit + ci),
    position = position_dodge(width = 1)
  ) 
ggsave(filename = "SBS_live_trees_sp.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)

MSL_trees_sl_sp_sum <- Rmisc::summarySE(MSL_trees_sl_sp[Species == "Bl" & Year < 2022|
                                                          Species == "Sx"& Year < 2022],
                                        measurevar = "MgUnit", 
                                        groupvars = c("treatment","Year", "Species"))
MSL_trees_sl_sp_sum <- data.table(MSL_trees_sl_sp_sum)

ggplot(NULL,
       aes(x = Year, y = MgUnit, fill = treatment, group = treatment)) +
  geom_line(data = MSL_trees_sl_sp_sum[Species == "Bl"|Species == "Sx"], 
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
  coord_cartesian(ylim = c(0, 80)) +
  labs(
    x = "Year",
    y = "Carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSL_trees_sl_sp_sum[Species == "Bl"|Species == "Sx"],
    aes(ymin = MgUnit - ci, ymax = MgUnit + ci),
    alpha = 0.25,
    linetype = "solid",
    color = "grey"
  )+
  facet_wrap(~Species)+
  geom_point(
    data = FSL_trees_sl_sp_sum,
    aes(shape = treatment),
    size = 3,
    position = position_dodge(width = 0.5)
  )+ 
  geom_jitter(
    data = FSL_trees_sl_sp[Species == "Bl"|Species == "Sx"],
    aes(shape = treatment),
    size = 1 ,
    position = position_dodge(width = 0.5)
  )+
  geom_point(
    data = MSL_trees_sl_sp[Species == "Bl" & Year < 2022|Species == "Sx"& Year < 2022],
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
ggsave(filename = "SBS_live_trees_sp_no_treat.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)
#Stats -------------------------------------------------------------------------
years <- c(1992, 1994, 1997, 2009, 2019, "All Years")

results <- lapply(years, function(year) {
  data <- select_years(year, data = MF_trees_sl)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Year = years, results_df)
results_df
res_m <- data.table(melt(results_df, id.vars = "Year"))
#cohen d: positive value - actual is XX MgHa higher than predicted

ggplot()+
  geom_line(aes(x = as.factor(Year), y = as.numeric(value), 
                group = as.factor(variable), color = variable), 
            data = res_m[Year != "All Years" & variable != "RMSE"])+
  geom_point(aes(x = as.factor(Year), y = as.numeric(value), 
                group = as.factor(variable), color = variable, size =2), 
            data = res_m[Year == "All Years" & variable != "RMSE"])
sp <- c("Bl","Sx")

results <- lapply(sp, function(sp) {
  data <- select_sp(sp, data = MF_trees_sl_sp)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df

ts <- c("light/no","med","heavy")

results <- lapply(ts, function(ts) {
  data <- select_ts(ts, data = MF_trees_sl)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = ts, results_df)
results_df

ts <- c("light/no","med","heavy")
sp <- c("Bl","Sx")
years <- c(1992, 1994, 1997, 2009, 2019, "All Years")

results <- lapply(years, function(ts, sp, years) {
  data <- select_all(ts, data = MF_trees_sl)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = ts, results_df)
results_df

#is there a significant difference between predicted and observed by 2019?
equi_result(FSL_trees_sl[Year == 2019]$MgUnit, 
            MSL_trees_sl[Year == 2019]$MgUnit,
            eq_margin = 0.5)
#not rejected == they are not different
mean(MSL_trees_sl[Year == 2019]$MgUnit)
sd(MSL_trees_sl[Year == 2019]$MgUnit)
mean(FSL_trees_sl[Year == 2019]$MgUnit)
sd(FSL_trees_sl[Year == 2019]$MgUnit)

equi_boot(actual = FSL_trees_sl[Year == 2019]$MgUnit, 
          predicted = MSL_trees_sl[Year == 2019]$MgUnit,
          n_bootstraps = 1000,
          eq_margin = 0.5)

MF_trees_sl_m[,`:=`(unit_f = as.factor(unit),
                    treatment = as.factor(treatment),
                    State = as.factor(State))]
str(MF_trees_sl_m)
MF_trees_sl_sp_m[,`:=`(unit = as.factor(unit),
                       TSH = Year - 1992)]
str(MF_trees_sl_sp_m)


model1 <- lmer(MgHa ~ Type * treatment * Species * TSH + (1 | unit), 
               data = MF_trees_sl_sp_m[Species == "Bl" |Species == "Sx"])

model2 <- lmer(MgHa ~ Type * treatment * Species + TSH + (1 | unit), 
               data = MF_trees_sl_sp_m[Species == "Bl" |Species == "Sx"])

model3 <- lmer(MgHa ~ Type * treatment * TSH + Species + (1 | unit), 
               data = MF_trees_sl_sp_m[Species == "Bl" |Species == "Sx"])

model4 <- lmer(MgHa ~ Type * Species * TSH + treatment + (1 | unit), 
               data = MF_trees_sl_sp_m[Species == "Bl" |Species == "Sx"])

model5 <- lmer(MgHa ~ treatment * Species * TSH + Type + (1 | unit), 
               data = MF_trees_sl_sp_m[Species == "Bl" |Species == "Sx"])

model6 <- lmer(MgHa ~ Type * treatment + Species * TSH + (1 | unit), 
               data = MF_trees_sl_sp_m[Species == "Bl" |Species == "Sx"])

model7 <- lmer(MgHa ~ Type * Species + treatment * TSH + (1 | unit), 
               data = MF_trees_sl_sp_m[Species == "Bl" |Species == "Sx"])

model8 <- lmer(MgHa ~ Type * TSH + treatment * Species + (1 | unit), 
               data = MF_trees_sl_sp_m[Species == "Bl" |Species == "Sx"])

model9 <- lmer(MgHa ~ Type + TSH + treatment * Species + (1 | unit),
               data = MF_trees_sl_sp_m[Species == "Bl" |Species == "Sx"])

model10 <- lmer(MgHa ~ treatment + TSH + Type * Species + (1 | unit), 
               data = MF_trees_sl_sp_m[Species == "Bl" |Species == "Sx"])

model11 <- lmer(MgHa ~ Species + TSH + Type * treatment + (1 | unit), 
               data = MF_trees_sl_sp_m[Species == "Bl" |Species == "Sx"])

#MgHa ~ Type * treatment * Species * TSH + (1 | unit)
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
    model11)
summary(model5)
anova(model5)
fixef(model5)
contrast(emmeans(model5, ~ treatment), method = "tukey")
contrast(emmeans(model5, ~ Species | TSH), method = "tukey")

# this estimates the slope effects and contrasts:
emt <- emtrends(model5, tukey ~ Species, var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)


#i still think Spruce isn't growing fast enough in the heavy removal. Tried a bunch of things - 
#go back to 05/06
#heavy vs light is significantly different for Bl, but not Sx, and same for med vs. light/no
mean(MSL_trees_sl[Year ==1992 & treatment == "light/no"]$MgUnit)
#59 MgHa at 1992
mean(MSL_trees_sl[Year == 2009 & treatment == "med"]$MgUnit)
#by 20 years after harvest, med treatment regained carbon
mean(MSL_trees_sl[Year == 2022 & treatment == "heavy"]$MgUnit)
#by 30 years after harvest, heavy treatment regained carbon

#just modelled - all 100 years
model1 <- lm(MgUnit ~ treatment * Species * Year, MSL_trees_sl_sp[Species == "Sx"|Species == "Bl"])
anova(model1)
contrast(emmeans(model5, ~ treatment|Species), method = "tukey")
contrast(emmeans(model5, ~ Species | TSH), method = "tukey")

#at 100 years - what is the difference in carbon in treatments
summary(lm(MgUnit ~ treatment-1, MSL_trees_sl[Year == 2091]))
anova(lm(MgUnit ~ treatment-1, MSL_trees_sl[Year == 2091]))

ggplot()+
  geom_boxplot(aes(y = MgUnit, x =treatment, fill = treatment), 
               data = MSL_trees_sl[Year == 2091])
ggplot()+
  geom_boxplot(aes(y = MgUnit, x =treatment, fill = treatment), 
               data = MSL_trees_sl[Year == 2051])
ggplot()+
  geom_boxplot(aes(y = MgUnit, x =treatment, fill = treatment), 
               data = MSL_trees_sl[Year == 1992])
# after 100 years, the carbon in the no/light treatment remains significantly
# higher than med & heavy treatments, but the heavy and medium treatments are
# no longer significantly different? 
# looks like at about 50 years, the medium and heavy harvests merge



#Date creek ----------------------------------------------------------------------------------------


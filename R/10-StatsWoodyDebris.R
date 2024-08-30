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

MS_cwd_sl <- readRDS(file.path(in_path,"MS_cwd_sl.RDS"))
MS_cwd_sl[,Unit:=as.character(unit)][, unit := NULL] #deal with this earlier
MS_cwd_sl_dc <- readRDS(file.path(in_path, "MS_cwd_sl_dc.RDS"))
FS_cwd_sl <- readRDS(file.path(in_path,"FS_cwd_sl.RDS"))
#FS_cwd_sl_sg <- readRDS(file.path(in_path,"FS_cwd_sl_sg.RDS")) #by species groups


setkey(FS_cwd_sl,Unit)
setkey(MS_cwd_sl,Unit)

#SBS
MS_cwd_sl
FS_cwd_sl

ggplot(MS_cwd_sl, aes(x=Year, y = MgHa, group = Unit, colour = treatment))+
  geom_line(linewidth=1)+
  stat_summary(geom = "ribbon", fun.min = min, fun.max = max, alpha = 0.1) 
ggsave(filename = "cwd_SBS.jpg",
       path = file.path(out_path,"Supplementary"), device='jpeg', dpi=1000, bg="white")
#Volume  
ggplot(NULL)+
  geom_jitter(data = FS_cwd_sl, aes(x= Year, y = VolHa, colour = treatment))+
  geom_line(data = MS_cwd_sl, aes(x= Year, y = VolHa, 
                                  group = Unit, colour = treatment), linetype = 1)
#geom_point(data = FS_cwd_dc, aes(x= Yrs_Post, y = VolHa, colour = Treatment))
#geom_jitter(data = FS_cwd_sl_s, aes(x=timestep, y = VolHa, colour = treatment),
 #           size = 1, shape = 2)

#Carbon 
ggplot(NULL)+
  geom_jitter(data = FS_cwd_sl, aes(x= Year, y = MgHa, colour = treatment))+
  geom_line(data = MS_cwd_sl, aes(x= Year, y = MgHa, 
                                  group = Unit, colour = treatment), linetype = 1)#
#geom_point(data = FS_cwd_dc, aes(x= Yrs_Post, y = MgHa, colour = Treatment))
#geom_jitter(data = FS_cwd_sl_s, aes(x=timestep, y = MgHa, colour = treatment),
 #           size = 1, shape = 2)
MF_wd_sl <- merge(FSL_trees_sl, MSL_trees_sl, by = c("unit","treatment","Year","State"),
                     all.x = TRUE)
setnames(MF_wd_sl, c("MgUnit.x","MgUnit.y"), c("MgHa_obs","MgHa_pred"))

# Carbon predicted vs observed 
MS_cwd_sl[Year == 2020]$MgHa
FS_cwd_sl$MgHa
ggplot()+
  geom_point(aes(x = , y = , group = as.factor(unit), 
                 color = treatment), 
             alpha = 0.9,
             size = 2)+
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
  xlim(c(0,120))+
  ylim(c(0,120))
ggsave(filename = "SBS_cwd_fit.png",
       path = file.path(out_path), device='png', dpi=1200)

ggplot(MS_cwd_sl_dc) +
  geom_line(aes(x = Year, y = MgHa, group = unit, colour = treatment))+
  facet_wrap(~as.factor(DecayClass))

ggsave(filename = "cwd_decay_rates_SBS.jpg",
       path = file.path(out_path,"Supplementary"), device='jpeg', dpi=1000, bg="white")


MF_wd_sl_sp <- merge(FSL_trees_sl_sp, MSL_trees_sl_sp, by = c("unit","treatment","Year","State","Species"),
                     all.x = TRUE)
setnames(MF_wd_sl_sp, c("MgUnit.x","MgUnit.y"), c("MgHa_obs","MgHa_pred"))
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

#ICH -------------------------------------------------------------------------------
#Date Creek
MS_cwd_dc <- readRDS(file.path(in_path,"MS_cwd_dc.RDS"))
FS_cwd_dc <- readRDS(file.path(in_path,"FS_cwd_dc.RDS"))

MS_cwd_dc_sg <- readRDS(file.path(in_path,"MS_cwd_dc.RDS"))
FS_cwd_dc_sg <- readRDS(file.path(in_path,"FS_cwd_dc.RDS"))

# Modelled:
#By pixel id - summ all the decay class volumes and carbon together
M_cwd_dc <- cwdc_dc[,.(pixMgHa = sum(MgHa), pixVolHa = sum(values)),
                    by = c("Treatment","Unit","timestep","ID")]
#Add then take the average value across the whole unit
MS_cwd_dc <- M_cwd_dc[,.(MgHa = mean(pixMgHa), VolHa = mean(pixVolHa)),
                      by = c("Treatment","Unit","timestep")]


#MS_cwd_dc[, Yrs_Post:= as.numeric(timestep)]
setnames(MS_cwd_dc, "timestep", "Yrs_Post")

ggplot(MS_cwd_dc)+
  geom_line(aes(x=Yrs_Post, y = MgHa, group = Unit, colour = Treatment))





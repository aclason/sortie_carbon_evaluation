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

ggplot(MS_cwd_sl_dc) +
  geom_line(aes(x = Year, y = MgHa, group = unit, colour = treatment))+
  facet_wrap(~as.factor(DecayClass))

ggsave(filename = "cwd_decay_rates_SBS.jpg",
       path = file.path(out_path,"Supplementary"), device='jpeg', dpi=1000, bg="white")

#ICH -------------------------------------------------------------------------------
#Date Creek
MS_cwd_dc <- readRDS(file.path(in_path,"MS_cwd_dc.RDS"))
FS_cwd_dc <- readRDS(file.path(in_path,"FS_cwd_dc.RDS"))


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





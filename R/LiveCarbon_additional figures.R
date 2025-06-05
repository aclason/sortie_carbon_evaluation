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
MF_trees_sl_sp[is.na(MgHa_pred), MgHa_pred := 0]

# summary by treatment and year
MSL_trees_sl_sum_ty <- Rmisc::summarySE(data = MSL_trees_sl, 
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year"))
MSL_trees_sl_sum_ty <- MSL_trees_sl_sum[MSL_trees_sl_sum$Year < 2093,]
FSL_trees_sl_sum_ty <- Rmisc::summarySE(data = FSL_trees_sl, 
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year"))
# summary by year
MSL_trees_sl_sum <- Rmisc::summarySE(data = MSL_trees_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Year"))
FSL_trees_sl_sum <- Rmisc::summarySE(data = FSL_trees_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Year"))

# summary by treatment, year and species for Bl and Sx
MSL_trees_sl_sp_sum_blsx <- Rmisc::summarySE(MSL_trees_sl_sp[Species == "Bl" |
                                                               Species == "Sx" ],
                                             measurevar = "MgHa", 
                                             groupvars = c("Treatment","Year", "Species"))
FSL_trees_sl_sp_sum_blsx <- Rmisc::summarySE(FSL_trees_sl_sp[Species == "Bl"|
                                                               Species == "Sx"],
                                             measurevar = "MgHa", 
                                             groupvars = c("Treatment","Year", "Species"))

# summary by treatment, year and species for all species
MSL_trees_sl_sp_sum <- Rmisc::summarySE(MSL_trees_sl_sp,
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year", "Species"))
MSL_trees_sl_sp_sum <- data.table(MSL_trees_sl_sp_sum)



# Figures -----------------------------------------------------------------------------------------
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
    x = "Live carbon (Mg/ha) predicted",
    y = "Live carbon (Mg/ha) observed",
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
ggsave(filename = "SBS_live_fit_yr.png",plot = last_plot(), width = 7.91, height = 5.61,
       units = "in", path = file.path(out_path, "Supplementary"), device='png', dpi=1200)
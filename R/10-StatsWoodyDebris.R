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
FS_cwd_sl <- readRDS(file.path(in_path,"FS_cwd_sl.RDS"))

#MS_cwd_sl[,Unit:=as.character(unit)][, unit := NULL] #deal with this earlier
MS_cwd_sl_dc_sg <- readRDS(file.path(in_path, "MS_cwd_sl_dc_sg.RDS"))
FS_cwd_sl_dc_sg <- readRDS(file.path(in_path,"FS_cwd_sl_dc_sg.RDS"))
#FS_cwd_sl_sg <- readRDS(file.path(in_path,"FS_cwd_sl_sg.RDS")) #by species groups
setnames(MS_cwd_sl_dc_sg, "unit","Unit")
setnames(MS_cwd_sl, "unit","Unit")
setnames(FS_cwd_sl_dc_sg, "Decay","DecayClass")


setkey(FS_cwd_sl,Unit)
setkey(MS_cwd_sl,Unit)

MS_cwd_sl[, Year:= timestep+1992]
FS_cwd_sl

MF_cwd_sl <- merge(FS_cwd_sl, MS_cwd_sl, by = c("Unit","treatment","Year"),
                      all.x = TRUE)
setnames(MF_cwd_sl, c("MgHa.x","MgHa.y", "VolHa.x", "VolHa.y"), 
         c("MgHa_obs","MgHa_pred","VolHa_obs","VolHa_pred"))
#MF_cwd_sl[is.na(MgHa.x), `:=`(VolHa.x MgHa.x = )]


#ggplot(MS_cwd_sl, aes(x=Year, y = MgHa, group = Unit, colour = treatment))+
 # geom_line(linewidth=1)+
#  stat_summary(geom = "ribbon", fun.min = min, fun.max = max, alpha = 0.1) 
#ggsave(filename = "cwd_SBS.jpg",
 #      path = file.path(out_path,"Supplementary"), device='jpeg', dpi=1000, bg="white")
#Volume  
#ggplot(NULL)+
#  geom_jitter(data = FS_cwd_sl, aes(x= Year, y = VolHa, colour = treatment))+
#  geom_line(data = MS_cwd_sl, aes(x= Year, y = VolHa, 
 #                                 group = Unit, colour = treatment), linetype = 1)
#geom_point(data = FS_cwd_dc, aes(x= Yrs_Post, y = VolHa, colour = Treatment))
#geom_jitter(data = FS_cwd_sl_s, aes(x=timestep, y = VolHa, colour = treatment),
 #           size = 1, shape = 2)

#Carbon 
#ggplot(NULL)+
#  geom_jitter(data = FS_cwd_sl, aes(x= Year, y = MgHa, colour = treatment))+
#  geom_line(data = MS_cwd_sl, aes(x= Year, y = MgHa, 
#                                  group = Unit, colour = treatment), linetype = 1)#
#geom_point(data = FS_cwd_dc, aes(x= Yrs_Post, y = MgHa, colour = Treatment))
#geom_jitter(data = FS_cwd_sl_s, aes(x=timestep, y = MgHa, colour = treatment),
 #           size = 1, shape = 2)


# Carbon predicted vs observed 
MS_cwd_sl[Year == 2020]$MgHa
FS_cwd_sl$MgHa
ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(Unit), 
                 color = treatment), 
             alpha = 0.9,
             size = 2,
             data = MF_cwd_sl)+
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
  xlim(c(0,30))+
  ylim(c(0,30))
ggsave(filename = "SBS_cwd_fit.png",
       path = file.path(out_path), device='png', dpi=1200)



MS_cwd_sl_sum <- Rmisc::summarySE(data = MS_cwd_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("treatment","Year"))
FS_cwd_sl_sum <- Rmisc::summarySE(data = FS_cwd_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("treatment","Year"))

ggplot(NULL,
       aes(x = Year, y = MgHa, fill = treatment, group = treatment)) +
  geom_line(data = MS_cwd_sl_sum, aes(col = treatment), size = 1.2) +
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
  coord_cartesian(ylim = c(0, 30)) +
  labs(
    x = "Year",
    y = "Carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MS_cwd_sl_sum,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  geom_point(
    data = FS_cwd_sl,
    aes(shape = treatment),
    size = 1 ,
    position = position_dodge(width = 3)
  )+
  scale_shape_manual(
    values = c(24, 23, 21),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  )+
  #xlim(c(1990, 2022))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave(filename = "SBS_cwd_time.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)


MS_cwd_sl_dc_sp_sum <- Rmisc::summarySE(MS_cwd_sl_dc_sg,
                                        measurevar = "MgHa", 
                                        groupvars = c("treatment","Year", "DecayClass","SpGrp"))
MS_cwd_sl_dc_sp_sum <- data.table(MS_cwd_sl_dc_sp_sum)
FS_cwd_sl_dc_sp_sum <- Rmisc::summarySE(data = FS_cwd_sl_dc_sg, 
                                  measurevar = "MgHa", 
                                  groupvars = c("treatment","Year", "DecayClass","SpGrp"))


ggplot(NULL,
       aes(x = Year, y = MgHa, fill = treatment, group = treatment)) +
  geom_line(data = MS_cwd_sl_dc_sp_sum, 
            aes(col = treatment), size = 1.8) +
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
  coord_cartesian(ylim = c(0, 20)) +
  labs(
    x = "Year",
    y = "Carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment",
    linetype = "Treatment"
  ) +
  facet_grid(c("SpGrp","DecayClass"))+#, labeller = as_labeller(c("Bl" = "Fir",
                                      #              "Sx" = "Spruce")))+
  geom_jitter(
    data = FS_cwd_sl_dc_sg,
    aes(shape = treatment),
    size = 2 ,
    position = position_dodge(width = 1)
  )+ 
  geom_line(
    data = MS_cwd_sl_dc_sg,
    aes(group = Unit, color = treatment, linetype = treatment),
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
ggsave(filename = "SBS_cwd_dc_sg_tr.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)




ggplot(MS_cwd_sl_dc_sg) +
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

MS_cwd_dc_dc_sg <- readRDS(file.path(in_path,"MS_cwd_dc_dc_sg.RDS"))
FS_cwd_dc_dc_sg <- readRDS(file.path(in_path,"FS_cwd_dc_dc_sg.RDS"))


MF_cwd_dc <- merge(FS_cwd_dc, MS_cwd_dc, by = c("Unit","Treatment","Year"),
                   all.x = TRUE)
setnames(MF_cwd_dc, c("MgHa.x","MgHa.y", "VolHa.x", "VolHa.y"), 
         c("MgHa_obs","MgHa_pred","VolHa_obs","VolHa_pred"))
#MF_cwd_sl[is.na(MgHa.x), `:=`(VolHa.x MgHa.x = )]


ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(Unit), 
                 color = Treatment), 
             alpha = 0.9,
             size = 2,
             data = MF_cwd_dc)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  labs(
    x = "Carbon (Mg/ha) predicted",
    y = "Carbon (Mg/ha) observed",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,50))+
  ylim(c(0,50))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5, order = 1),  # Treatment legend first
         shape = guide_legend(title.position = "top", title.hjust = 0.5, order = 2)) +  # Species legend second
  guides(color = guide_legend(override.aes = list(size = 5))) 

 ggsave(filename = "ICH_cwd_fit.png",
       path = file.path(out_path), device='png', dpi=1200)


ggplot()+
 geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(Unit), 
                color = Treatment), 
            alpha = 0.9,
            size = 2,
            data = MF_cwd_dc)+
 geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
 scale_color_manual(
   values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
   breaks = c("NH","LR", "HR", "CC"),
   labels = c("No harvest","High retention", "Medium retention", "No retention")
 ) +
 labs(
   x = "Carbon (Mg/ha) predicted",
   y = "Carbon (Mg/ha) observed",
   col = "Treatment",
   fill = "Treatment",
   shape = "Treatment"
 ) +
 xlim(c(0,50))+
 ylim(c(0,50))+ 
 scale_shape_manual(
    values = c(24, 23, 21, 25),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
 theme(legend.position = "bottom")+
 facet_wrap(~Year)+
 guides(color = guide_legend(title.position = "top", title.hjust = 0.5, order = 1),  # Treatment legend first
        shape = guide_legend(title.position = "top", title.hjust = 0.5, order = 2)) +  # Species legend second
 guides(color = guide_legend(override.aes = list(size = 5))) 

ggsave(filename = "ICH_cwd_fit.png",
      path = file.path(out_path), device='png', dpi=1200)


MS_cwd_dc_sum <- Rmisc::summarySE(data = MS_cwd_dc, 
                                  measurevar = "MgHa", 
                                  groupvars = c("Treatment","Year"))
FS_cwd_dc_sum <- Rmisc::summarySE(data = FS_cwd_dc, 
                                  measurevar = "MgHa", 
                                  groupvars = c("Treatment","Year"))

ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MS_cwd_dc_sum, aes(col = Treatment), size = 1.2) +
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
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    x = "Year",
    y = "Carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MS_cwd_dc_sum,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  geom_point(
    data = FS_cwd_dc,
    aes(shape = Treatment),
    size = 1 ,
    position = position_dodge(width = 3)
  )+
  scale_shape_manual(
    values = c(24, 23, 21, 25),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  #xlim(c(1990, 2022))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave(filename = "ICH_cwd_time.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)


MS_cwd_dc_dc_sp_sum <- Rmisc::summarySE(MS_cwd_dc_dc_sg,
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year", "DecayClass","SpGrp"))
MS_cwd_dc_dc_sp_sum <- data.table(MS_cwd_dc_dc_sp_sum)
FS_cwd_dc_dc_sp_sum <- Rmisc::summarySE(data = FS_cwd_dc_dc_sg, 
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year", "DecayClass","SpGrp"))


ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MS_cwd_dc_dc_sp_sum, 
            aes(col = Treatment), size = 0.8) +
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
  coord_cartesian(ylim = c(0, 30)) +
  labs(
    x = "Year",
    y = "Carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment",
    linetype = "Treatment"
  ) +
  facet_grid(c("SpGrp","DecayClass"))+#, labeller = as_labeller(c("Bl" = "Fir",
  #              "Sx" = "Spruce")))+
  geom_jitter(
    data = FS_cwd_dc_dc_sg,
    aes(shape = Treatment),
    size = 2 ,
    position = position_dodge(width = 1)
  )+ 
  geom_line(
    data = MS_cwd_dc_dc_sg,
    aes(group = Unit, color = Treatment, linetype = Treatment),
    alpha = 0.3,
    size = 0.8
  ) +
  scale_shape_manual(
    values = c(24, 23, 21, 25),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
#  scale_linetype_manual(
 #   values = c("solid", "dashed", "dotted"),
  #  breaks = c("light/no", "med", "heavy"),
   # labels = c("High retention", "Medium retention", "Low retention")
#  )+
  theme(legend.position = "bottom")+
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(size = 5)),
    shape = guide_legend(override.aes = list(size = 5)),
    linetype = guide_legend(override.aes = list(size = 5))) 
ggsave(filename = "ICH_cwd_dc_sg_tr.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)

ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment)) +
  geom_line(data = MS_cwd_dc_dc_sp_sum, 
            aes(col = Treatment, group = interaction(SpGrp, Treatment),
                linetype = as.factor(SpGrp)), 
            size = 0.1) +
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
  coord_cartesian(ylim = c(0, 30)) +
  labs(
    x = "Year",
    y = "Carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment",
    linetype = "Treatment"
  ) +
  facet_wrap(~DecayClass)+#, labeller = as_labeller(c("Bl" = "Fir",
  #              "Sx" = "Spruce")))+
  geom_jitter(
    data = FS_cwd_dc_dc_sg,
    aes(shape = Treatment),
    size = 2 ,
    position = position_dodge(width = 1)
  )+ 
  #geom_line(
   # data = MS_cwd_dc_dc_sg,
    #aes(group = interaction(SpGrp, Unit), color = Treatment, linetype = Treatment),
    #alpha = 0.3,
    #size = 0.8
#  ) +
  scale_shape_manual(
    values = c(24, 23, 21, 25),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  #  scale_linetype_manual(
  #   values = c("solid", "dashed", "dotted"),
  #  breaks = c("light/no", "med", "heavy"),
  # labels = c("High retention", "Medium retention", "Low retention")
  #  )+
  theme(legend.position = "bottom")+
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(size = 5)),
    shape = guide_legend(override.aes = list(size = 5)),
    linetype = guide_legend(override.aes = list(size = 5))) 











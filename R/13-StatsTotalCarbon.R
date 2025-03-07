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

# Live -------------------------------------------------------------------------------------------
MSL_trees_sl <- readRDS(file.path(in_path,"MSL_trees_sl.RDS"))
FSL_trees_sl <- readRDS(file.path(in_path,"FSL_trees_sl.RDS"))
MSL_trees_sl[,`:=`(pred_obs = "Predicted", CarbonPool = "Live")]
FSL_trees_sl[,`:=`(pred_obs = "Observed", CarbonPool = "Live")]

MSD_trees_sl <- readRDS(file.path(in_path,"MSD_trees_sl.RDS"))
FSD_trees_sl <- readRDS(file.path(in_path,"FSD_trees_sl.RDS"))
MSD_trees_sl[,`:=`(pred_obs = "Predicted", CarbonPool = "Dead")]
FSD_trees_sl[,`:=`(pred_obs = "Observed", CarbonPool = "Dead")]

MS_cwd_sl <- readRDS(file.path(in_path,"MS_cwd_sl.RDS"))
FS_cwd_sl <- readRDS(file.path(in_path,"FS_cwd_sl.RDS"))
MS_cwd_sl[,`:=`(pred_obs = "Predicted", CarbonPool = "Down")]
FS_cwd_sl[,`:=`(pred_obs = "Observed", CarbonPool = "Down")]
setnames(MS_cwd_sl, c("unit","treatment"),
         c("Unit","Treatment"))
setnames(FS_cwd_sl, c("treatment"),
         c("Treatment"))


all_pools <- rbind(MSL_trees_sl[,.(Unit, Treatment, Year, pred_obs, CarbonPool, MgHa)],
                   FSL_trees_sl[,.(Unit, Treatment, Year, pred_obs, CarbonPool, MgHa)],
                   MSD_trees_sl[,.(Unit, Treatment, Year, pred_obs, CarbonPool, MgHa)],
                   FSD_trees_sl[,.(Unit, Treatment, Year, pred_obs, CarbonPool, MgHa)],
                   MS_cwd_sl[,.(Unit, Treatment, Year, pred_obs, CarbonPool, MgHa)],
                   FS_cwd_sl[,.(Unit, Treatment, Year, pred_obs, CarbonPool, MgHa)])
all_pools[,`:=`(TSH = ifelse(Unit == 4, Year - 1994, 
                          ifelse(Unit == 15, Year - 1994, 
                                 Year - 1992)))]

all_pools[, TSI_cat := as.numeric(as.character(cut(TSH,
                          breaks = seq(0, 100, by = 5), 
                          labels = seq(5, 100, by = 5),
                          right = TRUE,
                          include.lowest = TRUE)))]
#all_pools[,TSI_cat := as.numeric(TSI_cat*5)]

all_means <- all_pools[,.(mn_MgHa = mean(MgHa)),
                       by=c("pred_obs","CarbonPool","TSI_cat")]
all_means[, CarbonPool := factor(CarbonPool, 
                                 levels = c("Live", "Dead", "Down"))]

ggplot(all_means)+
  geom_area(aes(x = TSI_cat, y = mn_MgHa, fill = CarbonPool))+
  facet_grid(rows="pred_obs")+
  #facet_wrap("pred_obs")+
  scale_fill_discrete(name="Carbon Pool",
                 labels=c("Live","Dead","Down"))+
  ylab(expression("Carbon Mg" ~ ha^-1))+
  xlab("Time since harvest (years)")+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank())

ggplot() +
  # Modeled areas
  geom_area(data = all_means[pred_obs == "Predicted"],
            aes(x = TSI_cat, y = mn_MgHa, fill = CarbonPool),
            alpha = 0.6) +
  
  # Observed areas
  geom_area(data = all_means[pred_obs == "Observed"], 
            aes(x = TSI_cat, y = mn_MgHa, color = CarbonPool), 
            alpha = 0.6) +
  scale_fill_discrete(name = "Predicted Carbon Pools",
                      labels = c("Dead", "Down", "Live")) +
  scale_color_discrete(name = "Observed Carbon Pools",
                       labels = c("Dead", "Down", "Live")) +
  ylab(expression("Carbon Mg" ~ ha^-1)) +
  xlab("Time since harvest (years)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        text = element_text(family = "Arial"),  
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"))+
  guides(
    color = guide_legend(title.position = "top", 
                         title.hjust = 0.5, override.aes = list(size = 5)),
    shape = guide_legend(override.aes = list(size = 5)),
    linetype = guide_legend(override.aes = list(size = 5)))


ggplot() +
  # Modeled areas 
  geom_area(data = all_means[pred_obs == "Predicted"],
            aes(x = TSI_cat, y = mn_MgHa, fill = CarbonPool),
            alpha = 0.5) +
  
  # Observed areas 
  geom_area(data = all_means[pred_obs == "Observed"], 
            aes(x = TSI_cat, y = mn_MgHa, fill = CarbonPool),
            alpha = 1) +
  
  # Custom fill scales
  scale_fill_manual(
    name = "Carbon Pools",
    values = c(
      "Dead" = "#D55E00",   # Predicted: Orange
      "Down" = "#0072B2",   # Predicted: Blue
      "Live" = "#009E73"   # Predicted: Green
    ),
    labels = c("Live","Dead", "Down")
  ) +
  
  ylab(expression("Carbon Mg" ~ ha^-1)) +
  xlab("Time since harvest (years)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        text = element_text(family = "Arial"),  
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"))+
  guides(
    color = guide_legend(title.position = "top", 
                         title.hjust = 0.5, override.aes = list(size = 5)),
    shape = guide_legend(override.aes = list(size = 5)),
    linetype = guide_legend(override.aes = list(size = 5)))

all_means <- all_pools[,.(mn_MgHa = mean(MgHa)),
                       by=c("pred_obs","CarbonPool","TSI_cat","Treatment")]
all_means[, CarbonPool := factor(CarbonPool, 
                                 levels = c("Live", "Dead", "Down"))]
all_means[, Treatment := factor(Treatment, 
                                levels = c("light/no", "med", "heavy"))]

sl_all <- ggplot() +
  # Modeled areas 
  geom_area(data = all_means[pred_obs == "Predicted"],
            aes(x = TSI_cat, y = mn_MgHa, fill = CarbonPool),
            alpha = 0.5) +
  
  # Observed areas 
  geom_area(data = all_means[pred_obs == "Observed"], 
            aes(x = TSI_cat, y = mn_MgHa, fill = CarbonPool),
            alpha = 1) +
  facet_grid(rows = "Treatment",labeller = labeller(
    Treatment = c("light/no" = "High retention", 
                  "med" = "Medium retention", 
                  "heavy" = "Low retention")
  ))+
  # Custom fill scales
  scale_fill_manual(
    name = "Carbon Pools",
    values = c(
      "Dead" = "#D55E00",   # Predicted: Orange
      "Down" = "#0072B2",   # Predicted: Blue
      "Live" = "#009E73"   # Predicted: Green
    ),
    labels = c("Live","Dead", "Down")
  ) +
  ylab(expression("Carbon Mg" ~ ha^-1)) +
  xlab("Time since harvest (years)") +
  ylim(c(0,250))+
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        text = element_text(family = "Arial"),  
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"))+
  guides(
    color = guide_legend(title.position = "top", 
                         title.hjust = 0.5, override.aes = list(size = 5)),
    shape = guide_legend(override.aes = list(size = 5)),
    linetype = guide_legend(override.aes = list(size = 5)))

#ggsave(filename = "SBS_all_area_tr.png",plot = last_plot(),
 #      path = file.path(out_path), device='png', dpi=1200)





all_pools


MSL_trees_sl_sum <- Rmisc::summarySE(data = MSL_trees_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Treatment","Year"))
MSL_trees_sl_sum <- MSL_trees_sl_sum[MSL_trees_sl_sum$Year < 2093,]
FSL_trees_sl_sum <- Rmisc::summarySE(data = FSL_trees_sl, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Treatment","Year"))

ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSL_trees_sl_sum, aes(col = Treatment), size = 1.2) +
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
  coord_cartesian(ylim = c(0, 160)) +
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
  geom_point(
    data = FSL_trees_sl,
    aes(shape = Treatment),
    size = 1 ,
    position = position_dodge(width = 3)
  )+
  scale_shape_manual(
    values = c(24, 23, 21),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  )+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "SBS_live_trees.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)

# ICH -------------------------------------------------------------------------------------------
MSL_trees_dc <- readRDS(file.path(in_path,"MSL_trees_dc.RDS"))
FSL_trees_dc <- readRDS(file.path(in_path,"FSL_trees_dc.RDS"))
MSL_trees_dc[,`:=`(pred_obs = "Predicted", CarbonPool = "Live")]
FSL_trees_dc[,`:=`(pred_obs = "Observed", CarbonPool = "Live")]

MSD_trees_dc <- readRDS(file.path(in_path,"MSD_trees_dc.RDS"))
FSD_trees_dc <- readRDS(file.path(in_path,"FSD_trees_dc.RDS"))
FSD_trees_dc <- merge(FSD_trees_dc, DateCreekData::Treatments, by = "Unit")
MSD_trees_dc[,`:=`(pred_obs = "Predicted", CarbonPool = "Dead")]
FSD_trees_dc[,`:=`(pred_obs = "Observed", CarbonPool = "Dead")]

MS_cwd_dc <- readRDS(file.path(in_path,"MS_cwd_dc.RDS"))
FS_cwd_dc <- readRDS(file.path(in_path,"FS_cwd_dc.RDS"))
MS_cwd_dc[,`:=`(pred_obs = "Predicted", CarbonPool = "Down")]
FS_cwd_dc[,`:=`(pred_obs = "Observed", CarbonPool = "Down")]


all_pools <- rbind(MSL_trees_dc[,.(Unit, Treatment, Year, pred_obs, CarbonPool, MgHa)],
                   FSL_trees_dc[,.(Unit, Treatment, Year, pred_obs, CarbonPool, MgHa)],
                   MSD_trees_dc[,.(Unit, Treatment, Year, pred_obs, CarbonPool, MgHa)],
                   FSD_trees_dc[,.(Unit, Treatment, Year, pred_obs, CarbonPool, MgHa)],
                   MS_cwd_dc[,.(Unit, Treatment, Year, pred_obs, CarbonPool, MgHa)],
                   FS_cwd_dc[,.(Unit, Treatment, Year, pred_obs, CarbonPool, MgHa)])
all_pools[,`:=`(TSH = Year - 1992)]
all_pools <- all_pools[Year > 1992]
all_pools[, TSI_cat := as.numeric(as.character(cut(TSH,
                           breaks = seq(0, 100, by = 5), 
                           labels = seq(5, 100, by = 5),
                           right = TRUE,
                           include.lowest = TRUE)))]
#all_pools[,TSI_cat := TSI_cat*5]

all_means <- all_pools[,.(mn_MgHa = mean(MgHa)),
                       by=c("pred_obs","CarbonPool","TSI_cat")]
all_means[, CarbonPool := factor(CarbonPool, 
                                 levels = c("Live", "Dead", "Down"))]


ggplot() +
  # Modeled areas 
  geom_area(data = all_means[pred_obs == "Predicted"],
            aes(x = TSI_cat, y = mn_MgHa, fill = CarbonPool),
            alpha = 0.5) +
  
  # Observed areas 
  geom_area(data = all_means[pred_obs == "Observed"], 
            aes(x = TSI_cat, y = mn_MgHa, fill = CarbonPool),
            alpha = 1) +
  
  # Custom fill scales
  scale_fill_manual(
    name = "Carbon Pools",
    values = c(
      "Dead" = "#D55E00",   # Predicted: Orange
      "Down" = "#0072B2",   # Predicted: Blue
      "Live" = "#009E73"   # Predicted: Green
    ),
    labels = c("Live","Dead", "Down")
  ) +
  
  ylab(expression("Carbon Mg" ~ ha^-1)) +
  xlab("Time since harvest (years)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

all_means <- all_pools[,.(mn_MgHa = mean(MgHa)),
                       by=c("pred_obs","CarbonPool","TSI_cat","Treatment")]
all_means[, CarbonPool := factor(CarbonPool, 
                                 levels = c("Live", "Dead", "Down"))]
all_means[, Treatment := factor(Treatment, 
                                levels = c("NH", "LR", "HR", "CC"))]

dc_all <- ggplot() +
  # Modeled areas 
  geom_area(data = all_means[pred_obs == "Predicted"],
            aes(x = TSI_cat, y = mn_MgHa, fill = CarbonPool),
            alpha = 0.5) +
  
  # Observed areas 
  geom_area(data = all_means[pred_obs == "Observed"], 
            aes(x = TSI_cat, y = mn_MgHa, fill = CarbonPool),
            alpha = 1) +
  facet_grid(rows = "Treatment",labeller = labeller(
    Treatment = c("NH" = "No Harvest", 
                  "LR" = "High retention", 
                  "HR" = "Medium retention",
                  "CC" = "No retention")
  ))+
  # Custom fill scales
  scale_fill_manual(
    name = "Carbon Pools",
    values = c(
      "Dead" = "#D55E00",   # Predicted: Orange
      "Down" = "#0072B2",   # Predicted: Blue
      "Live" = "#009E73"   # Predicted: Green
    ),
    labels = c("Live","Dead", "Down")
  ) +
  ylab(NULL)+
  #ylab(expression("Carbon Mg" ~ ha^-1)) +
  xlab("Time since harvest (years)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        text = element_text(family = "Arial"),  
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"))+
  guides(
    color = guide_legend(title.position = "top", 
                         title.hjust = 0.5, override.aes = list(size = 5)),
    shape = guide_legend(override.aes = list(size = 5)),
    linetype = guide_legend(override.aes = list(size = 5)))

ggsave(filename = "ICH_all_area_tr.png",plot = last_plot(),
       path = file.path(out_path), device='png', dpi=1200)


# Combine the two groups into a 3x2 layout
combined_plot <- sl_all | dc_all

combined_plot
ggsave(filename = "All_pools_area_both.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)





# This is actually what we're talking about in the paper: ---------------------------------------
MS_cwd_sl_dc_sg <- readRDS(file.path(in_path, "MS_cwd_sl_dc_sg.RDS"))
FS_cwd_sl_dc_sg <- readRDS(file.path(in_path,"FS_cwd_sl_dc_sg.RDS"))
setnames(MS_cwd_sl_dc_sg, c("unit","treatment"),c("Unit","Treatment"))
setnames(FS_cwd_sl_dc_sg, c("Decay","treatment"),c("DecayClass","Treatment"))

MS_cwd_sl_dc_sg[,`:=`(pred_obs = "Predicted")]
FS_cwd_sl_dc_sg[,`:=`(pred_obs = "Observed")]

sl_cwd_pools <- rbind(MS_cwd_sl_dc_sg[,.(Unit, Treatment, Year, pred_obs, DecayClass, SpGrp, MgHa)],
                      FS_cwd_sl_dc_sg[,.(Unit, Treatment, Year, pred_obs, DecayClass, SpGrp, MgHa)])
sl_cwd_pools[,`:=`(TSH = Year - 1992)]
sl_cwd_pools <- sl_cwd_pools[Year > 1992]
sl_cwd_pools[, TSI_cat := as.numeric(as.character(cut(TSH,
                                                      breaks = seq(0, 100, by = 2), 
                                                      labels = seq(2, 100, by = 2),
                                                      right = TRUE,
                                                      include.lowest = TRUE)))]
sl_cwd_means <- sl_cwd_pools[SpGrp == 1,.(mn_MgHa = mean(MgHa)),
                             by=c("pred_obs","DecayClass","TSI_cat","Treatment")]#all species groups
sl_cwd_means[, DecayClass := factor(DecayClass, 
                                    levels = c("1", "2", "3","4","5"))]
sl_cwd_means[, Treatment := factor(Treatment, 
                                levels = c("light/no", "med", "heavy"))]


sl_cwd <- ggplot() +
  # Modeled areas 
  geom_area(data = sl_cwd_means[pred_obs == "Predicted"],
            aes(x = TSI_cat, y = mn_MgHa, fill = DecayClass),
            alpha = 0.8) +
  
  # Observed areas 
  #geom_area(data = sl_cwd_means[pred_obs == "Observed"], 
   #         aes(x = TSI_cat, y = mn_MgHa, fill = DecayClass),
    #        alpha = 1) +
  geom_col(data = sl_cwd_means[pred_obs == "Observed"], 
           aes(x = TSI_cat, y = mn_MgHa, fill = DecayClass), 
           position = "stack", width = 1.7)+
  #geom_jitter(data = sl_cwd_means[pred_obs == "Observed"], 
   #         aes(x = TSI_cat, y = mn_MgHa, colour = DecayClass),
    #        alpha = 1, size =2)+
  facet_grid(rows = vars(Treatment),
             labeller = labeller(
               Treatment = c("light/no" = "High retention", 
                             "med" = "Medium retention", 
                             "heavy" = "Low retention")
             ))+
  # Custom fill scales
  scale_colour_manual(
    name = "Decay Classes",
    values = c(
      "1" = "#08306B",  # Darkest blue
      "2" = "#2171B5",
      "3" = "#4292C6",
      "4" = "#6BAED6",
      "5" = "#C6DBEF"   # Lightest blue
    ),
    labels = c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")
  )+
  # Custom fill scales
  scale_fill_manual(
    name = "Decay Classes",
    values = c(
      "1" = "#08306B",  # Darkest blue
      "2" = "#2171B5",
      "3" = "#4292C6",
      "4" = "#6BAED6",
      "5" = "#C6DBEF"   # Lightest blue
    ),
    labels = c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")
  ) +
  ylab(expression("Dead down carbon Mg" ~ ha^-1))+
  ylim(c(0,25))+
  #ylab(expression("Carbon Mg" ~ ha^-1)) +
  xlab("Time since harvest (years)") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        text = element_text(family = "Arial"),  
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"))+
  guides(
    color = guide_legend(title.position = "top", 
                         title.hjust = 0.5, override.aes = list(size = 5)),
    shape = guide_legend(override.aes = list(size = 5)),
    linetype = guide_legend(override.aes = list(size = 5)))



MS_cwd_dc_dc_sg <- readRDS(file.path(in_path,"MS_cwd_dc_dc_sg.RDS"))
FS_cwd_dc_dc_sg <- readRDS(file.path(in_path,"FS_cwd_dc_dc_sg.RDS"))
MS_cwd_dc_dc_sg[,`:=`(pred_obs = "Predicted")]
FS_cwd_dc_dc_sg[,`:=`(pred_obs = "Observed")]


dc_cwd_pools <- rbind(MS_cwd_dc_dc_sg[,.(Unit, Treatment, Year, pred_obs, DecayClass, SpGrp, MgHa)],
                      FS_cwd_dc_dc_sg[,.(Unit, Treatment, Year, pred_obs, DecayClass, SpGrp, MgHa)])
dc_cwd_pools[,`:=`(TSH = Year - 1992)]
dc_cwd_pools <- dc_cwd_pools[Year > 1992]
dc_cwd_pools[, TSI_cat := as.numeric(as.character(cut(TSH,
                                                   breaks = seq(0, 100, by = 2), 
                                                   labels = seq(2, 100, by = 2),
                                                   right = TRUE,
                                                   include.lowest = TRUE)))]
#dc_cwd_pools[,TSI_cat := TSI_cat*5]

dc_cwd_means <- dc_cwd_pools[SpGrp == 1,.(mn_MgHa = mean(MgHa)),
                       by=c("pred_obs","DecayClass","TSI_cat","Treatment")]#all species groups
dc_cwd_means[, DecayClass := factor(DecayClass, 
                                 levels = c("1", "2", "3","4","5"))]
dc_cwd_means[, Treatment := factor(Treatment, 
                                levels = c("NH", "LR", "HR", "CC"))]
dc_cwd_means[, pred_obs := factor(pred_obs, 
                                   levels = c(c("Predicted","Observed")))]


dc_cwd <- ggplot() +
  # Modeled areas 
  geom_area(data = dc_cwd_means[pred_obs == "Predicted"],
            aes(x = TSI_cat, y = mn_MgHa, fill = DecayClass),
            alpha = 1) +
  
  # Observed areas 
  geom_area(data = dc_cwd_means[pred_obs == "Observed"], 
            aes(x = TSI_cat, y = mn_MgHa, fill = DecayClass),
            alpha = 1) +
  facet_grid(rows = vars(Treatment), cols = vars(pred_obs),
             labeller = labeller(
    Treatment = c("NH" = "No Harvest", 
                  "LR" = "High retention", 
                  "HR" = "Medium retention",
                  "CC" = "No retention")
  ))+
  # Custom fill scales
  scale_fill_manual(
    name = "Decay Classes",
    values = c(
      "1" = "#08306B",  # Darkest blue
      "2" = "#2171B5",
      "3" = "#4292C6",
      "4" = "#6BAED6",
      "5" = "#C6DBEF"   # Lightest blue
    ),
    labels = c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")
  ) +
  ylim(c(0,25))+
  ylab(NULL)+
  #ylab(expression("Carbon Mg" ~ ha^-1)) +
  xlab("Time since harvest (years)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        text = element_text(family = "Arial"),  
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"))+
  guides(
    color = guide_legend(title.position = "top", 
                         title.hjust = 0.5, override.aes = list(size = 5)),
    shape = guide_legend(override.aes = list(size = 5)),
    linetype = guide_legend(override.aes = list(size = 5)))

# Combine the two groups into a 3x2 layout
combined_plot <- sl_cwd | dc_cwd

combined_plot
ggsave(filename = "cwd_pools_area_both.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)









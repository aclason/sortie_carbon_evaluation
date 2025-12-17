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

ggsave(filename = "SBS_all_area_tr.png",plot = last_plot(),
       path = file.path(out_path), device='png', dpi=1200)


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








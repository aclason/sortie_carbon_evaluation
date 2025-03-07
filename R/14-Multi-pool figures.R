library(ggplot2)
library(data.table)
library(patchwork)

in_path <- "04_out_carbon"
out_path <- "05_out_analysis"

#Live
MSL_trees_sl <- readRDS(file.path(in_path,"MSL_trees_sl.RDS"))
FSL_trees_sl <- readRDS(file.path(in_path,"FSL_trees_sl.RDS"))
MFL_trees_sl <- merge(FSL_trees_sl, MSL_trees_sl, by = c("Unit","Treatment","Year","State"),
                      all.x = TRUE)
setnames(MFL_trees_sl, c("MgHa.x","MgHa.y","BaHa.x","BaHa.y"), 
         c("MgHa_obs","MgHa_pred","BaHa_obs","BaHa_pred"))

MSL_trees_dc <- readRDS(file.path(in_path,"MSL_trees_dc.RDS"))
FSL_trees_dc <- readRDS(file.path(in_path,"FSL_trees_dc.RDS"))
MFL_trees_dc <- merge(FSL_trees_dc, MSL_trees_dc, by = c("Unit","Treatment","Year","State"),
                      all.x = TRUE)
setnames(MFL_trees_dc, c("MgHa.x","MgHa.y", "BAHa.x", "BAHa.y"),
         c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred"))

#Dead
MSD_trees_sl <- readRDS(file.path(in_path,"MSD_trees_sl.RDS"))
FSD_trees_sl <- readRDS(file.path(in_path,"FSD_trees_sl.RDS"))
MFD_trees_sl <- merge(FSD_trees_sl, MSD_trees_sl, by = c("Unit","Treatment","Year","State"),
                      all.x = TRUE)
MFD_trees_sl[is.na(MFD_trees_sl)] <- 0
setnames(MFD_trees_sl, c("MgHa.x","MgHa.y"), c("MgHa_obs","MgHa_pred"))

MSD_trees_dc <- readRDS(file.path(in_path,"MSD_trees_dc.RDS"))
FSD_trees_dc <- readRDS(file.path(in_path,"FSD_trees_dc_mh.RDS")) 
FSD_trees_dc[, State := "Dead"]
MFD_trees_dc <- merge(FSD_trees_dc, MSD_trees_dc, by = c("Unit","Year","State"),
                      all.x = TRUE)
setnames(MFD_trees_dc, c("MgHa.x","MgHa.y", "BAHa.x", "BAHa.y"),
         c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred"))

#Downed
MS_cwd_sl <- readRDS(file.path(in_path,"MS_cwd_sl.RDS"))
FS_cwd_sl <- readRDS(file.path(in_path,"FS_cwd_sl.RDS"))
setnames(MS_cwd_sl, "unit","Unit")
MF_cwd_sl <- merge(FS_cwd_sl, MS_cwd_sl, by = c("Unit","treatment","Year"),
                   all.x = TRUE)
setnames(MF_cwd_sl, c("MgHa.x","MgHa.y", "VolHa.x", "VolHa.y"), 
         c("MgHa_obs","MgHa_pred","VolHa_obs","VolHa_pred"))

MS_cwd_dc <- readRDS(file.path(in_path,"MS_cwd_dc.RDS"))
FS_cwd_dc <- readRDS(file.path(in_path,"FS_cwd_dc.RDS"))
MF_cwd_dc <- merge(FS_cwd_dc, MS_cwd_dc, by = c("Unit","Treatment","Year"),
                   all.x = TRUE)
setnames(MF_cwd_dc, c("MgHa.x","MgHa.y", "VolHa.x", "VolHa.y"), 
         c("MgHa_obs","MgHa_pred","VolHa_obs","VolHa_pred"))


theme_set(theme_minimal(base_family = "Arial") +  # Change "Arial" to your desired font
            theme(
              text = element_text(family = "Arial"),  # Change "Arial" to your desired font
              plot.title = element_text(size = 10, face = "bold"),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text = element_text(size = 10, face = "bold"),
              legend.text = element_text(size = 10, face = "bold"),
              strip.text = element_text(size = 10, face = "bold"),
              legend.title = element_text(size = 10, face = "bold")
            ))


plot1 <- ggplot()+
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
  theme(legend.position = "none")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))

plot2 <- ggplot()+
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
    x = "Live carbon (Mg/ha) predicted",
    y = "Live carbon (Mg/ha) observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,275))+
  ylim(c(0,275))+
  theme(legend.position = "none")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))



plot3 <- ggplot()+
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
    x = "Dead carbon (Mg/ha) predicted",
    y = "Dead carbon (Mg/ha) observed",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,35))+
  ylim(c(0,35))+
  theme(legend.position = "none")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))

plot4 <- ggplot()+
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
    x = "Dead carbon (Mg/ha) predicted",
    y = "Dead carbon (Mg/ha) observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,70))+
  ylim(c(0,70))+
  theme(legend.position = "none")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))

plot5 <- ggplot()+
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
    x = "Downed carbon (Mg/ha) predicted",
    y = "Downed carbon (Mg/ha) observed",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,30))+
  ylim(c(0,30))+
  theme(legend.position = "none")
  
  #guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
 # guides(color = guide_legend(override.aes = list(size = 5)))

plot6 <- ggplot()+
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
    x = "Downed carbon (Mg/ha) predicted",
    y = "Downed carbon (Mg/ha) observed",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,50))+
  ylim(c(0,50))+
  theme(legend.position = "none")


group1 <- (plot1 + plot3 + plot5) + 
  plot_layout(ncol = 1)

# Group plots 2, 4, 6 with their legend
group2 <- (plot2 + plot4 + plot6) + 
  plot_layout(ncol = 1)

# Combine the two groups into a 3x2 layout
combined_plot <- group1 | group2

combined_plot
ggsave(filename = "All_pools_fit.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)

  
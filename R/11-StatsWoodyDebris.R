# A.Clason
library(equivalence)
library(ggplot2)
library(data.table)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(patchwork)
source(file.path("R","00-utils","utils.R"))

in_path <- "04_out_carbon"
out_path <- "05_out_analysis"

# SBS -------------------------------------------------------------------------------------------

MS_cwd_sl <- readRDS(file.path(in_path,"MS_cwd_sl.RDS"))
FS_cwd_sl <- readRDS(file.path(in_path,"FS_cwd_sl.RDS"))

MS_cwd_sl_dc_sg <- readRDS(file.path(in_path, "MS_cwd_sl_dc_sg.RDS"))
FS_cwd_sl_dc_sg <- readRDS(file.path(in_path,"FS_cwd_sl_dc_sg.RDS"))

setnames(MS_cwd_sl_dc_sg, "unit","Unit")
setnames(MS_cwd_sl, "unit","Unit")
setnames(FS_cwd_sl_dc_sg, "Decay","DecayClass")
MS_cwd_sl[, Unit:= as.numeric(Unit)]

setkey(FS_cwd_sl,Unit)
setkey(MS_cwd_sl,Unit)

MF_cwd_sl <- merge(FS_cwd_sl, MS_cwd_sl, by = c("Unit","treatment","Year"),
                      all.x = TRUE)
setnames(MF_cwd_sl, c("MgHa.x","MgHa.y", "VolHa.x", "VolHa.y"), 
         c("MgHa_obs","MgHa_pred","VolHa_obs","VolHa_pred"))

MS_cwd_sl_sum <- Rmisc::summarySE(data = MS_cwd_sl, 
                                  measurevar = "MgHa", 
                                  groupvars = c("treatment","Year"))
FS_cwd_sl_sum <- Rmisc::summarySE(data = FS_cwd_sl, 
                                  measurevar = "MgHa", 
                                  groupvars = c("treatment","Year"))

MS_cwd_sl_dc_sp_sum <- Rmisc::summarySE(MS_cwd_sl_dc_sg,
                                        measurevar = "MgHa", 
                                        groupvars = c("treatment","Year", "DecayClass","SpGrp"))
MS_cwd_sl_dc_sp_sum <- data.table(MS_cwd_sl_dc_sp_sum)
FS_cwd_sl_dc_sp_sum <- Rmisc::summarySE(data = FS_cwd_sl_dc_sg, 
                                        measurevar = "MgHa", 
                                        groupvars = c("treatment","Year", "DecayClass","SpGrp"))
#2. Figures
#Carbon predicted vs observed 
ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(Unit), 
                 color = treatment), 
             alpha = 0.9,
             size = 4,
             data = MF_cwd_sl)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  coord_cartesian() +
  labs(
    x = "Downed carbon (Mg·ha⁻¹) predicted",
    y = "Downed carbon (Mg·ha⁻¹) observed",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,30))+
  ylim(c(0,30))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(filename = "SBS_cwd_fit.png",width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)


#2. Statistics ----------------------------------------------------------
years <- c(2020, "All Years")
results <- lapply(years, function(year) {
  data <- select_years(year,meas_obs = MgHa_obs, meas_pred = MgHa_pred,
                       data = MF_cwd_sl)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Year = years, results_df)
results_df
results <- as.data.table(lapply(results_df, function(col) {
  if (all(sapply(col, length) == 1)) unlist(col) else col
}))
results[, f_test := NULL]

t.test(MF_cwd_sl[Year == 2020]$MgHa_obs,
       MF_cwd_sl[Year == 2020]$MgHa_pred)

tost(x = MF_cwd_sl[Year == 2020]$MgHa_obs, 
     y = MF_cwd_sl[Year == 2020]$MgHa_pred,
     epsilon = 0.7,
     paired = FALSE,
     conf.level = 0.95) # 10% of the mean observed

MFD_trees_summary <- MF_cwd_sl %>%
  group_by(Year) %>%
  summarise(
    MgHa_obs_mean = mean(MgHa_obs, na.rm = TRUE),
    MgHa_obs_sd   = sd(MgHa_obs, na.rm = TRUE),
    MgHa_pred_mean = mean(MgHa_pred, na.rm = TRUE),
    MgHa_pred_sd   = sd(MgHa_pred, na.rm = TRUE)
  )

# Define the equivalence bounds as percentages of the mean observed value
equivalence_bounds <- c(0.05, 0.10, 0.15, 0.17, 0.2, 0.25, 0.3,
                        0.35, 0.4, 0.45, 0.5, 0.55,0.6, 0.65)

# Subset data for the specific year
yr_subset <- MF_cwd_sl[Year == 2020, ]

# Calculate the mean of observed values for the species
mean_obs <- mean(yr_subset$MgHa_obs)

# Loop through each equivalence bound and calculate the TOST p-value
results_table <- data.table(
  Bound_Percentage = equivalence_bounds,
  Bound_Value = equivalence_bounds * mean_obs,
  TOST_p_value = sapply(equivalence_bounds, function(bound) {
    tost_result <- equi_result(
      yr_subset$MgHa_obs,
      yr_subset$MgHa_pred,
      mean_obs * bound
    )
    round(tost_result$tost.p.value, 2)
  })
)

results_table

MFD_down_summary <- merge(results[Year != "All Years",.(Year = as.numeric(Year), Bias, RMSE, R_squared)], 
                           MFD_trees_summary, by = c("Year"))
MFD_down_summary[, per_bias := round((Bias/MgHa_obs_mean)*100,0)]
MFD_down_summary[, Ecosystem := "SBS"]
final_table <- MFD_down_summary[, .(
  Ecosystem,
  Year,
  `Bias – MgHa (% of mean)` = sprintf("%.2f (%.0f %%)", round(Bias, 2), per_bias),
  RMSE = round(RMSE, 2),
  R2 = round(R_squared, 2),
  `Mean ± (SD) predicted MgHa` = paste0(round(MgHa_pred_mean, 1),
                                        " ± (", round(MgHa_pred_sd, 1), ")"),
  `Mean ± (SD) observed MgHa` = paste0(round(MgHa_obs_mean, 1),
                                       " ± (", round(MgHa_obs_sd, 1), ")")
)]

# ---------------
final_table
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


ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(Unit), 
                 color = Treatment), 
             alpha = 0.9,
             size = 4,
             data = MF_cwd_dc)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  labs(
    x = "Downed carbon (Mg·ha⁻¹) predicted",
    y = "Downed carbon (Mg·ha⁻¹) observed",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,50))+
  ylim(c(0,50))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5),
         width = 7.91, height = 5.61)+
  guides(color = guide_legend(override.aes = list(size = 5)))

 ggsave(filename = "ICH_cwd_fit.png",width = 7.91, height = 5.61,
        path = file.path(out_path), device='png', dpi=1200)

years <- c(1992, 1993, 2011, 2019, "All Years")

results <- lapply(years, function(year) {
  data <- select_years(year,meas_obs = MgHa_obs, meas_pred = MgHa_pred,
                       data = MF_cwd_dc)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Year = years, results_df)
results_df
results <- as.data.table(lapply(results_df, function(col) {
  if (all(sapply(col, length) == 1)) unlist(col) else col
}))
results[, f_test := NULL]

t.test(MF_cwd_dc[Year == 2019]$MgHa_obs,
       MF_cwd_dc[Year == 2019]$MgHa_pred)

tost(x = MF_cwd_dc[Year == 2019]$MgHa_obs, 
     y = MF_cwd_dc[Year == 2019]$MgHa_pred,
     epsilon = 0.18,
     paired = FALSE,
     conf.level = 0.95) # 10% of the mean observed

MFD_cwd_summary <- MF_cwd_dc %>%
  group_by(Year) %>%
  summarise(
    MgHa_obs_mean = mean(MgHa_obs, na.rm = TRUE),
    MgHa_obs_sd   = sd(MgHa_obs, na.rm = TRUE),
    MgHa_pred_mean = mean(MgHa_pred, na.rm = TRUE),
    MgHa_pred_sd   = sd(MgHa_pred, na.rm = TRUE)
  )

# Define the equivalence bounds as percentages of the mean observed value
equivalence_bounds <- c(0.05, 0.10, 0.15, 0.17, 0.2, 0.25, 0.3,
                        0.35, 0.4, 0.45, 0.5, 0.55,0.6, 0.65)

# Subset data for the specific year
yr_subset <- MF_cwd_dc[Year == 2019, ]

# Calculate the mean of observed values for the species
mean_obs <- mean(yr_subset$MgHa_obs)

# Loop through each equivalence bound and calculate the TOST p-value
results_table <- data.table(
  Bound_Percentage = equivalence_bounds,
  Bound_Value = equivalence_bounds * mean_obs,
  TOST_p_value = sapply(equivalence_bounds, function(bound) {
    tost_result <- equi_result(
      yr_subset$MgHa_obs,
      yr_subset$MgHa_pred,
      mean_obs * bound
    )
    round(tost_result$tost.p.value, 2)
  })
)

results_table
MFD_cwd_summary <- merge(results[Year != "All Years",.(Year = as.numeric(Year), Bias, RMSE, R_squared)], 
                         MFD_cwd_summary, by = c("Year"))
MFD_cwd_summary[, per_bias := round((Bias/MgHa_obs_mean)*100,0)]
MFD_cwd_summary[, Ecosystem := "ICH"]
final_table <- MFD_cwd_summary[, .(
  Ecosystem,
  Year,
  `Bias – MgHa (% of mean)` = sprintf("%.2f (%.0f %%)", round(Bias, 2), per_bias),
  RMSE = round(RMSE, 2),
  R2 = round(R_squared, 2),
  `Mean ± (SD) predicted MgHa` = paste0(round(MgHa_pred_mean, 1),
                                        " ± (", round(MgHa_pred_sd, 1), ")"),
  `Mean ± (SD) observed MgHa` = paste0(round(MgHa_obs_mean, 1),
                                       " ± (", round(MgHa_obs_sd, 1), ")")
)]


# Figure 9 ----------------------------------------------------------------------------------------
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
  geom_col(data = sl_cwd_means[pred_obs == "Observed"], 
           aes(x = TSI_cat, y = mn_MgHa, fill = DecayClass), 
           position = "stack", width = 1.7)+
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







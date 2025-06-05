ggplot(data = MF_trees_sl_m[val_type == "MgHa"]) +
  geom_line(
    aes(x = Year, y = val_ha, group = Unit, color = Treatment, linetype = Treatment),
    size = 0.8 ,
    position = position_dodge(width = 1)
  )+
  facet_wrap(~obs_preds)+
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
  coord_cartesian(ylim = c(0, 50)) +
  labs(
    x = "Year",
    y = "Carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment",
    linetype = "Treatment"
  ) +
  geom_line(
    data = FSL_trees_sl,
    aes(group = Unit, color = Treatment, linetype = Treatment),
    size = 0.8 ,
    position = position_dodge(width = 1)
  )+
  geom_line(
    data = MSL_trees_sl,
    aes(group = Unit, color = Treatment, linetype = Treatment),
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


ggplot()+
  geom_point(aes(x = Height, y = meas_hgt, 
                 color = Species), 
             alpha = 0.4,
             size = 2,
             data = F_trees_sl[!is.na(meas_hgt) & Class <3])+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  coord_cartesian() +
  labs(
    x = "Height predicted (m) ",
    y = "Height observed (m)",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,50))+
  ylim(c(0,50))+
  theme(legend.position = "none")+
  #guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  # guides(color = guide_legend(override.aes = list(size = 5)))+
  facet_wrap(~Species)
#ggsave(filename = "Hgt_sl_pred_obs.png",plot = last_plot(),
#      units = "in", path = file.path(out_path,"Supplementary"), device='png', dpi=1200)

#res_m <- data.table(melt(results_df, id.vars = "Year"))
#cohen d: positive value - actual is XX MgHa higher than predicted

#ggplot()+
#  geom_line(aes(x = as.factor(Year), y = as.numeric(value), 
#                group = as.factor(variable), color = variable), 
#            data = res_m[Year != "All Years" & variable != "RMSE"])+
#  geom_point(aes(x = as.factor(Year), y = as.numeric(value), 
#                group = as.factor(variable), color = variable, size =2), 
#            data = res_m[Year == "All Years" & variable != "RMSE"])


# dead carbon

ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSD_trees_dc_sum, aes(col = Treatment), size = 1.2)+
  geom_line(aes(x = Year, y = MgHa,group = as.factor(Unit), color = Treatment), 
            data = MSD_trees_dc,
            alpha = 0.3,
            size = 0.3)+
  geom_jitter(
    data = FSD_trees_dc,
    aes(x = Year, y = MgHa, shape = Treatment),
    size = 2 ,
    position = position_dodge(width = 1)
  )+ 
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
  coord_cartesian(ylim = c(0, 60)) +
  labs(
    x = "Year",
    y = "Carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSD_trees_dc_sum,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  scale_shape_manual(
    values = c(24, 23, 21, 25),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  scale_linetype_manual(
    values = c("twodash","solid", "dashed", "dotted"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  theme(legend.position = "bottom", strip.text = element_blank()) +
  facet_wrap(~Treatment)+
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5, 
                         override.aes = list(size = 5)),
    shape = guide_legend(override.aes = list(size = 5)),
    linetype = guide_legend(override.aes = list(size = 5)))

# Carbon predicted vs observed  - year
ggplot()+
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
    x = "Carbon (Mg/ha) predicted",
    y = "Carbon (Mg/ha) observed",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  xlim(c(0,70))+
  ylim(c(0,70))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  facet_wrap(~Year)
ggsave(filename = "ICH_dead_fit_yr.png",plot = last_plot(), width = 7.91, height = 5.61,
       units = "in", path = file.path(out_path), device='png', dpi=1200)


MF_trees_dc_m[,`:=`(Unit = as.factor(Unit),
                    TSH = Year - 1992)]
dat_dc <- MF_trees_dc_m[Measure == "MgHa"][TSH > 0]

model1 <- lmer(val_ha ~ Type * Treatment * TSH + (1 | Unit), 
               data = dat_dc)

model2 <- lmer(val_ha ~ Type * Treatment + TSH + (1 | Unit), 
               data = dat_dc)

model3 <- lmer(val_ha ~ Treatment * TSH + Type + (1 | Unit), 
               data = dat_dc)

model4 <- lmer(val_ha ~ Type * TSH + Treatment  + (1 | Unit), 
               data = dat_dc)

model5 <- lmer(val_ha ~ Type + TSH + Treatment  + (1 | Unit),
               data = dat_dc)

#MgHa ~ Type * treatment * Species * TSH + (1 | unit)
AIC(model1, model2, model3, model4, model5)
anova(model2)

contrast(emmeans(model2, ~ Type), method = "tukey")

contrast(emmeans(model2, ~ Type | Treatment), method = "tukey")

# this estimates the slope effects and contrasts:
emt <- emtrends(model1, tukey ~ Type, var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)

emt <- emtrends(model1, tukey ~ Treatment, var = "TSH")
multcomp::cld(emt,  alpha=.05, Letters=letters)


# Define the equivalence bounds as percentages of the mean observed value
equivalence_bounds <- c(0.05, 0.10, 0.15, 0.17, 0.2, 0.25, 0.3,
                        0.35, 0.4, 0.45, 0.5, 0.55,0.6, 0.65)

# Subset data for the specific year
yr_subset <- MFD_trees_dc[Year == 2022, ]

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

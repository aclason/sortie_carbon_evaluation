library(ggplot2)
source(file.path("R","00-utils","utils.R"))

in_path <- "04_out_carbon"
out_path <- "05_out_analysis"

F_trees_sl <- readRDS(file.path(in_path,"F_trees_sl.RDS"))
F_trees_dc <- readRDS(file.path(in_path,"F_trees_dc.RDS"))

# SBS -------------------------------------------------------------------------------------------

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
ggsave(filename = "Hgt_sl_pred_obs.png",plot = last_plot(),
       units = "in", path = file.path(out_path,"Supplementary"), device='png', dpi=1200)

#stats to go with the above:
select_sp_h <- function(sp, data) {
  obs <- data[Species == sp]$meas_hgt
  pred <- data[Species == sp]$Height
  n_value <- nrow(data[Species == sp])
  list(obs = obs, pred = pred, n_value = n_value)
}
n_vals <-  F_trees_sl[!is.na(meas_hgt) & Class <3, .N, by = "Species"]
sp <- unique(F_trees_sl[!is.na(meas_hgt) & Class <3]$Species)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data =  F_trees_sl[!is.na(meas_hgt) & Class <3])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
merge(results_df, n_vals, by.x = "Species", by.y = "Species")

# ICH -------------------------------------------------------------------------------------------
ggplot()+
  geom_point(aes(x = Height, y = cruise_hgt, 
                 color = Spp), 
             alpha = 0.4,
             size = 2,
             data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" & Tree.Class <3])+
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
  facet_wrap(~Spp)
ggsave(filename = "Hgt_dc_pred_obs.png",plot = last_plot(),
       units = "in", path = file.path(out_path,"Supplementary"), device='png', dpi=1200)

#stats to go with the above:
select_sp_h <- function(sp, data) {
  obs <- data[Spp == sp]$cruise_hgt
  pred <- data[Spp == sp]$Height
  n_value <- nrow(data[Spp == sp])
  list(obs = obs, pred = pred, n_value = n_value)
}
n_vals <- F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" & Tree.Class <3, .N, by = "Spp"]
sp <- unique(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" & Tree.Class <3]$Spp)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" & Tree.Class <3])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
merge(results_df, n_vals, by.x = "Species", by.y = "Spp")

#most species, the bias is observed is greater than (taller than) the predicted

#what about growth?

  
  
  
  
  
  

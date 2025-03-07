library(ggplot2)
library(data.table)
source(file.path("R","00-utils","utils.R"))

in_path <- "04_out_carbon"
out_path <- "05_out_analysis"

F_trees_sl <- readRDS(file.path(in_path,"F_trees_sl.RDS")) #need to re-create these
F_trees_sl <- merge(F_trees_sl, SummitLakeData::Treatments, by = "unit")
setnames(F_trees_sl, c("unit","treatment"), c("Unit","Treatment"))
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
#ggsave(filename = "Hgt_sl_pred_obs.png",plot = last_plot(),
 #      units = "in", path = file.path(out_path,"Supplementary"), device='png', dpi=1200)



#height by species and treatment ---------------------------------
F_trees_sl[, Treatment := factor(Treatment, 
                                 levels = c("light/no", "med", "heavy"))]

ggplot()+
  geom_point(aes(x = Height, y = meas_hgt, 
                 color = Treatment), 
             alpha = 0.8,
             size = 2,
             data = F_trees_sl[!is.na(meas_hgt) & Class <3][Species == "Bl"|Species == "Sx"])+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  coord_cartesian() +
  labs(
    x = "Height predicted (m) ",
    y = "Height observed (m)",
    col = NULL,
    fill = "Treatment",
    shape = "Treatment"
  ) +
  scale_color_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  )+
  xlim(c(0,40))+
  ylim(c(0,40))+
  theme(legend.position = "none")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  facet_grid(c("Treatment","Species"),
             labeller = labeller(Species = c("Bl" = "Subalpine fir", "Sx" = "Spruce"),
                                 Treatment = c("light/no" = "High retention",
                                               "med" = "Medium retention",
                                               "heavy" = "Low retention")))
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

rsquared(F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Bl" & Treatment == "light/no"]$meas_hgt,
         F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Bl"& Treatment == "light/no"]$Height)
rsquared(F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Bl" & Treatment == "med"]$meas_hgt,
         F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Bl"& Treatment == "med"]$Height)
rsquared(F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Bl" & Treatment == "heavy"]$meas_hgt,
         F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Bl"& Treatment == "heavy"]$Height)

rsquared(F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Sx" & Treatment == "light/no"]$meas_hgt,
         F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Sx"& Treatment == "light/no"]$Height)
rsquared(F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Sx" & Treatment == "med"]$meas_hgt,
         F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Sx"& Treatment == "med"]$Height)
rsquared(F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Sx" & Treatment == "heavy"]$meas_hgt,
         F_trees_sl[!is.na(meas_hgt) & Class <3 & Species == "Sx"& Treatment == "heavy"]$Height)


# ICH -------------------------------------------------------------------------------------------

ggplot(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" & Tree.Class <3 & Spp == "Pl"])+
  geom_point(aes(x = Height, y = cruise_hgt))

F_trees_dc[, Treatment := factor(Treatment, 
                                 levels = c("NH", "LR", "HR","CC"))]
#with the plantation and residual allom applied:
ggplot()+
  geom_point(aes(x = Height, y = cruise_hgt, 
                 color = Treatment), 
             alpha = 0.4,
             size = 2,
             data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" & Tree.Class <3][Spp == "Hw"|
                                                                                     Spp == "Cw"|
                                                                                     Spp == "Pl"|
                                                                                     Spp == "Ba"|
                                                                                     Spp == "Sx"])+
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
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  #guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
 # guides(color = guide_legend(override.aes = list(size = 5)))+
  facet_grid(c("Treatment","Spp"),
             labeller = labeller(Spp = c("Hw" = "Hemlock",
                                             "Sx" = "Spruce",
                                             "Pl" = "Pine",
                                             "Cw" = "Cedar",
                                             "Ba" = "Fir"),
                                 Treatment = c("NH" = "No harvest",
                                                "LR" = "High retention",
                                               "HR" = "Medium retention",
                                               "CC" = "No retention")))
ggsave(filename = "Hgt_dc_pred_obs_adj.png",plot = last_plot(),
       units = "in", path = file.path(out_path,"Supplementary"), device='png', dpi=1200)

#stats to go with the above:
select_sp_h <- function(sp, data) {
  obs <- data[Spp == sp]$cruise_hgt
  pred <- data[Spp == sp]$Height
  n_value <- nrow(data[Spp == sp])
  list(obs = obs, pred = pred, n_value = n_value)
}

#CC
n_vals <- F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                     & Tree.Class <3 & Treatment == "CC", .N, by = "Spp"]
sp <- unique(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                        & Tree.Class <3 & Treatment == "CC"]$Spp)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                                            & Tree.Class <3 & Treatment == "CC"])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df

#HR
n_vals <- F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                     & Tree.Class <3 & Treatment == "HR", .N, by = "Spp"]
sp <- unique(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                        & Tree.Class <3 & Treatment == "HR"]$Spp)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                                            & Tree.Class <3 & Treatment == "HR"])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df

#LR
n_vals <- F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                     & Tree.Class <3 & Treatment == "LR", .N, by = "Spp"]
sp <- unique(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                        & Tree.Class <3 & Treatment == "LR"]$Spp)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                                            & Tree.Class <3 & Treatment == "LR"])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df

#NH
n_vals <- F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                     & Tree.Class <3 & Treatment == "NH", .N, by = "Spp"]
sp <- unique(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                        & Tree.Class <3 & Treatment == "NH"]$Spp)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                                            & Tree.Class <3 & Treatment == "NH"])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df



#unadjusted:
tree_dt_92 <- DateCreekData::trees_1992(cruise_data = "D:/Github/DateCreekData/data-raw/Trees/1992data.csv",
                                        fixed_data = "D:/Github/DateCreekData/data-raw/Trees/1992fixed_radius_data_fromTable20_DateCkHandbook.csv",
                                        calc_height = TRUE)
tree_dt_92 <- tree_dt_92[SPH != 0]
tree_dt_93 <- DateCreekData::trees_1993(data =  "D:/Github/DateCreekData/data-raw/Trees/SS93forR.csv",
                                        sm_tree = "D:/Github/DateCreekData/data-raw/Trees/1993_under1.3m_tree_tallies.csv",
                                        calc_height = TRUE,
                                        tree_cl_stub = "8") #tree class 8 considered stubin 1993

tree_dt_10_no <- DateCreekData::trees_2010(lrg_trees = "D:/Github/DateCreekData/data-raw/Trees/Data Creek 2010 Data large trees.csv",
                                        cc_trees = "D:/Github/DateCreekData/data-raw/Trees/Trees 10cm and above in clearcut 2010.csv",
                                        small_trees = "D:/Github/DateCreekData/data-raw/Trees/Date Creek 2010 Trees less than 10 cm tallies.csv",
                                        snag_heights = "D:/Github/DateCreekData/data-raw/Trees/SnagHeights2010.csv",
                                        measured_heights2022 = "D:/Github/DateCreekData/data-raw/Trees/StandStructureData_Nov2022_Final.xlsx",
                                        calc_height = TRUE,
                                        use_standard = TRUE, #TRUE = standard, FALSE = residual 
                                        id_gap_trees = FALSE, #use gaps to trigger plantation allom
                                        use_size = FALSE, #use tree size to trigger plantation allom
                                        use_cc = FALSE) #use cc to trigger plantation allom


tree_dt_18 <- DateCreekData::trees_201x(data_file = "D:/Github/DateCreekData/data-raw/Trees/Date Creek 2018 Data large trees_re-entered.xlsx",
                                        data_2018 = "DataCk re-entry 2018 largeTrees",
                                        data_2019 = "D:/Github/DateCreekData/data-raw/Trees/Data Creek 2019 Data large trees.csv",
                                        inter_trees = "D:/Github/DateCreekData/data-raw/Trees/2018-19intermediatetrees.csv",
                                        small_trees = "D:/Github/DateCreekData/data-raw/Trees/Small trees 2018 2019 KHP.csv",
                                        lrg_trees_2010 = "D:/Github/DateCreekData/data-raw/Trees/Data Creek 2010 Data large trees.csv",
                                        measured_heights2022 = "D:/Github/DateCreekData/data-raw/Trees/StandStructureData_Nov2022_Final.xlsx",
                                        snag_hgts_18 = "D:/Github/DateCreekData/data-raw/Trees/SnagHeights2018.csv",
                                        calc_height = TRUE,
                                        use_standard = TRUE, #TRUE = standard, FALSE = residual
                                        id_gap_trees = FALSE, #use gaps to trigger plantation allom
                                        use_size = FALSE, #use tree size to trigger plantation allom
                                        use_cc = FALSE)#use cc to trigger plantation allom

tree_dt_22 <- DateCreekData::trees_2022(data_file = "D:/Github/DateCreekData/data-raw/Trees/StandStructureData_Nov2022_Final.xlsx",
                                        large_trees = "Large",
                                        inter_trees = "Inter",
                                        small_trees = "Small",
                                        calc_height = TRUE)
F_trees_dc_no <- rbind(tree_dt_92, tree_dt_93, tree_dt_10, tree_dt_18, tree_dt_22, fill = TRUE)
F_trees_dc_no <- merge(F_trees_dc_no, DateCreekData::Treatments)

F_trees_dc_no[, Treatment := factor(Treatment, 
                                 levels = c("NH", "LR", "HR","CC"))]
ggplot(tree_dt_10_no[!is.na(cruise_hgt) & StubYN == "N" & Tree.Class <3 & Spp == "Pl"])+
  geom_point(aes(x = Height, y = cruise_hgt))

#with the plantation and residual allom applied:
ggplot()+
  geom_point(aes(x = Height, y = cruise_hgt, 
                 color = Treatment), 
             alpha = 0.4,
             size = 2,
             data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" & Tree.Class <3][Spp == "Hw"|
                                                                                     Spp == "Cw"|
                                                                                     Spp == "Pl"|
                                                                                     Spp == "Ba"|
                                                                                     Spp == "Sx"])+
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
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  #guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  # guides(color = guide_legend(override.aes = list(size = 5)))+
  facet_grid(c("Treatment","Spp"),
             labeller = labeller(Spp = c("Hw" = "Hemlock",
                                         "Sx" = "Spruce",
                                         "Pl" = "Pine",
                                         "Cw" = "Cedar",
                                         "Ba" = "Fir"),
                                 Treatment = c("NH" = "No harvest",
                                               "LR" = "High retention",
                                               "HR" = "Medium retention",
                                               "CC" = "No retention")))
ggsave(filename = "Hgt_dc_pred_obs_no_adj.png",plot = last_plot(),
       units = "in", path = file.path(out_path,"Supplementary"), device='png', dpi=1200)

#stats to go with the above:
select_sp_h <- function(sp, data) {
  obs <- data[Spp == sp]$cruise_hgt
  pred <- data[Spp == sp]$Height
  n_value <- nrow(data[Spp == sp])
  list(obs = obs, pred = pred, n_value = n_value)
}

#CC
n_vals <- F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                     & Tree.Class <3 & Treatment == "CC", .N, by = "Spp"]
sp <- unique(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                        & Tree.Class <3 & Treatment == "CC"]$Spp)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                                            & Tree.Class <3 & Treatment == "CC"])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df

#HR
n_vals <- F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                     & Tree.Class <3 & Treatment == "HR", .N, by = "Spp"]
sp <- unique(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                        & Tree.Class <3 & Treatment == "HR"]$Spp)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                                            & Tree.Class <3 & Treatment == "HR"])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df

#LR
n_vals <- F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                     & Tree.Class <3 & Treatment == "LR", .N, by = "Spp"]
sp <- unique(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                        & Tree.Class <3 & Treatment == "LR"]$Spp)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                                            & Tree.Class <3 & Treatment == "LR"])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df

#NH
n_vals <- F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                     & Tree.Class <3 & Treatment == "NH", .N, by = "Spp"]
sp <- unique(F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                        & Tree.Class <3 & Treatment == "NH"]$Spp)
results <- lapply(sp, function(sp) {
  data <- select_sp_h(sp, data = F_trees_dc[!is.na(cruise_hgt) & StubYN == "N" 
                                            & Tree.Class <3 & Treatment == "NH"])
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df

comp_allom <-  F_trees_dc[, no_adj := F_trees_dc_no$Height]

#how different are they?
ggplot(comp_allom)+
  geom_point(aes(x = no_adj, y = Height, color = Treatment))


comp_allom[, diff := Height - no_adj, by = seq_len(nrow(comp_allom))]

ggplot(comp_allom)+
  geom_boxplot(aes(x = Treatment, y = diff))

#shorter in the clearcut
#no diff in HR
#taller in LR & NH
anova(lm(diff ~ Treatment, data = comp_allom))

hist(na.omit(comp_allom$diff))
length(na.omit(comp_allom$diff))
nrow(comp_allom[diff !=0])
(2840/20780)*100 #13% of trees had a different height with the adjustments.





#most species, the bias is observed is greater than (taller than) the predicted
#what about growth?

  
  
  
  
  
  

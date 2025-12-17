library(data.table)
library(SummitLakeData)
library(DateCreekData)
library(treeCalcs) 

in_path <- file.path("03_out_sortie")
out_path <- file.path("04_out_carbon")

# SBS data ----
raw_tree_sl <- "D:/Github/SummitLakeData/data-raw/SummitLakeData.csv"

# ICH data ----
dc_dat_path <- file.path("D:","Github","DateCreekData","data-raw","Trees")
dat_cruise_1992 <- file.path(dc_dat_path,"1992data.csv")
dat_fixed_1992 <- file.path(dc_dat_path,"1992fixed_radius_data_fromTable20_DateCkHandbook.csv")
dat_1993 <- file.path(dc_dat_path,"SS93forR.csv")
dat_sm_1993 <- file.path(dc_dat_path,"1993_under1.3m_tree_tallies.csv")
dat_lrg_2010 <- file.path(dc_dat_path,"Data Creek 2010 Data large trees.csv")
dat_cc_2010 <- file.path(dc_dat_path,"Trees 10cm and above in clearcut 2010.csv")
dat_sm_2010 <- file.path(dc_dat_path,"Date Creek 2010 Trees less than 10 cm tallies.csv")
dat_snag_hgt_2010 <- file.path(dc_dat_path,"SnagHeights2010.csv")
dat_2018 <- file.path(dc_dat_path,"Date Creek 2018 Data large trees_re-entered.xlsx")
dat_lg_2018 <- "DataCk re-entry 2018 largeTrees"
dat_int_2018 <- file.path(dc_dat_path,"2018-19intermediatetrees.csv")
dat_sm_2018 <- file.path(dc_dat_path,"Small trees 2018 2019 KHP.csv")
dat_lg_2019 <- file.path(dc_dat_path,"Data Creek 2019 Data large trees.csv")
dat_snag_hgts_18 <- file.path(dc_dat_path,"SnagHeights2018.csv")
dat_2022 <- file.path(dc_dat_path,"StandStructureData_Nov2022_Final.xlsx")

#SBS --------------------------------------------------------------------
#1. Clean tree data:

# using diam-height relationship - same as Date Creek
F_trees_sl <- SummitLakeData::clean_trees(raw_data = raw_tree_sl)
#saveRDS(F_trees_sl, file.path(out_path,"F_trees_sl.RDS"))

#2. Calculate carbon and basal area:
# Calculate carbon per tree 
F_trees_sl[, Kg_treeC := treeCalcs::calc_tree_c(Species = Species,
                                                DBH = DBH,
                                                HT = ifelse(is.na(meas_hgt),
                                                            Height,meas_hgt),
                                                Tree_class = Class),
           by= seq_len(nrow(F_trees_sl))][, Mg_treeC := Kg_treeC/1000]

# Calculate basal area per tree
F_trees_sl[, BA := pi*(DBH/200)^2 ,
           by= seq_len(nrow(F_trees_sl))
]
#get the treatments & remove the unit/year combos that were not sampled:
treats <- SummitLakeData::Treatments[, .(Year = rep(sort(unique(F_trees_sl$Year)), 
                                                    length.out = .N*length(unique(F_trees_sl$Year)))),
                                     by = .(unit, treatment)]
remeas_freq_m <- melt(remeas_freq, 
                      id.vars = "PSP", 
                      variable.factor = FALSE)
setkey(remeas_freq_m, PSP)
setnames(remeas_freq_m, 
         c("PSP","variable"),
         c("unit","Year"))
remeas_freq_m[,Year := as.numeric(Year)]
treats_surveyed <- merge(treats, 
                         remeas_freq_m[!is.na(value)], 
                         by = c("unit","Year"))
treats_surveyed[,value:=NULL]

#3. Summarize by unit and unit/species
#each plot was 0.05ha in size (12.6m radius)
# Live -----
FSL_trees_sl <- F_trees_sl[State == "Live" & DeadStatus == "NotDown" & DBH >= 7.5,
                           .(MgHa = sum(Mg_treeC)*20,
                             BaHa = sum(na.omit(BA))*20), 
                           by = .(unit, Year, State)]
FSL_trees_sl <- merge(treats_surveyed, FSL_trees_sl, 
                      by = c("unit","Year"), all = TRUE)
FSL_trees_sl[is.na(MgHa), `:=`(MgHa = 0,
                               BaHa = 0,
                               State = "Live")]
setnames(FSL_trees_sl, c("unit", "treatment"), c("Unit","Treatment"))

# Dead -----
FSD_trees_sl <- F_trees_sl[State == "Dead" & DeadStatus == "Dead" & DBH >= 7.5,
                           .(MgHa = sum(Mg_treeC)*20,
                             BaHa = sum(na.omit(BA))*20), 
                           by = .(unit, Year, State)]
FSD_trees_sl <- merge(treats_surveyed, FSD_trees_sl,
                      by = c("unit","Year"), all = TRUE)
FSD_trees_sl[is.na(MgHa), `:=`(MgHa = 0, BaHa = 0, State = "Dead")]
setnames(FSD_trees_sl, c("unit", "treatment"), c("Unit","Treatment"))


#Live by species ------------------
unit_sp <- treats_surveyed[,.(Species = c("Sx","Bl")), 
                           by = .(unit, Year, treatment)]

FSL_trees_sl_sp <- F_trees_sl[State == "Live" & DeadStatus == "NotDown" & DBH >= 7.5,
                              .(MgHa = sum(Mg_treeC)*20,
                                BaHa = sum(na.omit(BA))*20), 
                              by = .(unit, Year, State, Species)]
FSL_trees_sl_sp <- merge(unit_sp, FSL_trees_sl_sp, 
                         by = c("unit","Year", "Species"), all.x = TRUE)
FSL_trees_sl_sp[is.na(MgHa), `:=`(MgHa = 0, State = "Live")]
FSL_trees_sl_sp[is.na(BaHa), `:=`(BaHa = 0)]
setnames(FSL_trees_sl_sp, c("unit", "treatment"), c("Unit","Treatment"))

#4. Save outputs
saveRDS(FSL_trees_sl, file.path(out_path,"FSL_trees_sl.RDS"))
saveRDS(FSD_trees_sl, file.path(out_path,"FSD_trees_sl.RDS"))
saveRDS(FSL_trees_sl_sp, file.path(out_path,"FSL_trees_sl_sp.RDS"))

#ICH ----------------------------------------------------------------------------------------------
tree_dt_92 <- DateCreekData::trees_1992(cruise_data = dat_cruise_1992,
                                        fixed_data = dat_fixed_1992,
                                        calc_height = TRUE)
tree_dt_92 <- tree_dt_92[SPH != 0]
tree_dt_93 <- DateCreekData::trees_1993(data =  dat_1993,
                                        sm_tree = dat_sm_1993,
                                        calc_height = TRUE,
                                        tree_cl_stub = "8") #tree class 8 considered stubin 1993
tree_dt_10 <- DateCreekData::trees_2010(lrg_trees = dat_lrg_2010,
                                        cc_trees = dat_cc_2010,
                                        small_trees = dat_sm_2010,
                                        snag_heights = dat_snag_hgt_2010,
                                        measured_heights2022 = dat_2022,
                                        calc_height = TRUE,
                                        use_standard = FALSE, #TRUE = standard, FALSE = residual 
                                        id_gap_trees = TRUE, #use gaps to trigger plantation allom
                                        use_size = TRUE, #use tree size to trigger plantation allom
                                        use_cc = TRUE, #use cc to trigger plantation allom
                                        planted_hgt = 20,
                                        planted_diam = 20,
                                        sp_plant = "Sx")


tree_dt_18 <- DateCreekData::trees_201x(data_file = dat_2018,
                                        data_2018 = dat_lg_2018,
                                        data_2019 = dat_lg_2019,
                                        inter_trees = dat_int_2018,
                                        small_trees = dat_sm_2018,
                                        lrg_trees_2010 = dat_lrg_2010,
                                        measured_heights2022 = dat_2022,
                                        snag_hgts_18 = dat_snag_hgts_18,
                                        calc_height = TRUE,
                                        use_standard = FALSE, #TRUE = standard, FALSE = residual
                                        id_gap_trees = TRUE, #use gaps to trigger plantation allom
                                        use_size = TRUE, #use tree size to trigger plantation allom
                                        use_cc = TRUE,#use cc to trigger plantation allom
                                        planted_hgt = 20,
                                        planted_diam = 20,
                                        sp_plant = "Sx")

tree_dt_22 <- DateCreekData::trees_2022(data_file = dat_2022,
                                        large_trees = "Large",
                                        inter_trees = "Inter",
                                        small_trees = "Small",
                                        calc_height = TRUE)

#make labels
labs_92 <- data.table(DateCreekData::get_all_plots_1992(cruise_data = dat_cruise_1992))
labs_93 <- data.table(DateCreekData::get_all_plots_1993(data = dat_1993))
labs_10 <- data.table(DateCreekData::get_all_plots_2010(lrg_trees = dat_lrg_2010,
                                                        cc_trees = dat_cc_2010,
                                                        small_trees = dat_sm_2010,
                                                        all_plots = TRUE))
labs_1x <- data.table(DateCreekData::get_all_plots_201x(data_file = dat_2018,
                                                        data_2018 = dat_lg_2018,
                                                        data_2019 = dat_lg_2019))
labs_22 <- data.table(DateCreekData::get_all_plots_22(data_file = dat_2022,
                                                      large_trees = "Large"))

all_labels <- rbind(labs_92, labs_93, labs_10, labs_1x, labs_22)
F_trees_dc <- rbind(tree_dt_92, tree_dt_93, tree_dt_10, tree_dt_18, tree_dt_22, fill = TRUE)
F_trees_dc <- merge(F_trees_dc, DateCreekData::Treatments)
#saveRDS(F_trees_dc, file.path(out_path, "F_trees_dc.RDS"))

# decisions on heights to use -------------------------------------------------------------------
# 2 trees in 92 that are dead stubs with height 0.

# 1992: Calculate carbon per tree for trees with a dbh using field height except for
# trees where height was missed

# 1993: Calculate carbon per tree for trees with a dbh using field height except for 
# trees where height was missed

# 2010: doesn't look like any measured heights
# use snag heights where they exist (heights from 2022)
# but keep modelled heights for stub for stub volume estimation lower down

# 2018: doesn't look like any measured heights
# use snag heights where they exist (heights from 2022) but keep modelled
# heights for stub for stub volume estimation lower down

# 2022: Calculate carbon for large trees with the measured height where we have it
#and estimated height where we don't

# in 1992, 1993, and 2022 there were measured field heights, so always use those first
# if the tree is dead, use snag height (called cruised_height in 2010 and 2018),
# and if it's a stub, use the taper equation 
# which (needs both the cruised and modelled heights)

F_trees_dc[, hgt_final := 
             ifelse(!is.na(DBH) & is.na(cruise_hgt), Height, #if DBH and no cruise hgt, use modelled,
                    ifelse(!is.na(DBH) & cruise_hgt == 0, Height, #if DBH and no cruise hgt, use modelled
                           ifelse(!is.na(DBH) & !is.na(cruise_hgt), cruise_hgt, #if DBH and cruise hgt, use that
                                  NA)))]

#only use predicted height - same as Summit Lake (used this in testing):
#F_trees_dc[, hgt_final := ifelse(!is.na(DBH) , Height, NA)]

# tree carbon --------------------------------------------------------------------------------------
F_trees_dc[, C_treeKg := treeCalcs::calc_tree_c(Species = Spp,
                                                DBH = DBH,
                                                HT = hgt_final,
                                                Tree_class = Tree.Class),
           by = seq_len(nrow(F_trees_dc))]
F_trees_dc[, C_treeMg := C_treeKg/1000]
F_trees_dc[is.na(C_treeMg)] #should just be regen

### stubs ###
#volume of a "conic-paraboloid" (Fraver et al. 2007) for stubs with carbon conversions from
#CWD decay class 2 wood density estimates. output is in Mg
F_trees_dc[, stub_vol := treeCalcs::StubVol_ConicPara(Species = Spp,
                                                      DBH = DBH,
                                                      StubHT = cruise_hgt, 
                                                      Height = Height),
           by = seq_len(nrow(F_trees_dc))]
F_trees_dc[, C_stubMg := treeCalcs::calc_cwd_c(volume_ha = stub_vol,
                                               Decay_class = 2,
                                               Species = Spp,
                                               BECzone = "ICH"),
           by = seq_len(nrow(F_trees_dc))]
#update the stub carbon
F_trees_dc[StubYN == "Y" & Tree.Class > 2, C_treeMg := C_stubMg]
#there's 2 stubs in 1992 that have 0 carbon because they had a cruise height of 0
##############

### regen ####
# Give regen <1.3 m tall a carbon value (only live trees)
#just using D10 (Diam_est) of 1 now which is the same as we used in the FireRehab paper
F_trees_dc[, C_regenKg := treeCalcs::calc_sm_tree_c_Ung(Species = Spp,
                                                        Diam_est = 1,
                                                        Height_class = "31-130",
                                                        Health = "L"),
           by = seq_len(nrow(F_trees_dc))]
F_trees_dc[, C_regenMg := C_regenKg/1000]
#fill in carbon for regen (<1.3):
F_trees_dc[is.na(DBH), C_treeMg := C_regenMg]
F_trees_dc[DBH == 0, C_treeMg := C_regenMg]
##############

#### #which trees count for carbon (and basal area and quadratic growth) ######
#1. remove dead trees with DBH <7.5 to match 1992 data that only counted live trees <7.5 cm DBH
F_trees_dc <- F_trees_dc[DBH >= 7.5 | Tree.Class <= 2]

#2. remove dead regen <1.3 m tall to match other years
F_trees_dc <- F_trees_dc[DBH > 0 | Tree.Class == 1] #other year regen maybe not working
#clean_2010 <- subset(clean_2010, clean_2010$DBH>0 | clean_2010$Tree.Class == 1)

#3. turning 0 DBH to NA
F_trees_dc[DBH == 0, DBH := NA]

FS_trees_dc <- F_trees_dc[,.(Treatment, Unit, Year, PlotNum, Spp, Tree.Class, DBH, 
                             BA, PHF, SPH, C_treeMg,  MgHa = C_treeMg * SPH)]
#F_trees_dc[, MgHa := Mg_treeC * SPH]

# Plot summaries -----------------------------------------------------------------------------------

# live trees:
FL_trees_dc <- FS_trees_dc[Tree.Class <= 2]

#ggplot(MSL_trees_dc[Year == 1992 & !is.na(DBH)])+
#  geom_histogram(aes(x = DBH))+
# facet_wrap(~Unit)

#ggplot(FL_trees_dc[Year == 1992 & !is.na(DBH) & DBH !=0])+
# geom_col(aes(x = DBH, y = SPH), width = 0.9, fill = "gray", color = NA)+
#  facet_wrap(~Unit)

FL_trees_dc_c <- FL_trees_dc[, .(MgHa_plot = sum(MgHa), 
                                 Ba_plot = sum(na.omit(BA*PHF)), 
                                 QMD_plot = sqrt(mean(na.omit(DBH)^2))), 
                             by = .(Treatment, Unit, Year, PlotNum)] #sum carbon by cruise plots
anti_join(FL_trees_dc_c, all_labels, by = c("Unit", "Year", "PlotNum")) #fixed plots 1992
anti_join(all_labels, FL_trees_dc_c, by = c("Unit", "Year", "PlotNum")) #plots with no trees

#remove 1992 regen
FL_trees_dc_cs <- FL_trees_dc_c[Year == 1992 & PlotNum == 1] #Seedlings 1992
FL_trees_dc_c <- FL_trees_dc_c[!(Year == 1992 & PlotNum == 1)] #take out regen before est mean

FL_trees_dc_c <- merge(FL_trees_dc_c, all_labels, 
                       by = c("Unit","Year","PlotNum"), all = TRUE)
FL_trees_dc_c[is.na(MgHa_plot), `:=` (MgHa_plot = 0,
                                      Ba_plot = 0,
                                      QMD_plot = 0)] #merge with all plot ids to capture 0s:
FL_trees_dc_c[is.na(Ba_plot), Ba_plot := 0]
#FL_trees_dc_c[is.na(QMD_plot), QMD_plot := 0]
FL_trees_dc_c  <- FL_trees_dc_c[, .(MgHa = mean(MgHa_plot), 
                                    BAHa = mean(Ba_plot),
                                    QMD = mean(na.omit(QMD_plot))), 
                                by = .(Unit, Year)]#take the average across cruise plots:
FL_trees_dc_c <- merge(FL_trees_dc_c, DateCreekData::Treatments, by = "Unit")

#add the carbon from tally fixed plots 1992 (all live)
setnames(FL_trees_dc_cs, c("MgHa_plot", "Ba_plot", "QMD_plot"),
         c("MgHa","BAHa","QMD"))
FSL_trees_dc <- rbind(FL_trees_dc_c, 
                      FL_trees_dc_cs[,.(Treatment, Unit,Year,MgHa,BAHa, QMD)])

FSL_trees_dc <- FSL_trees_dc[,.(MgHa = sum(MgHa),
                                BAHa = sum(BAHa),
                                QMD = sum(QMD)), by = .(Treatment, Unit, Year)]

#missing 0s from clearcuts in 1993:
miss_93 <- data.table(Treatment = rep("CC",4),
                      Unit = DateCreekData::Treatments[Treatment == "CC"]$Unit,
                      Year = 1993,
                      MgHa = 0,
                      BAHa = 0,
                      QMD = 0)
FSL_trees_dc <- rbind(FSL_trees_dc, miss_93)
FSL_trees_dc[, State := "Live"]

# dead trees:
FD_trees_dc <- FS_trees_dc[Tree.Class > 2]
FD_trees_dc_c <- FD_trees_dc[, .(MgHa_plot = sum(MgHa), Ba_plot = sum(BA*PHF)), 
                             by = .(Treatment, Unit, Year, PlotNum)] #sum carbon by cruise plots
FD_trees_dc_c <- merge(FD_trees_dc_c, all_labels, 
                       by = c("Unit","Year","PlotNum"), all.y = TRUE)
FD_trees_dc_c[is.na(MgHa_plot), MgHa_plot := 0] #merge with all plot ids to capture 0s:
FD_trees_dc_c[is.na(Ba_plot), Ba_plot := 0]
FD_trees_dc_c  <- FD_trees_dc_c[, .(MgHa = mean(MgHa_plot), BAHa = mean(Ba_plot)), 
                                by = .(Unit, Year)]#take the average across cruise plots:
FSD_trees_dc <- merge(FD_trees_dc_c, DateCreekData::Treatments, by = "Unit")

FSD_trees_dc[, State := "Dead"]

saveRDS(FSL_trees_dc, file.path(out_path, "FSL_trees_dc.RDS"))
saveRDS(FD_trees_dc_c, file.path(out_path, "FSD_trees_dc.RDS"))


#Live by species ------------------
FL_trees_dc <- FS_trees_dc[Tree.Class <= 2]

FL_trees_dc_sp <- FL_trees_dc[, .(MgHa_plot = sum(MgHa), 
                                  Ba_plot = sum(na.omit(BA*PHF))), 
                              by = .(Treatment, Unit, Year, PlotNum, Spp)] #sum carbon by cruise plots

anti_join(FL_trees_dc_sp, all_labels, by = c("Unit", "Year", "PlotNum"))
anti_join(all_labels, FL_trees_dc_sp, by = c("Unit", "Year", "PlotNum"))
FL_trees_dc_cs_sp <- FL_trees_dc_sp[Year == 1992 & PlotNum == 1] #take out regen
FL_trees_dc_sp <- FL_trees_dc_sp[!(Year == 1992 & PlotNum == 1)] #take out regen before est mean

unit_sp <- all_labels[,.(Spp = unique(FL_trees_dc_sp$Spp)), 
                      by = .(Unit, Year, PlotNum)]
FL_trees_dc_sp <- merge(FL_trees_dc_sp, unit_sp, 
                        by = c("Unit","Year","PlotNum","Spp"), all.y = TRUE)
FL_trees_dc_sp[is.na(MgHa_plot), MgHa_plot := 0] #merge with all plot ids to capture 0s:
FL_trees_dc_sp[is.na(Ba_plot), Ba_plot := 0]
#FL_trees_dc_c[is.na(QMD_plot), QMD_plot := 0]
FL_trees_dc_sp_m  <- FL_trees_dc_sp[, .(MgHa = mean(MgHa_plot), 
                                        BAHa = mean(Ba_plot)), 
                                    by = .(Unit, Year, Spp)]#take the average across cruise plots by sp
FL_trees_dc_sp_m <- merge(FL_trees_dc_sp_m, DateCreekData::Treatments, by = "Unit")

#add the carbon from fixed plots (all live)
setnames(FL_trees_dc_cs_sp, c("MgHa_plot", "Ba_plot"), c("MgHa","BAHa"))
FSL_trees_dc_sp <- rbind(FL_trees_dc_sp_m, 
                         FL_trees_dc_cs_sp[,.(Treatment, Unit,Year,Spp, MgHa,BAHa)])

FSL_trees_dc_sp <- FSL_trees_dc_sp[,.(MgHa = sum(MgHa),
                                      BAHa = sum(BAHa)), 
                                   by = .(Treatment, Unit, Year, Spp)]

#missing 0s from clearcuts in 1993:
miss_93_ <- data.table(Treatment = rep("CC",4),
                       Unit = DateCreekData::Treatments[Treatment == "CC"]$Unit,
                       Year = 1993,
                       MgHa = 0,
                       BAHa = 0)
miss_93_sp <- miss_93_[,.(Spp = unique(FSL_trees_dc_sp$Spp)), 
                       by = .(Treatment, Unit, Year, MgHa, BAHa)]

FSL_trees_dc_sp <- rbind(FSL_trees_dc_sp, miss_93_sp)

FSL_trees_dc_sp[, State := "Live"]
setnames(FSL_trees_dc_sp, "Spp" ,"Species")

saveRDS(FSL_trees_dc_sp, file.path(out_path, "FSL_trees_dc_sp.RDS"))


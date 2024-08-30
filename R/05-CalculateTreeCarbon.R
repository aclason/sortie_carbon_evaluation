library(data.table)
library(SummitLakeData)
library(DateCreekData)
library(treeCalcs) 

in_path <- file.path("03_out_sortie")
out_path <- file.path("04_out_carbon")


#SBS--------------------------------------------------------------------------------------------
#to do - rename MgUnit to MgHa

### SORTIE ----------------------------------------------------
My_newvalsPath <- file.path("02_init_sortie","02_summit_lake","ParameterValues")
sum_in_path <- file.path(in_path, "02_summit_lake","extracted")

run_name <- "ds-nci_si-"
outfiles <- grep(run_name, list.files(sum_in_path, pattern = ".csv", 
                                      full.names = TRUE), value = TRUE)
plots <- stringr::str_split(list.files(My_newvalsPath, pattern = "summit"),".csv",
                            simplify = TRUE)[,1]

tree_files <- grep("trees",outfiles, value = TRUE)
tree_dt <- rbindlist(lapply(tree_files, fread))

#clip to centre 1 ha:
tree_dt <- tree_dt[X >50 & X <150 & Y >50 & Y <150]

## Basal area
#tree_dt[, ':='(BA = treeCalcs::calc_BA(DBH))]

## Carbon
#use allometry to calculate height (From treeCalcs package)
M_trees_sl <- treeCalcs::sortie_tree_carbon(sortie_outputs = tree_dt, dead = TRUE, BEC = "SBS",
                         Ht_from_diam = TRUE)
M_trees_sl[, unit := as.numeric(tstrsplit(Unit, "t_", fixed = TRUE)[[2]])][,Unit:=NULL]
M_trees_sl[, Year := ifelse(unit == 4, 1994 + timestep,
                        ifelse(unit == 15, 1994 + timestep,
                               1992 + timestep))]

################## just do this if looking at SPH #################
#leave it as individuals (for SPH calcs):
minDBH <- round(min(M_trees_sl$DBH, na.rm = TRUE),0)
maxDBH <- round(max(M_trees_sl$DBH, na.rm = TRUE),0)
# Create a vector of DBH size classes, by 2 cm increments
diam_classes <- seq(minDBH,(maxDBH + 2),
                    by = 2)
for(j in 1:length(diam_classes)){
  M_trees_sl[DBH <= diam_classes[j] & DBH > diam_classes[j] - 2,
         DBH_bin := diam_classes[j]]
}

M_trees_sl_sph <- M_trees_sl[, .(SPH = .N), by = .(unit, Year, Type, tree_species, DBH_bin)]

M_trees_sl_sph_sdl <- M_trees_sl_sph[Type == "Seedling"]
M_trees_sl_sph_sap <- M_trees_sl_sph[Type == "Sapling"]
M_trees_sl_sph <- M_trees_sl_sph[Type != "Seedling" & Type != "Sapling"]

#add all zeros:
all_poss <- CJ(unique(M_trees_sl_sph$unit), unique(M_trees_sl_sph$tree_species), 
               unique(M_trees_sl_sph$Year),
               unique(M_trees_sl_sph$Type), unique(M_trees_sl_sph$DBH_bin))
setnames(all_poss,c("V1","V2","V3","V4","V5"),
         c("unit","tree_species","Year","Type","DBH_bin"))

M_trees_sl_sph_a <- merge(M_trees_sl_sph, all_poss,
                 by = c("unit","tree_species","Year","Type","DBH_bin"),
                 all = T)
M_trees_sl_sph_a[is.na(SPH), SPH := 0]
M_trees_sl_sph_a <- M_trees_sl_sph_a[Year == 1992 |
                                       Year == 1994 |
                                       Year == 1997 |
                                       Year == 2009 |
                                       Year == 2018]

ggplot(data = M_trees_sl_sph_a[unit == 12 & Type == "Adult"])+
  geom_col(aes(x = DBH_bin, y = SPH, fill = tree_species))+
  facet_wrap(~Year)+
  theme(legend.position = "bottom")

ggplot(data = M_trees_sl_sph_a[unit == 3 & Type == "Snag"])+
  geom_col(aes(x = DBH_bin, y = SPH, fill = tree_species))+
  facet_wrap(~Year)+
  theme(legend.position = "bottom")

ggplot(data = M_trees_sl_sph_a[Type == "Adult"])+
  geom_col(aes(x = DBH_bin, y = SPH, fill = tree_species))+
  facet_grid(c("unit","Year"))+
  theme(legend.position = "bottom")
####################################################################
# by unit -------------------
# Live -----
#MSL_trees_sl <- M_trees_sl[Type == "Adult" & DBH >=7.5,
 #                          .(MgUnit = sum(Mg_treeC)), by = .(unit, Year)]
MSL_trees_l <- M_trees_sl[Type != "Snag"]
MSL_trees_sl <- MSL_trees_l[,.(MgHa = sum(na.omit(Mg_treeC))), by = .(unit, Year)]
MSL_trees_sl <- merge(SummitLakeData::Treatments, MSL_trees_sl, by = "unit", all = TRUE)
MSL_trees_sl[, State := "Live"]
setnames(MSL_trees_sl, c("unit", "treatment"), c("Unit","Treatment"))
# Dead -----
#MSD_trees_sl <- M_trees_sl[Type == "Snag" & DBH >=7.5,
 #                          .(MgUnit = sum(Mg_treeC)), by = .(unit, Year)]
MSD_trees_d <- M_trees_sl[Type == "Snag"]
MSD_trees_sl <- MSD_trees_d[,.(MgHa = sum(na.omit(Mg_treeC))), by = .(unit, Year)]
MSD_trees_sl <- merge(SummitLakeData::Treatments, MSD_trees_sl, by = "unit", all = TRUE)
MSD_trees_sl[, State := "Dead"]
setnames(MSL_trees_sl, c("unit", "treatment"), c("Unit","Treatment"))

#output -
saveRDS(MSL_trees_sl, file.path(out_path,"MSL_trees_sl.RDS"))
saveRDS(MSD_trees_sl, file.path(out_path,"MSD_trees_sl.RDS"))


#by species ------------------
# Live -----
#MSL_trees_sl_sp <- M_trees_sl[Type == "Adult" & DBH >=7.5,
 #                             .(MgUnit = sum(Mg_treeC)), by = .(unit, Year, Species)]
MSL_trees_sl_sp <- MSL_trees_l[,.(MgHa = sum(na.omit(Mg_treeC))), by = .(unit, Year, Species)]
MSL_trees_sl_sp <- merge(SummitLakeData::Treatments, MSL_trees_sl_sp, by = c("unit"), all = TRUE)
MSL_trees_sl_sp[, State := "Live"]
setnames(MSL_trees_sl_sp, c("unit", "treatment"), c("Unit","Treatment"))
# Dead -----
#MSD_trees_sl_sp <- M_trees_sl[Type == "Snag" & DBH >=7.5,
 #                             .(MgUnit = sum(Mg_treeC)), by = .(unit, Year, Species)]
MSD_trees_sl_sp <- MSD_trees_d[,.(MgHa = sum(na.omit(Mg_treeC))), by = .(unit, Year, Species)]
MSD_trees_sl_sp <- merge(SummitLakeData::Treatments, MSD_trees_sl_sp, by = c("unit"), all = TRUE)
MSD_trees_sl_sp[, State := "Dead"]
setnames(MSD_trees_sl_sp, c("unit", "treatment"), c("Unit","Treatment"))

saveRDS(MSL_trees_sl_sp, file.path(out_path,"MSL_trees_sl_sp.RDS"))
saveRDS(MSD_trees_sl_sp, file.path(out_path,"MSD_trees_sl_sp.RDS"))



### Field -------------------------------------------------------------------------------------
# using dima-height relationship - same as Date Creek
F_trees_sl <- SummitLakeData::clean_trees(raw_data = 
                                            "D:/Github/SummitLakeData/data-raw/SummitLakeData.csv")

#get the treatments & remove the unit/year combos that were not sampled:
treats <- SummitLakeData::Treatments[, .(Year = rep(sort(unique(F_trees_sl$Year)), 
                                                    length.out = .N*length(unique(F_trees_sl$Year)))),
                                     by = .(unit, treatment)]
remeas_freq_m <- melt(remeas_freq, id.vars = "PSP", variable.factor = FALSE)
setkey(remeas_freq_m, PSP)
setnames(remeas_freq_m, c("PSP","variable"),c("unit","Year"))
remeas_freq_m[,Year := as.numeric(Year)]
treats_surveyed <- merge(treats, remeas_freq_m[!is.na(value)], by = c("unit","Year"))
treats_surveyed[,value:=NULL]

#only use trees > 7.5cm dbh (moved to 7.5 cutoff in 2009)
#F_trees_sl <- F_trees_sl[DBH >= 7.5]

# by unit -------------------
#each plot was 0.05ha in size (12.6m radius)
# Live -----
FSL_trees_sl <- F_trees_sl[State == "Live" & DeadStatus == "NotDown",
                           .(MgHa = sum(Mg_treeC)*20), by = .(unit, Year, State)]
FSL_trees_sl <- merge(treats_surveyed, FSL_trees_sl, by = c("unit","Year"), all = TRUE)
FSL_trees_sl[is.na(MgHa), `:=`(MgHa = 0, State = "Live")]
setnames(FSL_trees_sl, c("unit", "treatment"), c("Unit","Treatment"))
# Dead -----
FSD_trees_sl <- F_trees_sl[State == "Dead" & DeadStatus == "Dead",
                           .(MgHa = sum(Mg_treeC)*20), by = .(unit, Year, State)]
FSD_trees_sl <- merge(treats_surveyed, FSD_trees_sl, by = c("unit","Year"), all = TRUE)
FSD_trees_sl[is.na(MgHa), `:=`(MgHa = 0, State = "Dead")]
setnames(FSD_trees_sl, c("unit", "treatment"), c("Unit","Treatment"))

saveRDS(FSL_trees_sl, file.path(out_path,"FSL_trees_sl.RDS"))
saveRDS(FSD_trees_sl, file.path(out_path,"FSD_trees_sl.RDS"))

#by species ------------------
# Live -----
FSL_trees_sl_sp <- F_trees_sl[State == "Live" & DeadStatus == "NotDown",
                           .(MgHa = sum(Mg_treeC)*20), by = .(unit, Year, State, Species)]
FSL_trees_sl_sp <- merge(treats_surveyed, FSL_trees_sl_sp, by = c("unit","Year"), all = TRUE)
FSL_trees_sl_sp[is.na(MgHa), `:=`(MgHa = 0, State = "Live")]
setnames(FSL_trees_sl_sp, c("unit", "treatment"), c("Unit","Treatment"))

# Dead -----
FSD_trees_sl_sp <- F_trees_sl[State == "Dead" & DeadStatus == "Dead",
                           .(MgHa = sum(Mg_treeC)*20), by = .(unit, Year, State, Species)]
FSD_trees_sl_sp <- merge(treats_surveyed, FSD_trees_sl_sp, by = c("unit","Year"), all = TRUE)
FSD_trees_sl_sp[is.na(MgHa), `:=`(MgHa = 0, State = "Dead")]
setnames(FSD_trees_sl_sp, c("unit", "treatment"), c("Unit","Treatment"))

saveRDS(FSL_trees_sl_sp, file.path(out_path,"FSL_trees_sl_sp.RDS"))
saveRDS(FSD_trees_sl_sp, file.path(out_path,"FSD_trees_sl_sp.RDS"))



#snag fall?

#ICH--------------------------------------------------------------------------------------------
### SORTIE ----------------------------------------------------
dc_in_path <- file.path(in_path, "01_date_creek","extracted")

run_name <- "ah"
outfiles <- grep("grids",grep(run_name, list.files(dc_in_path, pattern = ".csv", 
                                      full.names = TRUE), value = TRUE),
                 value = TRUE, invert = TRUE)

tree_dt <- rbindlist(lapply(outfiles, fread), fill = TRUE)

#because this comes from the GUI, need to change column names:
setnames(tree_dt,
         c("Species"),
         c("tree_species"))

## Basal area
#tree_dt[, ':='(BA = treeCalcs::calc_BA(DBH))]

## Carbon
#use allometry to calculate height (From treeCalcs package)
M_trees_dc_sa <- tree_dt[Type != "Seedling"]
M_trees_dc_sa <- treeCalcs::sortie_tree_carbon(sortie_outputs = M_trees_dc_sa, 
                                            dead = TRUE, 
                                            BEC = "ICH",
                                            ht_from_diam = "standard")

M_trees_dc_sa[, Mg_treeC := ifelse(Type == "Seedling",
                                treeCalcs::calc_sm_tree_c_Ung(Species = Spp,
                                                              Height_class = "31-130",
                                                              Diam_est = 1,
                                                              Health = "L"),
                                ifelse(is.na(cruise_hgt) | cruise_hgt == 0,
                                       treeCalcs::calc_tree_c(Species = Spp,
                                                              DBH = DBH,
                                                              HT = Height,
                                                              Tree_class = Tree.Class),
                                       treeCalcs::calc_tree_c(Species = Spp,
                                                              DBH = DBH,
                                                              HT = cruise_hgt, 
                                                              Tree_class = Tree.Class))),
           by= seq_len(nrow(F_trees_dc))]

M_trees_dc_sdls <- tree_dt[Type == "Seedling"]
M_trees_dc_sdls[, Species:= ifelse(tree_species == "Western_redcedar", "Cw",
                      ifelse(tree_species == "Western_Hemlock", "Hw",
                        ifelse(tree_species == "Subalpine_Fir", "Bl",
                          ifelse(tree_species == "Hybrid_spruce","Sx",
                            ifelse(tree_species == "Interior_Spruce","Sx",
                              ifelse(tree_species == "Paper_Birch","Ep",
                                ifelse(tree_species == "Black_Cottonwood","Ac",
                                  ifelse(tree_species == "Amabalis_Fir","Ba",
                                   ifelse(tree_species == "Trembling_Aspen","At",
                                    ifelse(tree_species == "Lodgepole_Pine","Pl",
                                     ifelse(tree_species == "Douglas_Fir","Fd",
                                      ifelse(tree_species == "Western_Larch", "Lw",NA))))))))))))]
M_trees_dc_sdls[, hgt_cl := ifelse(Height <= 30, "0-30",
                                   "31-130")]
M_trees_dc_sdls[, dbh_est := ifelse(hgt_cl == "0-30", 0.1, 1)]
M_trees_dc_sdls[, Kg_treeC := treeCalcs::calc_sm_tree_c_Ung(Species = Species,
                                                  Height_class = hgt_cl,
                                                  Diam_est = dbh_est,
                                                  Health = "L"),
                by = seq_len(nrow(M_trees_dc_sdls))]



M_trees_dc[, Year := 1992 + timestep]

#M_trees_dc[, SPH := 10]
#M_trees_dc[, MgHa := Mg_treeC * SPH]

#do you want all live trees - or just over a certain DBH?
MSL_trees_dc <- M_trees_dc[Type != "Snag" & !is.na(Mg_treeC)]
#MSL_trees_dc <- M_trees_dc[Type == "Adult" & DBH >=7.5]
MSL_trees_dc_a <- MSL_trees_dc[,.(MgUnit = sum(na.omit(Mg_treeC))), 
                               by = .(Unit, Year, SubPlot)] #sum each subplot
#MSL_trees_dc_a <- MSL_trees_dc[,.(MgHa_SP = sum(na.omit(MgHa))), 
 #                              by = .(Unit, Year, SubPlot)] #sum each subplot
#to do - add in seedling carbon
MSL_trees_dc_ab <- MSL_trees_dc_a[,.(MgUnit = mean(MgUnit)), 
                               by = .(Unit, Year)] #average MgUnit across all subplots
MSL_trees_dc_ab[, MgHa := MgUnit/0.1] #divided by the ha measured in a subplot

MSL_trees_dc <- merge(DateCreekData::Treatments, MSL_trees_dc_ab, 
                      by = "Unit", all = TRUE)
MSL_trees_dc[, State := "Live"][,MgUnit := NULL]

# Dead -----
#MSD_trees_sl <- M_trees_sl[Type == "Snag" & DBH >=7.5,
#                          .(MgUnit = sum(Mg_treeC)), by = .(unit, Year)]
MSD_trees_dc <- M_trees_dc[Type == "Snag"]
#sum each subplot:
MSD_trees_dc_a <- MSD_trees_dc[,.(MgUnit = sum(na.omit(Mg_treeC))), 
                               by = .(Unit, Year, SubPlot)]
#average MgUnit across all subplots
MSD_trees_dc_ab <- MSD_trees_dc_a[,.(MgUnit = mean(MgUnit)), 
                                  by = .(Unit, Year)]
#divided by the ha measured in a subplot
MSD_trees_dc_ab[, MgHa := MgUnit/0.02]

MSD_trees_dc <- merge(DateCreekData::Treatments, MSL_trees_dc_ab, 
                      by = "Unit", all = TRUE)
MSD_trees_dc[, State := "Dead"]


#output -
saveRDS(MSL_trees_dc, file.path(out_path,"MSL_trees_dc.RDS"))
saveRDS(MSD_trees_dc, file.path(out_path,"MSD_trees_dc.RDS"))

#by species ------------------
# Live -----
#MSL_trees_sl_sp <- M_trees_sl[Type == "Adult" & DBH >=7.5,
#                             .(MgUnit = sum(Mg_treeC)), by = .(unit, Year, Species)]
#do you want all live trees - or just voer a certain DBH?
MSL_trees_dc_sp <- M_trees_dc[Type != "Snag" & !is.na(Mg_treeC)]
#MSL_trees_dc <- M_trees_dc[Type == "Adult" & DBH >=7.5]
MSL_trees_dc_sp_a <- MSL_trees_dc_sp[,.(MgUnit = sum(na.omit(Mg_treeC))), 
                                     by = .(Unit, Year, SubPlot, Species)] #sum each subplot
#instead of adding all the 0s, just add total and divide by number of subplots
MSL_trees_dc_sp_ab <- MSL_trees_dc_sp_a[,.(MgUnit = sum(MgUnit)), 
                                  by = .(Unit, Year, Species)] #sum MgUnit across all subplots
samp_int <- M_trees_dc[,.(SampInt = max(SubPlot)),by = .(Unit, Year)]
MSL_trees_dc_sp_ab <- merge(MSL_trees_dc_sp_ab, samp_int, by = c("Unit", "Year"))
MSL_trees_dc_sp_ab[, MgHa := MgUnit/(0.02*SampInt)]
MSL_trees_dc_sp <- merge(DateCreekData::Treatments, MSL_trees_dc_sp_ab, 
                            by = c("Unit"), all = TRUE)
MSL_trees_dc_sp[, State := "Live"]


# Dead -----
#MSD_trees_sl_sp <- M_trees_sl[Type == "Snag" & DBH >=7.5,
#                             .(MgUnit = sum(Mg_treeC)), by = .(unit, Year, Species)]
MSD_trees_dc_sp <- M_trees_dc[Type == "Snag"]
#MSL_trees_dc <- M_trees_dc[Type == "Adult" & DBH >=7.5]
MSD_trees_dc_sp_a <- MSD_trees_dc_sp[,.(MgUnit = sum(na.omit(Mg_treeC))), 
                                     by = .(Unit, Year, SubPlot, Species)] #sum each subplot
#instead of adding all the 0s, just add total and divide by number of subplots
MSD_trees_dc_sp_ab <- MSD_trees_dc_sp_a[,.(MgUnit = sum(MgUnit)), 
                                        by = .(Unit, Year, Species)] #sum MgUnit across all subplots
samp_int <- M_trees_dc[,.(SampInt = max(SubPlot)),by = .(Unit, Year)]
MSD_trees_dc_sp_ab <- merge(MSD_trees_dc_sp_ab, samp_int, by = c("Unit", "Year"))
MSD_trees_dc_sp_ab[, MgHa := MgUnit/(0.02*SampInt)]
MSD_trees_dc_sp <- merge(DateCreekData::Treatments, MSD_trees_dc_sp_ab, 
                         by = c("Unit"), all = TRUE)
MSD_trees_dc_sp[, State := "Dead"]

saveRDS(MSL_trees_dc_sp, file.path(out_path,"MSL_trees_dc_sp.RDS"))
saveRDS(MSD_trees_dc_sp, file.path(out_path,"MSD_trees_dc_sp.RDS"))


### Field ------------------------------------------------------------------------
tree_dt_92 <- DateCreekData::trees_1992(cruise_data = "D:/Github/DateCreekData/data-raw/Trees/1992data.csv",
                                           fixed_data = "D:/Github/DateCreekData/data-raw/Trees/1992fixed_radius_data_fromTable20_DateCkHandbook.csv",
                                        calc_height = TRUE) 
tree_dt_93 <- DateCreekData::trees_1993(data =  "D:/Github/DateCreekData/data-raw/Trees/SS93forR.csv",
                                        calc_height = TRUE) 

tree_dt_10 <- DateCreekData::trees_2010(lrg_trees = "D:/Github/DateCreekData/data-raw/Trees/Data Creek 2010 Data large trees.csv",
                                        cc_trees = "D:/Github/DateCreekData/data-raw/Trees/Trees 10cm and above in clearcut 2010.csv",
                                        small_trees = "D:/Github/DateCreekData/data-raw/Trees/Date Creek 2010 Trees less than 10 cm tallies.csv",
                                        snag_heights = "D:/Github/DateCreekData/data-raw/Trees/SnagHeights2010.csv",
                                        measured_heights2022 = "D:/Github/DateCreekData/data-raw/Trees/StandStructureData_Nov2022_Final.xlsx",
                                        calc_height = TRUE)
tree_dt_18 <- DateCreekData::trees_201x(data_file = "D:/Github/DateCreekData/data-raw/Trees/Date Creek 2018 Data large trees_re-entered.xlsx",
                                        data_2018 = "DataCk re-entry 2018 largeTrees",
                                        data_2019 = "D:/Github/DateCreekData/data-raw/Trees/Data Creek 2019 Data large trees.csv",
                                        inter_trees = "D:/Github/DateCreekData/data-raw/Trees/2018-19intermediatetrees.csv",
                                        small_trees = "D:/Github/DateCreekData/data-raw/Trees/Small trees 2018 2019 KHP.csv",
                                        lrg_trees_2010 = "D:/Github/DateCreekData/data-raw/Trees/Data Creek 2010 Data large trees.csv",
                                        measured_heights2022 = "D:/Github/DateCreekData/data-raw/Trees/StandStructureData_Nov2022_Final.xlsx",
                                        calc_height = TRUE)

tree_dt_22 <- DateCreekData::trees_2022(data_file = "StandStructureData_Nov2022_Final.xlsx",
                                        large_trees = "Large",
                                        inter_trees = "Inter",
                                        small_trees = "Small",
                                        calc_height = TRUE)



cruise_labs_92 <- data.table(DateCreekData::get_all_plots_92(cruise_data =
                                                               "D:/Github/DateCreekData/data-raw/Trees/1992data.csv"))
cruise_labs_92[, Year := 1992]

tree_dt_92[, Type := ifelse(is.na(DBH), "Seedling",
                            ifelse(DBH < 5, "Sapling", "Adult"))]

F_trees_dc <- rbind(tree_dt_92)
F_trees_dc <- merge(F_trees_dc, DateCreekData::Treatments)
#get all the years and then do this:
#using calculated heights unless it's a stub, then use stub heights - THIS IS DIFF THAN SORTIE, so
#likely we should skip this step. 2 trees in 92 that are dead stubs with height 0.

#use plantation heights for plantation trees after 1992
F_trees_dc[, calc_height := ifelse(Treatment == "CC",
                                   treeCalcs::height_dbh_plantations(Species = Spp,
                                                                     DBH = DBH,
                                                                     BECzone = "ICH"),
                                   treeCalcs::height_dbh(Species = Spp,
                                                         DBH = DBH,
                                                         BECzone = "ICH")),
           by= seq_len(nrow(F_trees_dc))]




#separate out the seedlings.
F_trees_dc[, Kg_treeC := ifelse(Type == "Seedling",
                                 treeCalcs::calc_sm_tree_c_Ung(Species = Spp,
                                                                Height_class = "31-130",
                                                                Diam_est = 1,
                                                                Health = "L"),
                                 ifelse(StubYN == "N",
                                        treeCalcs::calc_tree_c(Species = Spp,
                                                               DBH = DBH,
                                                               HT = Height,
                                                               Tree_class = Tree.Class),
                                        treeCalcs::calc_tree_c(Species = Spp,
                                                               DBH = DBH,
                                                               HT = Height, #use calc height even if stub
                                                               Tree_class = Tree.Class))),
    by= seq_len(nrow(F_trees_dc))]
#so this one jives with Erica's calcs
F_trees_dc[, Kg_treeC := ifelse(Type == "Seedling",
                                treeCalcs::calc_sm_tree_c_Ung(Species = Spp,
                                                              Height_class = "31-130",
                                                              Diam_est = 1,
                                                              Health = "L"),
                                ifelse(is.na(cruise_hgt) | cruise_hgt == 0,
                                       treeCalcs::calc_tree_c(Species = Spp,
                                                              DBH = DBH,
                                                              HT = Height,
                                                              Tree_class = Tree.Class),
                                       treeCalcs::calc_tree_c(Species = Spp,
                                                              DBH = DBH,
                                                              HT = cruise_hgt, 
                                                              Tree_class = Tree.Class))),
           by= seq_len(nrow(F_trees_dc))]

F_trees_dc[, Mg_treeC := Kg_treeC/1000]
F_trees_dc[, MgHa := Mg_treeC * SPH]

# live trees:
FL_trees_dc <- F_trees_dc[Tree.Class <= 2]
FL_trees_dc_c <- FL_trees_dc[PlotType == "V" , .(MgHa_plot = sum(MgHa)), 
                               by = .(Unit, Year, PlotNum)] #sum carbon by cruise plots
FL_trees_dc_c <- merge(FL_trees_dc_c, cruise_labs_92, 
                        by = c("Unit","Year","PlotNum"), all.y = TRUE)
FL_trees_dc_c[is.na(MgHa_plot), MgHa_plot := 0] #merge with all plot ids to capture 0s:
FL_trees_dc_c  <- FL_trees_dc_c[, .(MgHa = mean(MgHa_plot)), 
                                  by = .(Unit, Year)]#take the average across cruise plots:
#add the carbon from fixed plots (all live)
FL_trees_dc_f <- FL_trees_dc[PlotType == "F"]
FL_trees_dc_f <- FL_trees_dc_f[, .(MgHa = sum(MgHa)),
                                 by = .(Unit, Year)]

FL_trees_dc_cf <- rbind(FL_trees_dc_c, FL_trees_dc_f)
FSL_trees_dc <- FL_trees_dc_cf[, .(MgHa = sum(MgHa)), by = .(Unit, Year)]

# dead trees:
FD_trees_dc <- F_trees_dc[Tree.Class > 2]
FD_trees_dc_c <- FD_trees_dc[PlotType == "V" , .(MgHa_plot = sum(MgHa)), 
                             by = .(Unit, Year, PlotNum)] #sum carbon by cruise plots
FD_trees_dc_c <- merge(FD_trees_dc_c, cruise_labs_92, 
                       by = c("Unit","Year","PlotNum"), all.y = TRUE)
FD_trees_dc_c[is.na(MgHa_plot), MgHa_plot := 0] #merge with all plot ids to capture 0s:
FD_trees_dc_c  <- FD_trees_dc_c[, .(MgHa = mean(MgHa_plot)), 
                                by = .(Unit, Year)]#take the average across cruise plots:
#add the carbon from fixed plots (all live)
FD_trees_dc_f <- FD_trees_dc[PlotType == "F"]
FD_trees_dc_f <- FD_trees_dc_f[, .(MgHa = sum(MgHa)),
                               by = .(Unit, Year)]

FD_trees_dc_cf <- rbind(FD_trees_dc_c, FD_trees_dc_f)
FSD_trees_dc <- FD_trees_dc_cf[, .(MgHa = sum(MgHa)), by = .(Unit, Year)]












saveRDS(FSL_trees_dc, file.path(out_path, "FSL_trees_dc.RDS"))
saveRDS(FSL_trees_dc, file.path(out_path, "FSD_trees_dc.RDS"))


#1993:-------------------------------------------------------
# Count how many plots there are for each treatment unit
# so when averaging carbon/unit later, we can take into account the plots that had zero C
dat.1993$count<-rep(1, length(dat.1993$Unit ))
Trees_in_Plots<-ddply(dat.1993[c("Unit", "PlotNum", "count")], .(Unit, PlotNum), numcolwise(sum))
Trees_in_Plots$count<-rep(1, length(Trees_in_Plots$Unit ))
Plot_in_Units<-ddply(Trees_in_Plots[c("Unit", "count")], .(Unit), numcolwise(sum))
Plot_in_Units #stands should have 23 or 30 plots (30 for 40% retention treatments)

#2010:-------------------------------------------------------
# Count how many plots there are for each treatment unit
# so when averaging carbon/unit later, we can take into account the plots that had zero C
dat.2010$count<-rep(1, length(dat.2010$Unit ))
Trees_in_Plots<-ddply(dat.2010[c("Unit", "Gd.Pt","Plot.Size", "count")], .(Unit, Gd.Pt,Plot.Size), numcolwise(sum))
Trees_in_Plots$count<-rep(1, length(Trees_in_Plots$Unit ))
Plot_in_Units<-ddply(Trees_in_Plots[c("Unit", "count")], .(Unit), numcolwise(sum))
Plot_in_Units #stands should have 20 or 30 plots (30 for 40% and 70% retention treatments)
# C2 is down 2 plots
#no clear-cut large tree plots

#201x--------------------------------------------------------
# Count how many plots there are for each treatment unit
# so when averaging carbon/unit later, we can take into account the plots that had zero C
dat.2018x$count <- rep(1, length(dat.2018x$Unit))
Trees_in_Plots <- ddply(dat.2018x[c("Unit", "Gd.Pt","Plot.Size", "count")], .(Unit, Gd.Pt,Plot.Size), numcolwise(sum))
Trees_in_Plots$count <- rep(1, length(Trees_in_Plots$Unit))
Plot_in_Units <- ddply(Trees_in_Plots[c("Unit", "count")], .(Unit), numcolwise(sum))
Plot_in_Units

#2022--------------------------------------------------------
# Count how many plots there are for each treatment unit
# so when averaging carbon/unit later, we can take into account the plots that had zero C
#read in plot labels to calculate original number of plots per unit
labels2022<-read.csv("Cleaned 2022 labels.csv", stringsAsFactors = TRUE)
labels2022$count <- rep(1, length(labels2022$Unit))
Plot_in_Units <- ddply(labels2022[c("Unit", "count")], .(Unit), numcolwise(sum))
Plot_in_Units #stands should have 20 or 30 plots (30 for 40% and 70% retention treatments),





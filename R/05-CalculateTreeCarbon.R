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

run_name <- "ds-nci_si_6"
outfiles <- grep(run_name, list.files(sum_in_path, pattern = ".csv", 
                                      full.names = TRUE), value = TRUE)
plots <- stringr::str_split(list.files(My_newvalsPath, pattern = "summit"),".csv",
                            simplify = TRUE)[,1]

tree_files <- grep("trees",outfiles, value = TRUE)
#tree_dt <- rbindlist(lapply(tree_files, fread))
tree_dt <- rbindlist(
  lapply(tree_files, function(file) {
    # Read each file
    fread(file) # Read all as character to avoid type issues
  }),
  use.names = TRUE, # Match columns by name
  fill = TRUE       # Fill missing columns with NA
)

#tree_dt[!is.na(dead)]
#unique(tree_dt[!is.na(Fall)]$Fall)
#tree_dt[Fall == 2]
#clip to centre 1 ha:
tree_dt <- tree_dt[X >50 & X <150 & Y >50 & Y <150]

## Basal area
#tree_dt[, ':='(BA = treeCalcs::calc_BA(DBH))]

## Carbon
#use allometry to calculate height (From treeCalcs package)
M_trees_sl <- treeCalcs::sortie_tree_carbon(sortie_outputs = tree_dt, 
                                            BEC = "SBS",
                                            ht_from_diam  = "standard")
M_trees_sl[, unit := as.numeric(tstrsplit(Unit, "t_", fixed = TRUE)[[2]])][,Unit:=NULL]
M_trees_sl[, Year := ifelse(unit == 4, 1994 + timestep,
                        ifelse(unit == 15, 1994 + timestep,
                               1992 + timestep))]
M_trees_sl[, BA := pi*(DBH/200)^2,
           by = seq_len(nrow(M_trees_sl))]


####################################################################
# Live -----
#MSL_trees_sl <- M_trees_sl[Type == "Adult" & DBH >=7.5,
 #                         .(MgUnit = sum(Mg_treeC)), by = .(unit, Year)]
MSL_trees_l <- M_trees_sl[Type != "Snag"& DBH >=7.5]
MSL_trees_sl <- MSL_trees_l[,.(MgHa = sum(na.omit(Mg_treeC)),
                               BaHa = sum(na.omit(BA))), 
                            by = .(unit, Year)]
MSL_trees_sl <- merge(SummitLakeData::Treatments, MSL_trees_sl, 
                      by = "unit", all = TRUE)
MSL_trees_sl[, State := "Live"]
setnames(MSL_trees_sl, c("unit", "treatment"), c("Unit","Treatment"))

# Dead -----
MSD_trees_d <- M_trees_sl[Type == "Snag" & DBH >=7.5]
MSD_trees_sl <- MSD_trees_d[,.(MgHa = sum(na.omit(Mg_treeC)),
                               BaHa = sum(na.omit(BA))), 
                            by = .(unit, Year)]
MSD_trees_sl <- merge(SummitLakeData::Treatments, 
                      MSD_trees_sl, by = "unit", all = TRUE)
MSD_trees_sl[, State := "Dead"]
setnames(MSD_trees_sl, c("unit", "treatment"), c("Unit","Treatment"))

#output -
saveRDS(MSL_trees_sl, file.path(out_path,"MSL_trees_sl.RDS"))
saveRDS(MSD_trees_sl, file.path(out_path,"MSD_trees_sl.RDS"))


#by species ------------------
# Live -----
#MSL_trees_sl_sp <- M_trees_sl[Type == "Adult" & DBH >=7.5,
 #                             .(MgUnit = sum(Mg_treeC)), by = .(unit, Year, Species)]
MSL_trees_sl_sp <- MSL_trees_l[,.(MgHa = sum(na.omit(Mg_treeC)),
                                  BaHa = sum(na.omit(BA))), 
                               by = .(unit, Year, Species)]
unit_sp <- CJ(unit = SummitLakeData::Treatments$unit, Species = c("Sx", "Bl"))
unit_sp <- merge(SummitLakeData::Treatments, unit_sp, by = "unit")
MSL_trees_sl_sp <- merge(unit_sp, 
                         MSL_trees_sl_sp, by = c("unit", "Species"), all = TRUE)
MSL_trees_sl_sp[is.na(MgHa)]
MSL_trees_sl_sp[is.na(BaHa)]
MSL_trees_sl_sp[, State := "Live"]
setnames(MSL_trees_sl_sp, c("unit", "treatment"), c("Unit","Treatment"))

# Dead -----
#MSD_trees_sl_sp <- M_trees_sl[Type == "Snag" & DBH >=7.5,
 #                             .(MgUnit = sum(Mg_treeC)), by = .(unit, Year, Species)]
MSD_trees_sl_sp <- MSD_trees_d[,.(MgHa = sum(na.omit(Mg_treeC)),
                                  BaHa = sum(na.omit(BA))), 
                               by = .(unit, Year, Species)]
MSD_trees_sl_sp <- merge(unit_sp, MSD_trees_sl_sp, 
                         by = c("unit", "Species"), all = TRUE)
MSD_trees_sl_sp[is.na(MgHa), MgHa := 0]
MSD_trees_sl_sp[is.na(BaHa), BaHa := 0]
MSD_trees_sl_sp[, State := "Dead"]
setnames(MSD_trees_sl_sp, c("unit", "treatment"), c("Unit","Treatment"))

saveRDS(MSL_trees_sl_sp, file.path(out_path,"MSL_trees_sl_sp.RDS"))
saveRDS(MSD_trees_sl_sp, file.path(out_path,"MSD_trees_sl_sp.RDS"))



### Field -------------------------------------------------------------------------------------
# using diam-height relationship - same as Date Creek
F_trees_sl <- SummitLakeData::clean_trees(raw_data = 
                                            "D:/Github/SummitLakeData/data-raw/SummitLakeData.csv")
#saveRDS(F_trees_sl, file.path(out_path,"F_trees_sl.RDS"))

# Calculate carbon per tree 
F_trees_sl[, Kg_treeC := treeCalcs::calc_tree_c(Species = Species,
                                                DBH = DBH,
                                                HT = ifelse(is.na(meas_hgt),
                                                        Height,meas_hgt),
                                                Tree_class = Class),
           by= seq_len(nrow(F_trees_sl))][, Mg_treeC := Kg_treeC/1000]

#F_trees_sl[, Kg_treeC := treeCalcs::calc_tree_c(Species = Species,
 #                                        DBH = DBH,
  #                                       HT = Height,
   #                                      Tree_class = Class),
  #  by= seq_len(nrow(F_trees_sl))][, Mg_treeC := Kg_treeC/1000]

#F_trees_sl[, Kg_treeC := ifelse(is.na(meas_hgt), treeCalcs::calc_tree_c(Species = Species,
 #                                               DBH = DBH,
  #                                              HT = Height,
   #                                             Tree_class = Class), meas_hgt),
    #       by= seq_len(nrow(F_trees_sl))][, Mg_treeC := Kg_treeC/1000]


# Calculate basal area per tree
F_trees_sl[, BA := pi*(DBH/200)^2 ,
           by= seq_len(nrow(F_trees_sl))
                                                ]
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

saveRDS(FSL_trees_sl, file.path(out_path,"FSL_trees_sl.RDS"))
saveRDS(FSD_trees_sl, file.path(out_path,"FSD_trees_sl.RDS"))

#by species ------------------
unit_sp <- treats_surveyed[,.(Species = c("Sx","Bl")), 
                           by = .(unit, Year, treatment)]

# Live -----
FSL_trees_sl_sp <- F_trees_sl[State == "Live" & DeadStatus == "NotDown" & DBH >= 7.5,
                           .(MgHa = sum(Mg_treeC)*20,
                             BaHa = sum(na.omit(BA))*20), 
                           by = .(unit, Year, State, Species)]
FSL_trees_sl_sp <- merge(unit_sp, FSL_trees_sl_sp, 
                         by = c("unit","Year", "Species"), all.x = TRUE)
FSL_trees_sl_sp[is.na(MgHa), `:=`(MgHa = 0, State = "Live")]
FSL_trees_sl_sp[is.na(BaHa), `:=`(BaHa = 0)]
setnames(FSL_trees_sl_sp, c("unit", "treatment"), c("Unit","Treatment"))

# Dead -----
#FSD_trees_sl_sp <- F_trees_sl[State == "Dead" & DeadStatus == "Dead" & DBH >= 7.5,
#                           .(MgHa = sum(Mg_treeC)*20,
#                             BaHa = sum(na.omit(BA))*20), 
#                           by = .(unit, Year, State, Species)]
#FSD_trees_sl_sp <- merge(unit_sp, FSD_trees_sl_sp, 
#                         by = c("unit","Year", "Species"), all.x = TRUE)
#FSD_trees_sl_sp[is.na(MgHa), `:=`(MgHa = 0, State = "Dead")]
#FSD_trees_sl_sp[is.na(BaHa), `:=`(BaHa = 0)]
#setnames(FSD_trees_sl_sp, c("unit", "treatment"), c("Unit","Treatment"))

saveRDS(FSL_trees_sl_sp, file.path(out_path,"FSL_trees_sl_sp.RDS"))
#saveRDS(FSD_trees_sl_sp, file.path(out_path,"FSD_trees_sl_sp.RDS"))
#snag fall?






#ICH--------------------------------------------------------------------------------------------
### SORTIE ----------------------------------------------------
dc_in_path <- file.path(in_path, "01_date_creek","extracted")

outfiles <- grep("grids",list.files(dc_in_path, pattern = ".csv", 
                                      full.names = TRUE),
                 value = TRUE, invert = TRUE)
outfiles <- grep("ah",outfiles, invert = T, value = T)
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
                                            BEC = "ICH",
                                            ht_from_diam = "standard")

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
M_trees_dc_sdls[, Mg_treeC := Kg_treeC/1000]
M_trees_dc_sdls[, Class := 1]
M_trees_dc <- rbind(M_trees_dc_sa, M_trees_dc_sdls, fill = TRUE)

M_trees_dc[, Year := 1992 + timestep]
M_trees_dc[, BA := pi*(DBH/200)^2]
#M_trees_dc[, QMD := sqrt(mean(DBH^2))]

#M_trees_dc[, SPH := 10]
#M_trees_dc[, MgHa := Mg_treeC * SPH]

#### #which trees count for carbon (and basal area and quadratic growth) ######
#MSL_trees_dc <- M_trees_dc[Type == "Adult" & DBH >=7.5] ?
#1. remove dead trees with DBH <7.5 to match 1992 Field data that only counted live trees <7.5 cm DBH
M_trees_dc <- M_trees_dc[DBH >= 7.5 | Type == "Adult"]

#2. remove dead regen <1.3 m tall to match other years
M_trees_dc <- M_trees_dc[DBH > 0 | Dead.Code == "Alive"] #other year regen maybe not working

#do you want all live trees - or just over a certain DBH?
MSL_trees_dc <- M_trees_dc[Type != "Snag" & !is.na(Mg_treeC)]

MSL_trees_dc_a <- MSL_trees_dc[,.(MgUnit = sum(na.omit(Mg_treeC)), 
                                  BaUnit = sum(na.omit(BA)),
                                  QMD = sqrt(mean(na.omit(DBH)^2))), 
                               by = .(Unit, Year, SubPlot)] #sum each subplot and get QMD for each subplot
#MSL_trees_dc_a <- MSL_trees_dc[,.(MgHa_SP = sum(na.omit(MgHa))), 
 #                              by = .(Unit, Year, SubPlot)] #sum each subplot
#number of subplots:
subs <- unique(M_trees_dc[,.(Unit, Year, SubPlot)])
MSL_trees_dc_ab <- merge(MSL_trees_dc_a, subs, all.y = TRUE)
MSL_trees_dc_ab[is.na(MgUnit), `:=`(MgUnit = 0, BaUnit = 0, QMD = 0)]

MSL_trees_dc_abc <- MSL_trees_dc_ab[,.(MgUnit = mean(MgUnit), 
                                     BaUnit = mean(BaUnit),
                                     QMD = mean(na.omit(QMD))), 
                               by = .(Unit, Year)] #average MgUnit across all subplots
MSL_trees_dc_abc[,`:=`(MgHa = MgUnit/0.1, 
                      BAHa = BaUnit/0.1)] #divided by the ha measured in a subplot

MSL_trees_dc <- merge(DateCreekData::Treatments, MSL_trees_dc_abc, 
                      by = "Unit", all = TRUE)
MSL_trees_dc[, State := "Live"][,MgUnit := NULL][,BaUnit := NULL]

# Dead -----
#MSD_trees_sl <- M_trees_sl[Type == "Snag" & DBH >=7.5,
#                          .(MgUnit = sum(Mg_treeC)), by = .(unit, Year)]
MSD_trees_dc <- M_trees_dc[Type == "Snag"]
#sum each subplot:
MSD_trees_dc_a <- MSD_trees_dc[,.(MgUnit = sum(na.omit(Mg_treeC)), 
                                  BaUnit = sum(na.omit(BA))), 
                               by = .(Unit, Year, SubPlot)]

MSD_trees_dc_ab <- merge(MSD_trees_dc_a, subs, all.y = TRUE)
MSD_trees_dc_ab[is.na(MgUnit), `:=`(MgUnit = 0, BaUnit = 0)]

#average MgUnit across all subplots
MSD_trees_dc_abc <- MSD_trees_dc_ab[,.(MgUnit = mean(MgUnit), 
                                     BaUnit = mean(BaUnit)), 
                                  by = .(Unit, Year)]
#divided by the ha measured in a subplot
MSD_trees_dc_abc[, MgHa := MgUnit/0.1]
MSD_trees_dc_abc[, BAHa := BaUnit/0.1] #divided by the ha measured in a subplot
unit_yr <- DateCreekData::Treatments[, .(Year = seq(1992,2092)), by = .(Unit, Treatment)]
MSD_trees_dc <- merge(unit_yr, MSD_trees_dc_abc, 
                      by = c("Unit","Year"), all = TRUE)
MSD_trees_dc[, State := "Dead"][,MgUnit := NULL][,BaUnit :=NULL]
MSD_trees_dc[is.na(MgHa), `:=`(MgHa = 0, BAHa = 0)]


#output -
saveRDS(MSL_trees_dc, file.path(out_path,"MSL_trees_dc.RDS"))
saveRDS(MSD_trees_dc, file.path(out_path,"MSD_trees_dc.RDS"))

#by species -----------------------
# Live -----
#MSL_trees_sl_sp <- M_trees_sl[Type == "Adult" & DBH >=7.5,
#                             .(MgUnit = sum(Mg_treeC)), by = .(unit, Year, Species)]
#do you want all live trees - or just voer a certain DBH?
MSL_trees_dc_sp <- M_trees_dc[Type != "Snag" & !is.na(Mg_treeC)]
#MSL_trees_dc <- M_trees_dc[Type == "Adult" & DBH >=7.5]
MSL_trees_dc_sp_a <- MSL_trees_dc_sp[,.(MgUnit = sum(na.omit(Mg_treeC)),
                                        BaUnit = sum(na.omit(BA))), 
                                     by = .(Unit, Year, SubPlot, Species)] #sum each subplot
#instead of adding all the 0s, just add total and divide by the total number of subplots from the model run
MSL_trees_dc_sp_ab <- MSL_trees_dc_sp_a[,.(MgUnit = sum(MgUnit),
                                           BaUnit = sum(BaUnit)), 
                                  by = .(Unit, Year, Species)] #sum MgUnit across all subplots
samp_int <- MSL_trees_dc_sp[,.(SampInt = max(SubPlot)),
                       by = .(Unit)]
MSL_trees_dc_sp_ab <- merge(MSL_trees_dc_sp_ab, samp_int, by = c("Unit"))
MSL_trees_dc_sp_ab[, MgHa := MgUnit/(0.1*SampInt)][, BaHa := BaUnit/(0.1*SampInt)]
MSL_trees_dc_sp <- merge(DateCreekData::Treatments, MSL_trees_dc_sp_ab, 
                            by = c("Unit"), all = TRUE)

#missing 0s from clearcuts in 1993:
unit_tr_sp <- DateCreekData::Treatments[,.(Species = unique(MSL_trees_dc_sp$Species)), 
                       by = .(Treatment, Unit)]

unit_tr_sp_yr <- unit_tr_sp[,.(Year = unique(MSL_trees_dc_sp$Year)), 
                                        by = .(Treatment, Unit, Species)]
MSL_trees_dc_sp <- merge(MSL_trees_dc_sp, unit_tr_sp_yr, 
                         by = c("Treatment", "Year","Unit", "Species"), all = TRUE)
MSL_trees_dc_sp[is.na(MgHa),`:=`(MgHa = 0, BaHa = 0)]
MSL_trees_dc_sp[, State := "Live"][,MgUnit := NULL][,SampInt := NULL][,BaUnit := NULL]

#MSL_trees_dc_sp_m <- MSL_trees_dc_sp[Species == "Hw" | Species == "Cw"]
#MSL_trees_dc_sp_m



# Dead -----
#MSD_trees_sl_sp <- M_trees_sl[Type == "Snag" & DBH >=7.5,
#                             .(MgUnit = sum(Mg_treeC)), by = .(unit, Year, Species)]
#MSD_trees_dc_sp <- M_trees_dc[Type == "Snag"]
#MSL_trees_dc <- M_trees_dc[Type == "Adult" & DBH >=7.5]
#MSD_trees_dc_sp_a <- MSD_trees_dc_sp[,.(MgUnit = sum(na.omit(Mg_treeC))), 
 #                                    by = .(Unit, Year, SubPlot, Species)] #sum each subplot
#instead of adding all the 0s, just add total and divide by number of subplots
#MSD_trees_dc_sp_ab <- MSD_trees_dc_sp_a[,.(MgUnit = sum(MgUnit)), 
#                                        by = .(Unit, Year, Species)] #sum MgUnit across all subplots
#samp_int <- M_trees_dc[,.(SampInt = max(SubPlot)),by = .(Unit, Year)]
#MSD_trees_dc_sp_ab <- merge(MSD_trees_dc_sp_ab, samp_int, by = c("Unit", "Year"))
#MSD_trees_dc_sp_ab[, MgHa := MgUnit/(0.1*SampInt)]
#MSD_trees_dc_sp <- merge(DateCreekData::Treatments, MSD_trees_dc_sp_ab, 
 #                        by = c("Unit"), all = TRUE)
#MSD_trees_dc_sp[, State := "Dead"]

saveRDS(MSL_trees_dc_sp, file.path(out_path,"MSL_trees_dc_sp.RDS"))
#saveRDS(MSD_trees_dc_sp, file.path(out_path,"MSD_trees_dc_sp.RDS"))


### Field ------------------------------------------------------------------------
tree_dt_92 <- DateCreekData::trees_1992(cruise_data = "D:/Github/DateCreekData/data-raw/Trees/1992data.csv",
                                           fixed_data = "D:/Github/DateCreekData/data-raw/Trees/1992fixed_radius_data_fromTable20_DateCkHandbook.csv",
                                        calc_height = TRUE)
tree_dt_92 <- tree_dt_92[SPH != 0]
tree_dt_93 <- DateCreekData::trees_1993(data =  "D:/Github/DateCreekData/data-raw/Trees/SS93forR.csv",
                                        sm_tree = "D:/Github/DateCreekData/data-raw/Trees/1993_under1.3m_tree_tallies.csv",
                                        calc_height = TRUE,
                                        tree_cl_stub = "8") #tree class 8 considered stubin 1993

tree_dt_10 <- DateCreekData::trees_2010(lrg_trees = "D:/Github/DateCreekData/data-raw/Trees/Data Creek 2010 Data large trees.csv",
                                        cc_trees = "D:/Github/DateCreekData/data-raw/Trees/Trees 10cm and above in clearcut 2010.csv",
                                        small_trees = "D:/Github/DateCreekData/data-raw/Trees/Date Creek 2010 Trees less than 10 cm tallies.csv",
                                        snag_heights = "D:/Github/DateCreekData/data-raw/Trees/SnagHeights2010.csv",
                                        measured_heights2022 = "D:/Github/DateCreekData/data-raw/Trees/StandStructureData_Nov2022_Final.xlsx",
                                        calc_height = TRUE,
                                        use_standard = FALSE, #TRUE = standard, FALSE = residual 
                                        id_gap_trees = TRUE, #use gaps to trigger plantation allom
                                        use_size = TRUE, #use tree size to trigger plantation allom
                                        use_cc = TRUE, #use cc to trigger plantation allom
                                        planted_hgt = 20,
                                        planted_diam = 20,
                                        sp_plant = "Sx")


tree_dt_18 <- DateCreekData::trees_201x(data_file = "D:/Github/DateCreekData/data-raw/Trees/Date Creek 2018 Data large trees_re-entered.xlsx",
                                        data_2018 = "DataCk re-entry 2018 largeTrees",
                                        data_2019 = "D:/Github/DateCreekData/data-raw/Trees/Data Creek 2019 Data large trees.csv",
                                        inter_trees = "D:/Github/DateCreekData/data-raw/Trees/2018-19intermediatetrees.csv",
                                        small_trees = "D:/Github/DateCreekData/data-raw/Trees/Small trees 2018 2019 KHP.csv",
                                        lrg_trees_2010 = "D:/Github/DateCreekData/data-raw/Trees/Data Creek 2010 Data large trees.csv",
                                        measured_heights2022 = "D:/Github/DateCreekData/data-raw/Trees/StandStructureData_Nov2022_Final.xlsx",
                                        snag_hgts_18 = "D:/Github/DateCreekData/data-raw/Trees/SnagHeights2018.csv",
                                        calc_height = TRUE,
                                        use_standard = FALSE, #TRUE = standard, FALSE = residual
                                        id_gap_trees = TRUE, #use gaps to trigger plantation allom
                                        use_size = TRUE, #use tree size to trigger plantation allom
                                        use_cc = TRUE,#use cc to trigger plantation allom
                                        planted_hgt = 20,
                                        planted_diam = 20,
                                        sp_plant = "Sx")

tree_dt_22 <- DateCreekData::trees_2022(data_file = "D:/Github/DateCreekData/data-raw/Trees/StandStructureData_Nov2022_Final.xlsx",
                                        large_trees = "Large",
                                        inter_trees = "Inter",
                                        small_trees = "Small",
                                        calc_height = TRUE)

labs_92 <- data.table(DateCreekData::get_all_plots_1992(cruise_data =
                                                               "D:/Github/DateCreekData/data-raw/Trees/1992data.csv"))
labs_93 <- data.table(DateCreekData::get_all_plots_1993(data = "D:/Github/DateCreekData/data-raw/Trees/SS93forR.csv"))

labs_10 <- data.table(DateCreekData::get_all_plots_2010(lrg_trees = "D:/Github/DateCreekData/data-raw/Trees/Data Creek 2010 Data large trees.csv",
                                                        cc_trees = "D:/Github/DateCreekData/data-raw/Trees/Trees 10cm and above in clearcut 2010.csv",
                                                        small_trees = "D:/Github/DateCreekData/data-raw/Trees/Date Creek 2010 Trees less than 10 cm tallies.csv",
                                                        all_plots = TRUE))
labs_1x <- data.table(DateCreekData::get_all_plots_201x(data_file = "D:/Github/DateCreekData/data-raw/Trees/Date Creek 2018 Data large trees_re-entered.xlsx",
                                                        data_2018 = "DataCk re-entry 2018 largeTrees",
                                                        data_2019 = "D:/Github/DateCreekData/data-raw/Trees/Data Creek 2019 Data large trees.csv"))
labs_22 <- data.table(DateCreekData::get_all_plots_22(data_file = "D:/Github/DateCreekData/data-raw/Trees/StandStructureData_Nov2022_Final.xlsx",
                                                      large_trees = "Large"))

all_labels <- rbind(labs_92, labs_93, labs_10, labs_1x, labs_22)


#tree_dt_92[, Type := ifelse(is.na(DBH), "Seedling",
 #                           ifelse(DBH < 5, "Sapling", "Adult"))]

F_trees_dc <- rbind(tree_dt_92, tree_dt_93, tree_dt_10, tree_dt_18, tree_dt_22, fill = TRUE)

F_trees_dc <- merge(F_trees_dc, DateCreekData::Treatments)

#saveRDS(F_trees_dc, file.path(out_path, "F_trees_dc.RDS"))


#get all the years and then do this:
#using calculated heights unless it's a stub, then use stub heights - THIS IS DIFF THAN SORTIE, so
#likely we should skip this step. 2 trees in 92 that are dead stubs with height 0.

# decisions on heights to use -------------------------------------------------------------------

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

saveRDS(FSL_trees_dc, file.path(out_path, "FSL_trees_dc_mh.RDS"))
saveRDS(FD_trees_dc_c, file.path(out_path, "FSD_trees_dc_mh.RDS"))


#by species ------------------
# Live -----
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

saveRDS(FSL_trees_dc_sp, file.path(out_path, "FSL_trees_dc_sp_mh.RDS"))



#so this one jives with Erica's calcs
#F_trees_dc[, Kg_treeC := ifelse(Type == "Seedling",
 #                               treeCalcs::calc_sm_tree_c_Ung(Species = Spp,
  #                                                            Height_class = "31-130",
   #                                                           Diam_est = 1,
    #                                                          Health = "L"),
     #                           ifelse(is.na(cruise_hgt) | cruise_hgt == 0,
      #                                 treeCalcs::calc_tree_c(Species = Spp,
       #                                                       DBH = DBH,
        #                                                      HT = Height,
         #                                                     Tree_class = Tree.Class),
          #                             treeCalcs::calc_tree_c(Species = Spp,
           #                                                   DBH = DBH,
            #                                                  HT = cruise_hgt, 
             #                                                 Tree_class = Tree.Class))),
           #by= seq_len(nrow(F_trees_dc))]

#F_trees_dc[, Mg_treeC := Kg_treeC/1000]
#F_trees_dc[, MgHa := Mg_treeC * SPH]


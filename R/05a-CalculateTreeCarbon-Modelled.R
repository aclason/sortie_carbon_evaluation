library(data.table)
library(SummitLakeData)
library(DateCreekData)
library(treeCalcs) 

in_path <- file.path("03_out_sortie")
out_path <- file.path("04_out_carbon")


#SBS--------------------------------------------------------------------------------------------
#1. read in the extracted tree data:
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

#2. clip to centre 1 ha:
tree_dt <- tree_dt[X >50 & X <150 & Y >50 & Y <150]

#3. calculate basal area and carbon

## Carbon
#use allometry to calculate height (From treeCalcs package)
M_trees_sl <- treeCalcs::sortie_tree_carbon(sortie_outputs = tree_dt, 
                                            BEC = "SBS",
                                            ht_from_diam  = "standard")

M_trees_sl[, unit := as.numeric(tstrsplit(Unit, "t_", fixed = TRUE)[[2]])][,Unit:=NULL]
M_trees_sl[, Year := ifelse(unit == 4, 1994 + timestep,
                        ifelse(unit == 15, 1994 + timestep,
                               1992 + timestep))]

M_trees_sl[, BA := pi*(DBH/200)^2, #basal area
           by = seq_len(nrow(M_trees_sl))]

#4. Summarize Live tree data by plot & plot/species
MSL_trees_l <- M_trees_sl[Type != "Snag"& DBH >= 7.5]
MSL_trees_sl <- MSL_trees_l[,.(MgHa = sum(na.omit(Mg_treeC)),
                               BaHa = sum(na.omit(BA))), 
                            by = .(unit, Year)]
MSL_trees_sl <- merge(SummitLakeData::Treatments, 
                      MSL_trees_sl, 
                      by = "unit", all = TRUE)
MSL_trees_sl[, State := "Live"]
setnames(MSL_trees_sl, 
         c("unit", "treatment"), 
         c("Unit","Treatment"))
#by species:
MSL_trees_sl_sp <- MSL_trees_l[,.(MgHa = sum(na.omit(Mg_treeC)),
                                  BaHa = sum(na.omit(BA))), 
                               by = .(unit, Year, Species)]
unit_sp <- CJ(unit = SummitLakeData::Treatments$unit, 
              Species = c("Sx", "Bl"))
unit_sp <- merge(SummitLakeData::Treatments, 
                 unit_sp, 
                 by = "unit")
MSL_trees_sl_sp <- merge(unit_sp, 
                         MSL_trees_sl_sp, 
                         by = c("unit", "Species"), 
                         all = TRUE)
MSL_trees_sl_sp[is.na(MgHa)]
MSL_trees_sl_sp[is.na(BaHa)]
MSL_trees_sl_sp[, State := "Live"]
setnames(MSL_trees_sl_sp, 
         c("unit", "treatment"),
         c("Unit","Treatment"))


#5. Summarize Dead tree data by plot & plot/species
MSD_trees_d <- M_trees_sl[Type == "Snag" & DBH >=7.5]
MSD_trees_sl <- MSD_trees_d[,.(MgHa = sum(na.omit(Mg_treeC)),
                               BaHa = sum(na.omit(BA))), 
                            by = .(unit, Year)]
MSD_trees_sl <- merge(SummitLakeData::Treatments, 
                      MSD_trees_sl, by = "unit", all = TRUE)
MSD_trees_sl[, State := "Dead"]
setnames(MSD_trees_sl, c("unit", "treatment"), c("Unit","Treatment"))

#6. Save Outputs:
saveRDS(MSL_trees_sl, file.path(out_path,"MSL_trees_sl.RDS"))
saveRDS(MSD_trees_sl, file.path(out_path,"MSD_trees_sl.RDS"))
saveRDS(MSL_trees_sl_sp, file.path(out_path,"MSL_trees_sl_sp.RDS"))



#ICH--------------------------------------------------------------------------------------------
#1. read in the extracted tree data:
dc_in_path <- file.path(in_path, "01_date_creek","extracted")

outfiles <- grep("grids",list.files(dc_in_path, pattern = ".csv", 
                                      full.names = TRUE),
                 value = TRUE, invert = TRUE)
tree_dt <- rbindlist(lapply(outfiles, fread), fill = TRUE)

#because this comes from the GUI, need to change column names:
setnames(tree_dt,
         c("Species"),
         c("tree_species"))


#2. Calculate carbon and basal area
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

# remove dead trees with DBH <7.5 to match 1992 Field data that only counted live trees <7.5 cm DBH
M_trees_dc <- M_trees_dc[DBH >= 7.5 | Type == "Adult"]

# remove dead regen <1.3 m tall to match other years
M_trees_dc <- M_trees_dc[DBH > 0 | Dead.Code == "Alive"] #other year regen maybe not working

#3. Summarize by live trees
#do you want all live trees - or just over a certain DBH?
MSL_trees_dc <- M_trees_dc[Type != "Snag" & !is.na(Mg_treeC)]

MSL_trees_dc_a <- MSL_trees_dc[,.(MgUnit = sum(na.omit(Mg_treeC)), 
                                  BaUnit = sum(na.omit(BA)),
                                  QMD = sqrt(mean(na.omit(DBH)^2))), 
                               by = .(Unit, Year, SubPlot)] #sum each subplot and get QMD for each subplot

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

#4. Summarize Dead trees -----
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



#Summarize by live & species -----------------------
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

#output -
saveRDS(MSL_trees_dc, file.path(out_path,"MSL_trees_dc.RDS"))
saveRDS(MSD_trees_dc, file.path(out_path,"MSD_trees_dc.RDS"))
saveRDS(MSL_trees_dc_sp, file.path(out_path,"MSL_trees_dc_sp.RDS"))




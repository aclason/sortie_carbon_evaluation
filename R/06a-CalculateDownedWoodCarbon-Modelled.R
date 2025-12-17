library(data.table)
library(SummitLakeData)
library(DateCreekData)
library(treeCalcs)

in_path <- file.path("03_out_sortie")
out_path <- file.path("04_out_carbon")

# harvest bounds for ICH
Units_path <- "../DateCreekData_NotFunctionsYet/data-raw/Harvests_Plants/UnitBoundaries/"
Gaps_path <- "../DateCreekData_NotFunctionsYet/data-raw/Harvests_Plants/GapCutsDateCreek/"


#SBS--------------------------------------------------------------------------------------------
My_newvalsPath <- file.path("02_init_sortie","02_summit_lake","ParameterValues")
sum_in_path <- file.path(in_path, "02_summit_lake","extracted")

run_name <- "ds-nci_si_6"
gridFiles <- grep(run_name, list.files(sum_in_path, pattern = "grids.csv", 
                                      full.names = TRUE), value = TRUE)
plots <- stringr::str_split(list.files(My_newvalsPath, pattern = "summit"),".csv",
                            simplify = TRUE)[,1]

#read in the grids:
grid_list <- lapply(gridFiles, fread)
grid_dt <- rbindlist(grid_list)

#clip to centre 1 ha:
#grid_dt <- grid_dt[X >50 & X <150 & Y >50 & Y <150]

#don't need to clip to unit boundary (whole plot in "unit")
grid_to_output <- grep("vlog",unique(grid_dt$colnames), value = TRUE)
vlogs <- grid_dt[colnames %in% grid_to_output]
vlogs[, unit := tstrsplit(Unit, "summit_", fixed=TRUE, keep = 2)]
vlogs[,Unit:=NULL]
vlogs[,timestep := as.numeric(as.character(timestep))]

#add grouping columns to log volumes (already reported in vol/ha)
tr <- SummitLakeData::Treatments[, unit := as.character(unit)]
vlogs_tr <- merge(vlogs, tr, by = "unit")
vlogs_tr <- merge(vlogs_tr, treeCalcs::vollog_cat, by.x = "colnames", by.y = "grid")          

spGroups <- data.table(Sp =c("Sx","Pl","Bl","At","Lw","Fd","Ac","Ep"),
                       SpGrp = c(1,1,1,3,1,1,3,3))
#just keep the rows of the species that are relevant
cwdc_sm <- treeCalcs::sortie_cwd_carbon(volGrid = vlogs_tr, spGroups = spGroups)

#By pixel id - sum all the decay class volumes and carbon together
M_cwd_sl <- cwdc_sm[,.(pixMgHa = sum(MgHa), pixVolHa = sum(values)),
                    by = c("treatment","unit","timestep","point_id")]
#Add then take the average value across the whole unit
MS_cwd_sl <- M_cwd_sl[,.(MgHa = mean(pixMgHa), VolHa = mean(pixVolHa)),
                      by = c("treatment","unit","timestep")]
MS_cwd_sl[, Year := ifelse(unit == 4, 1994 + timestep,
                            ifelse(unit == 15, 1994 + timestep,
                                   1992 + timestep))]

#by decay class
MS_cwd_sl_dc <- cwdc_sm[,.(pixMgHa = sum(MgHa), pixVolHa = sum(values)),
                        by = c("treatment","unit","timestep","DecayClass","point_id")]
#Add then take the average value across the whole unit
MS_cwd_sl_dc <- MS_cwd_sl_dc[,.(MgHa = mean(pixMgHa), VolHa = mean(pixVolHa)),
                             by = c("treatment","unit","timestep","DecayClass")]
MS_cwd_sl_dc[, Year := ifelse(unit == 4, 1994 + timestep,
                              ifelse(unit == 15, 1994 + timestep,
                                     1992 + timestep))]


#by decay class and species groups
MS_cwd_sl_dc <- cwdc_sm[,.(pixMgHa = sum(MgHa), pixVolHa = sum(values)),
                        by = c("treatment","unit","timestep","DecayClass","SpGrp","point_id")]
#Add then take the average value across the whole unit
MS_cwd_sl_dc <- MS_cwd_sl_dc[,.(MgHa = mean(pixMgHa), VolHa = mean(pixVolHa)),
                             by = c("treatment","unit","timestep","DecayClass","SpGrp")]
MS_cwd_sl_dc[, Year := ifelse(unit == 4, 1994 + timestep,
                              ifelse(unit == 15, 1994 + timestep,
                                     1992 + timestep))]

# Outputs
saveRDS(MS_cwd_sl, file.path(out_path,"MS_cwd_sl.RDS"))
saveRDS(MS_cwd_sl_dc, file.path(out_path,"MS_cwd_sl_dc.RDS"))
saveRDS(MS_cwd_sl_dc, file.path(out_path,"MS_cwd_sl_dc_sg.RDS"))


#ICH-------------------------------------------------------------------------------------------

#read in the grids:
outfiles <- list.files(file.path(in_path,"01_date_creek","extracted"),
                       pattern = ".csv", full.names = TRUE)
grid_files <- grep("grids",outfiles, value = TRUE)
grid_dt <- rbindlist(lapply(grid_files, fread))#x & y still gets updated in the maskgrids function!

#clip by unit boundaries

grid_to_output <- grep("vlog",unique(grid_dt$colnames), value = TRUE)

vlogs <- DateCreekData::maskGrids(Blocks = DateCreekData::Treatments$Unit,
                   Units_path = Units_path, Gaps_path = Gaps_path,
                   NoCells_ToSample = NA, include_xy = TRUE,
                   grid_dat = grid_dt, output = "table",
                   grid_to_output = grid_to_output)
vlogs[,timestep := as.numeric(as.character(timestep))]
#add grouping columns to log volumes
vlogs_tr <- merge(vlogs, DateCreekData::Treatments, by = "Unit")
vlogs_tr <- merge(vlogs_tr,sortieCarbon::vollog_cat, by = "grid")          
vlogs_tr[,values:=value]


#total volume:
#grid_to_output <- "totallogvol"
#totlogs <- maskGrids(Blocks = DateCreekData::Treatments$Unit,
 #                  Units_path = Units_path, Gaps_path = Gaps_path,
  #                 NoCells_ToSample = NA, include_xy = FALSE,
   #                grid_dat = grid_dt, output = "table",
    #               grid_to_output = grid_to_output)
#totlogs[,timestep := as.numeric(as.character(timestep))]
#add grouping columns to log volumes
#totlogs_tr <- merge(totlogs, DateCreekData::Treatments, by = "Unit")
#totlogs_tr[,values:=value]

spGroups <- data.table(Sp =c("Hw","Cw","Ba", "Bl","Sx","Pl","At","Ac","Ep"),
                       SpGrp = c(1,2,1,1,1,1,3,3,3))

cwdc_dc <- sortieCarbon::cwdCfromSORTIE(volGrid = vlogs_tr, spGroups = spGroups)

#By pixel id - summ all the decay class volumes and carbon together
M_cwd_dc <- cwdc_dc[,.(pixMgHa = sum(MgHa), pixVolHa = sum(values)),
                    by = c("Treatment","Unit","timestep","ID")]
#Add then take the average value across the whole unit
MS_cwd_dc <- M_cwd_dc[,.(MgHa = mean(pixMgHa), VolHa = mean(pixVolHa)),
                      by = c("Treatment","Unit","timestep")]
MS_cwd_dc[, Year := timestep + 1992]


#by decay class:
MS_cwd_dc_sp_d <- cwdc_dc[,.(pixMgHa = sum(MgHa), pixVolHa = sum(values)),
                        by = c("Treatment","Unit","timestep","DecayClass",
                               "SpGrp","ID")]
#Add then take the average value across the whole unit
MS_cwd_dc_sp_d <- MS_cwd_dc_sp_d[,.(MgHa = mean(pixMgHa), VolHa = mean(pixVolHa)),
                             by = c("Treatment","Unit","timestep","DecayClass", "SpGrp")]
MS_cwd_dc_sp_d[, Year := timestep + 1992][, timestep := NULL]

#Outputs

saveRDS(MS_cwd_dc, file.path(out_path,"MS_cwd_dc.RDS"))
saveRDS(MS_cwd_dc_sp_d, file.path(out_path,"MS_cwd_dc_dc_sg.RDS"))




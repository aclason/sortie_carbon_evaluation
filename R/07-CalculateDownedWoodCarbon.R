library(sortieCarbon)
library(treeCalcs)
library(data.table)
library(DateCreekData)

######################### CALCULATE DOWNED WOOD CARBON ################################
#SBS---------------------------------------------------------------------------
out_path <- "./Inputs/SORTIEruns/SummitLake/Outputs/extracted/" 

#read in the grids:
gridFiles <- list.files(out_path, pattern = "grid", full.names = TRUE)
grid_list <- lapply(gridFiles, fread)
grid_dt <- rbindlist(grid_list)

#don't need to clip to unit boundary (whole plot in "unit")
grid_to_output <- grep("vlog",unique(grid_dt$colnames), value = TRUE)
vlogs <- grid_dt[colnames %in% grid_to_output]
vlogs[, unit := tstrsplit(Unit, "summit", fixed=TRUE, keep = 2)]
vlogs[,Unit:=NULL]

#add grouping columns to log volumes (already reported in vol/ha)
tr <- SummitLakeData::Treatments[, unit := as.character(unit)]
vlogs_tr <- merge(vlogs, tr, by = "unit")
vlogs_tr <- merge(vlogs_tr, sortieCarbon::vollog_cat, by.x = "colnames", by.y = "grid")          

#vm <- vlogs_tr[, mean(values), by=.(treatment,unit,timestep,group,size,decay)]

spGroups <- data.table(Sp =c("Hw","Cw","Ba", "Bl","Sx","Pl","At","Ac","Ep"),
                       SpGrp = c(1,3,1,1,1,1,2,2,2))
#just keep the rows of the species that are relevant

cwdc_sm <- cwdCfromSORTIE(volGrid = vlogs_tr, spGroups = spGroups)

MS_cwd_sm <- cwdc_sm[,.(MgHa = mean(MgHa),
                        VolHa = mean(values)), by = c("treatment","unit","timestep","DecayClass")]

ggplot(MS_cwd_sm)+
  geom_line(aes(x=timestep, y = VolHa, group=unit, colour = treatment))+
  facet_wrap(~as.factor(DecayClass))




#ICH------------------------------------------------------------------------------
out_path <- "../SORTIEParams/Outputs/ICH/JuvGrowth/extracted/" 

#read in the grids:
gridFiles <- list.files(out_path, pattern = "grid", full.names = TRUE)
grid_list <- lapply(gridFiles, fread)
grid_dt <- rbindlist(grid_list)

#clip by unit boundaries

Units_path <- "../DateCreekData/data-raw/Harvests_Plants/UnitBoundaries/"
Gaps_path <- "..//DateCreekData/data-raw/Harvests_Plants/GapCutsDateCreek/"

grid_to_output <- grep("vlog",unique(grid_dt$colnames), value = TRUE)

vlogs <- maskGrids(Blocks = DateCreekData::Treatments$Unit,
                   Units_path = Units_path, Gaps_path = Gaps_path,
                   NoCells_ToSample = NA,
                   grid_dat = grid_dt, output = "table",
                   grid_to_output = grid_to_output)


#add grouping columns to log volumes
vlogs_tr <- merge(vlogs, DateCreekData::Treatments, by = "Unit")
vlogs_tr <- merge(vlogs_tr,sortieCarbon::vollog_cat, by = "grid")          
vlogs_tr[,values:=value]

spGroups <- data.table(Sp =c("Hw","Cw","Ba", "Bl","Sx","Pl","At","Ac","Ep"),
                       SpGrp = c(1,3,1,1,1,1,2,2,2))

cwdc_dc <- cwdCfromSORTIE(volGrid = vlogs_tr, spGroups = spGroups)

MS_cwd_dc <- cwdc_dc[,.(MgHa = mean(MgHa), VolHa = mean(values)),
                       by = c("Treatment","Unit","timestep","DecayClass")]

ggplot(MS_cwd_dc)+
  geom_line(aes(x=timestep, y = VolHa, group = Unit, colour = Treatment))+
  facet_wrap(~as.factor(DecayClass))
#------------------------------------------------------------------------------------
#initial graphing mess around

FS_cwd_dc <- F_cwd_dc[,.(MgHa = mean(MgHa), VolHa = mean(MgHa)),
                      by = c("Treatment","Unit","Yrs_Post")]
ggplot(FS_cwd_dc)+
  geom_line(aes(x=Yrs_Post, y = VolHa, group = Unit, colour = Treatment))

FS_cwd_dc[Yrs_Post==0]











######################### CALCULATE DOWNED WOOD CARBON ################################
#SBS---------------------------------------------------------------------------

# Model-----------------------------------------------------------
out_path <- "./Inputs/SORTIEruns/SummitLake/Outputs/extracted/" 

#read in the grids:
gridFiles <- grep("ds_part",list.files(out_path, pattern = "grid",
                                       full.names = TRUE), value = TRUE)
grid_list <- lapply(gridFiles, fread)
grid_dt <- rbindlist(grid_list)

#don't need to clip to unit boundary (whole plot in "unit")
grid_to_output <- grep("vlog",unique(grid_dt$colnames), value = TRUE)
vlogs <- grid_dt[colnames %in% grid_to_output]
vlogs[, unit := tstrsplit(Unit, "summit", fixed=TRUE, keep = 2)]
vlogs[,Unit:=NULL]
vlogs[,timestep := as.numeric(as.character(timestep))]

#add grouping columns to log volumes (already reported in vol/ha)
tr <- SummitLakeData::Treatments[, unit := as.character(unit)]
vlogs_tr <- merge(vlogs, tr, by = "unit")
vlogs_tr <- merge(vlogs_tr, sortieCarbon::vollog_cat, by.x = "colnames", by.y = "grid")          


#vm <- vlogs_tr[, mean(values), by=.(treatment,unit,timestep,group,size,decay)]

spGroups <- data.table(Sp =c("Sx","Pl","Bl","At","Lw","Fd","Ac","Ep"),
                       SpGrp = c(1,1,1,3,1,1,3,3))
#just keep the rows of the species that are relevant

cwdc_sm <- cwdCfromSORTIE(volGrid = vlogs_tr, spGroups = spGroups)

#By pixel id - summ all the decay class volumes and carbon together
M_cwd_sl <- cwdc_sm[,.(pixMgHa = sum(MgHa), pixVolHa = sum(values)),
                    by = c("treatment","unit","timestep","point_id")]
#Add then take the average value across the whole unit
MS_cwd_sl <- M_cwd_sl[,.(MgHa = mean(pixMgHa), VolHa = mean(pixVolHa)),
                      by = c("treatment","unit","timestep")]

# Field----------------------------------------------------------------
F_cwd_sl <- merge(F_cwd_sl, treeCalcs::cwdC_conv_table[is.na(BECzone)| BECzone=="SBS",
                                                       .(BCname,DecayClass,
                                                          AbsoluteDensity,
                                                          StructuralReductionFactor,
                                                          CarbonConversionFactor)]
                  , by.x=c("Species","DecayClass"), by.y = c("BCname","DecayClass"),
                  all.x = TRUE)
setnames(F_cwd_sl, c("AbsoluteDensity","StructuralReductionFactor","CarbonConversionFactor"),
         c("AbsDens","StrRedFac","CarbConvFac"))
F_cwd_sl[is.na(AbsDens)]

#calculate carbon:---------
F_cwd_sl[, MgHa:= VolHa * AbsDens * StrRedFac * CarbConvFac, 
         by = seq_len(nrow(F_cwd_sl))]

#get treatment, unit and year
F_cwd_sl[, unit:= tstrsplit(Plot, "-", fixed=TRUE, keep = 2)]
F_cwd_sl[,`:=`(unit = as.numeric(unit),timestep = (Year - 1992))]
SummitLakeData::Treatments[, unit := as.numeric(unit)] #stop fliflopping - fix above
F_cwd_sl <- merge(F_cwd_sl[,.(unit,timestep,DecayClass,Species,VolHa,MgHa)], 
                  SummitLakeData::Treatments, by = "unit")

#when including species decay, need to get the plot totals, then means then sum of means
# sum all the piece volumes and carbon by plot - doesn't apply ot Summit Lake as there's 
# only one plot/unit
plotTots <- F_cwd_sl[, .(plotVol = sum(VolHa), plotMg = sum(MgHa)),
                     by = .(treatment, unit, timestep, Species, DecayClass)]

# take the mean vol and mg by species decay class and year across plots for each unit
# still the same as no repeats
FS_cwd_sl_sp_d <- plotTots[, .(VolHa = mean(plotVol), MgHa = mean(plotMg)),
                           by = .(treatment, unit, timestep, Species, DecayClass)]

#if you want the overall volume of MgHa, then sum all the average values together by unit
FS_cwd_sl <- FS_cwd_sl_sp_d[, .(VolHa = sum(VolHa), MgHa = sum(MgHa)),
                            by = .(treatment, unit, timestep)]

#calc by groups (same as modelled) OPTIONAL ----------------------------
F_cwd_sl_s <- merge(F_cwd_sl, spGroups, by.x = "Species", by.y = "Sp", 
                    all.x = TRUE)
F_cwd_sl_s[is.na(SpGrp), SpGrp := 1]

decay_grp_dens <- F_cwd_sl_s[ ,.(mnAbsDens = mean(AbsDens),
                                mnCarbConc = mean(CarbConvFac)),
                             by=c("DecayClass", "SpGrp")]

F_cwd_sl_s[,`:=`(DecFac = ifelse(DecayClass==1,1,
                            ifelse(DecayClass==2,1,
                              ifelse(DecayClass==3,0.8,
                                 ifelse(DecayClass==4,0.8,
                                    ifelse(DecayClass==5,0.412,NA))))))]
F_cwd_sl_s <- merge(decay_grp_dens, F_cwd_sl_s, by.x = c("DecayClass","SpGrp"), 
                 by.y = c("DecayClass","SpGrp"))

F_cwd_sl_s[, MgHa := VolHa*DecFac*mnAbsDens*mnCarbConc, by=seq_len(nrow(volGrid))]
#summarize by treatment, unit and year ------------------------
F_cwd_sl_s[, unit:= tstrsplit(Plot, "-", fixed=TRUE, keep = 2)]
F_cwd_sl_s[,`:=`(unit = as.numeric(unit),timestep = (Year - 1992))]
SummitLakeData::Treatments[, unit := as.numeric(unit)] #stop fliflopping - fix above
F_cwd_sl_s <- merge(F_cwd_sl_s[,.(unit,timestep,DecayClass,VolHa,MgHa)], 
                  SummitLakeData::Treatments, by = "unit")




#ICH-------------------------------------------------------------------------------------------

# Model-----------------------------------------------------------
out_path <- "../SORTIEParams/Outputs/ICH/CompMort/extracted/" 

#read in the grids:
gridFiles <- list.files(out_path, pattern = "grid", full.names = TRUE)
grid_list <- lapply(gridFiles, fread)
grid_dt <- rbindlist(grid_list)

#clip by unit boundaries

Units_path <- "../DateCreekData_NotFunctionsYet/data-raw/Harvests_Plants/UnitBoundaries/"
Gaps_path <- "../DateCreekData_NotFunctionsYet/data-raw/Harvests_Plants/GapCutsDateCreek/"

grid_to_output <- grep("vlog",unique(grid_dt$colnames), value = TRUE)

vlogs <- maskGrids(Blocks = DateCreekData::Treatments$Unit,
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
grid_to_output <- "totallogvol"
totlogs <- maskGrids(Blocks = DateCreekData::Treatments$Unit,
                   Units_path = Units_path, Gaps_path = Gaps_path,
                   NoCells_ToSample = NA, include_xy = FALSE,
                   grid_dat = grid_dt, output = "table",
                   grid_to_output = grid_to_output)
totlogs[,timestep := as.numeric(as.character(timestep))]
#add grouping columns to log volumes
totlogs_tr <- merge(totlogs, DateCreekData::Treatments, by = "Unit")
totlogs_tr[,values:=value]




spGroups <- data.table(Sp =c("Hw","Cw","Ba", "Bl","Sx","Pl","At","Ac","Ep"),
                       SpGrp = c(1,2,1,1,1,1,3,3,3))

cwdc_dc <- sortieCarbon::cwdCfromSORTIE(volGrid = vlogs_tr, spGroups = spGroups)

#By pixel id - summ all the decay class volumes and carbon together
M_cwd_dc <- cwdc_dc[,.(pixMgHa = sum(MgHa), pixVolHa = sum(values)),
                    by = c("Treatment","Unit","timestep","ID")]
#Add then take the average value across the whole unit
MS_cwd_dc <- M_cwd_dc[,.(MgHa = mean(pixMgHa), VolHa = mean(pixVolHa)),
                      by = c("Treatment","Unit","timestep")]


# Field----------------------------------------------------------------
# get wood density, structural reduction factor and carbon conversion factors for all measured CWD
F_cwd_dc <- merge(F_cwd_dc_all, treeCalcs::cwdC_conv_table[is.na(BECzone)| BECzone=="ICH",
                                                       .(BCname,DecayClass,
                                                          AbsoluteDensity,
                                                          StructuralReductionFactor,
                                                          CarbonConversionFactor)]
                  , by.x=c("Sp","Decay"), by.y = c("BCname","DecayClass"),
                  all.x = TRUE)
setnames(F_cwd_dc, c("AbsoluteDensity","StructuralReductionFactor","CarbonConversionFactor"),
         c("AbsDens","StrRedFac","CarbConvFac"))
F_cwd_dc[is.na(AbsDens)]

# so originally, I scripted this, but I updated the treeCalcs::cwdC_conv_table to estimate unknowns
# for each BEC zone - probably not the right way, but for now, it'll do.

#estimate density, and reduction and carbon factors for unknown species and round decay rates------------

#need to assign average density etc. to U species (no UC or UD in this dataset. 
#These are the species Ingrid suggested be include in the averaging)
#avg_conv <- treeCalcs::cwdC_conv_table[BCname %in% c("Bl", "Sx", "Pl","Ba","Hw", "Cw","At", "Ac", "Ep"),
 #                                      .(AbsDens = mean(AbsoluteDensity),
  #                                       CarbConvFac = mean(CarbonConversionFactor),
   #                                      StrRedFac = mean(StructuralReductionFactor)),
    #                                   by=c("DecayClass")]
#split out the NAs for unknown species and the rest
#F_cwd_dc_a <- F_cwd_dc[!is.na(AbsDens)]
#F_cwd_dc_u <- F_cwd_dc[is.na(AbsDens) & Sp =="U",.(Treatment,Unique_plot,
 #                                                  Sp, Decay,Year, 
  #                                                 Yrs_Post, Unit, VolumeHa)]
#F_cwd_dc_d <- F_cwd_dc[is.na(AbsDens) & Sp !="U",.(Treatment, Unique_plot,
 #                                                  Sp, Decay,Year, 
  #                                                 Yrs_Post, Unit, VolumeHa)]

#grab average values for unknown species
#F_cwd_dc_u[, Decay := ceiling(Decay)] #round up
#F_cwd_dc_u <- merge(F_cwd_dc_u,avg_conv, by.x = "Decay", by.y = "DecayClass", all.x = TRUE)

#need to estimate decay rate - will try rounding up versus rounding down and see if it changes story
#all .5 decay rates
#F_cwd_dc_d <- rbind(F_cwd_dc_d, F_cwd_dc_u[is.na(AbsDens),.(Treatment, Unique_plot,
 #                                                           Sp,Decay,Year,
  #                                                          Yrs_Post,Unit,VolumeHa)])
#F_cwd_dc_d[, Decay := ceiling(Decay)]

#merge with tree calcs
#known species
#F_cwd_dc_d1 <- merge(F_cwd_dc_d[Sp != "U"], treeCalcs::cwdC_conv_table[,.(BCname,DecayClass,
 #                                                                         AbsoluteDensity,
  #                                                                        StructuralReductionFactor,
   #                                                                       CarbonConversionFactor)]
    #                 , by.x=c("Sp","Decay"), by.y = c("BCname","DecayClass"),
     #                all.x = TRUE)
#setnames(F_cwd_dc_d1, c("AbsoluteDensity","StructuralReductionFactor","CarbonConversionFactor"),
 #        c("AbsDens","StrRedFac","CarbConvFac"))

#unknown decay - use averages for Hw
#F_cwd_dc_d1[is.na(AbsDens),`:=`(AbsDens = treeCalcs::cwdC_conv_table[BCname =="Hw",mean(AbsoluteDensity)],
 #                               StrRedFac = 1,
  #                              CarbConvFac = treeCalcs::cwdC_conv_table[BCname =="Hw",mean(CarbonConversionFactor)])]

#unknown species
#F_cwd_dc_d2 <- merge(F_cwd_dc_d[Sp=="U"],avg_conv, by.x = "Decay", by.y = "DecayClass", all.x = TRUE)

#F_cwd_dc_d <- rbind(F_cwd_dc_d1, F_cwd_dc_d2)
#F_cwd_dc_u <- F_cwd_dc_u[!is.na(AbsDens)]

#F_cwd_dc <- rbind(F_cwd_dc_a,F_cwd_dc_u,F_cwd_dc_d)

#calculate carbon:--------------------------------------------------------------------------------
F_cwd_dc[, MgHa:= VolumeHa * AbsDens * StrRedFac * CarbConvFac, by = seq_len(nrow(F_cwd_dc))]

#summarize by treatment, unit and year ----------------------------------------------------------
F_cwd_dc <- merge(F_cwd_dc, DateCreekData::Treatments, by = "Unit", all.x = TRUE)

#update Yrs post:
F_cwd_dc[, Yrs_Post:= ifelse(Year == 1992,0,
                             ifelse(Year == 1993, 1,
                                    ifelse(Year == 2011, 19, 27)))]

#when including species decay, need to get the plot totals, then means then sum of means
# sum all the piece volumes and carbon by plot
plotTots <- F_cwd_dc[, .(plotVol = sum(VolumeHa), plotMg = sum(MgHa)),
                     by = .(Treatment, Unit, Year, Yrs_Post, Sp, Decay, Unique_plot)]

# take the mean vol and mg by species decay class and year across plots for each unit
FS_cwd_dc_sp_d <- plotTots[, .(VolHa = mean(plotVol), MgHa = mean(plotMg)),
                           by = .(Treatment, Unit, Year, Yrs_Post, Sp, Decay)]

#if you want the overall volume of MgHa, then sum all the average values together by unit
FS_cwd_dc <- FS_cwd_dc_sp_d[, .(VolHa = sum(VolHa), MgHa = sum(MgHa)),
                            by = .(Treatment, Unit, Year, Yrs_Post)]

#return(F_cwd_dc)







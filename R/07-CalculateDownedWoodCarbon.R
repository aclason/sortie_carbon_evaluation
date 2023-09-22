
######################### CALCULATE DOWNED WOOD CARBON ################################
#SBS---------------------------------------------------------------------------

# Model-----------------------------------------------------------
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

spGroups <- data.table(Sp =c("Sx","Pl","Bl","At","Lw","Fd","Ac","Ep"),
                       SpGrp = c(1,1,1,3,1,1,3,3))
#just keep the rows of the species that are relevant

cwdc_sm <- cwdCfromSORTIE(volGrid = vlogs_tr, spGroups = spGroups)

MS_cwd_sl <- cwdc_sm[,.(MgHa = mean(MgHa), VolHa = mean(values)),
                     by = c("treatment","unit","timestep","DecayClass")]


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

#calculate carbon:--------------------------------------------------------------------------------
F_cwd_sl[, MgHa:= VolHa * AbsDens * StrRedFac * CarbConvFac, 
         by = seq_len(nrow(F_cwd_sl))]
#summarize by treatment, unit and year ----------------------------------------------------------
F_cwd_sl[, unit:= tstrsplit(Plot, "-", fixed=TRUE, keep = 2)]
F_cwd_sl[,`:=`(unit = as.numeric(unit),timestep = (Year - 1992))]
SummitLakeData::Treatments[, unit := as.numeric(unit)] #stop fliflopping - fix above
F_cwd_sl <- merge(F_cwd_sl[,.(unit,timestep,DecayClass,VolHa,MgHa)], 
                  SummitLakeData::Treatments, by = "unit")

#calc by groups (same as modelled)
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
#summarize by treatment, unit and year ----------------------------------------------------------
F_cwd_sl_s[, unit:= tstrsplit(Plot, "-", fixed=TRUE, keep = 2)]
F_cwd_sl_s[,`:=`(unit = as.numeric(unit),timestep = (Year - 1992))]
SummitLakeData::Treatments[, unit := as.numeric(unit)] #stop fliflopping - fix above
F_cwd_sl_s <- merge(F_cwd_sl_s[,.(unit,timestep,DecayClass,VolHa,MgHa)], 
                  SummitLakeData::Treatments, by = "unit")




#ICH-------------------------------------------------------------------------------------------

# Model-----------------------------------------------------------
out_path <- "../SORTIEParams/Outputs/ICH/Snags/extracted/" 

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
                   NoCells_ToSample = NA,
                   grid_dat = grid_dt, output = "table",
                   grid_to_output = grid_to_output)
vlogs[,timestep := as.numeric(as.character(timestep))]

#add grouping columns to log volumes
vlogs_tr <- merge(vlogs, DateCreekData::Treatments, by = "Unit")
vlogs_tr <- merge(vlogs_tr,sortieCarbon::vollog_cat, by = "grid")          
vlogs_tr[,values:=value]

spGroups <- data.table(Sp =c("Hw","Cw","Ba", "Bl","Sx","Pl","At","Ac","Ep"),
                       SpGrp = c(1,2,1,1,1,1,3,3,3))

cwdc_dc <- cwdCfromSORTIE(volGrid = vlogs_tr, spGroups = spGroups)

MS_cwd_dc <- cwdc_dc[,.(MgHa = mean(MgHa), VolHa = mean(values)),
                       by = c("Treatment","Unit","timestep","DecayClass")]

#MS_cwd_dc[,timestep:=as.numeric(timestep)]
#return(cwdc_dc)


# Field----------------------------------------------------------------
# get wood density, structural reduction factor and carbon conversion factors for all measured CWD
F_cwd_dc <- merge(F_cwd_dc, treeCalcs::cwdC_conv_table[is.na(BECzone)| BECzone=="ICH",
                                                       .(BCname,DecayClass,
                                                          AbsoluteDensity,
                                                          StructuralReductionFactor,
                                                          CarbonConversionFactor)]
                  , by.x=c("Sp","Decay"), by.y = c("BCname","DecayClass"),
                  all.x = TRUE)
setnames(F_cwd_dc, c("AbsoluteDensity","StructuralReductionFactor","CarbonConversionFactor"),
         c("AbsDens","StrRedFac","CarbConvFac"))
F_cwd_dc[is.na(AbsDens)]

#estimate density, and reduction and carbon factors for unknown species and round decay rates------------

#need to assign average density etc. to U species (no UC or UD in this dataset. 
#These are the species Ingrid suggested be include in the averaging)
avg_conv <- treeCalcs::cwdC_conv_table[BCname %in% c("Bl", "Sx", "Pl","Ba","Hw", "Cw","At", "Ac", "Ep"),
                                       .(AbsDens = mean(AbsoluteDensity),
                                         CarbConvFac = mean(CarbonConversionFactor),
                                         StrRedFac = mean(StructuralReductionFactor)),
                                       by=c("DecayClass")]
#split out the NAs for unknown species and the rest
F_cwd_dc_a <- F_cwd_dc[!is.na(AbsDens)]
F_cwd_dc_u <- F_cwd_dc[is.na(AbsDens) & Sp =="U",.(Sp, Decay,Year, Yrs_Post, Unit, VolumeHa)]
F_cwd_dc_d <- F_cwd_dc[is.na(AbsDens) & Sp !="U",.(Sp, Decay,Year, Yrs_Post, Unit, VolumeHa)]

#grab average values for unknown species
F_cwd_dc_u <- merge(F_cwd_dc_u,avg_conv, by.x = "Decay", by.y = "DecayClass", all.x = TRUE)

#need to estimate decay rate - will try rounding up versus rounding down and see if it changes story
#all .5 decay rates
F_cwd_dc_d <- rbind(F_cwd_dc_d, F_cwd_dc_u[is.na(AbsDens),.(Sp,Decay,Year,Yrs_Post,Unit,VolumeHa)])
F_cwd_dc_d[, Decay := ceiling(Decay)]

#merge with tree calcs
#known species
F_cwd_dc_d1 <- merge(F_cwd_dc_d[Sp != "U"], treeCalcs::cwdC_conv_table[,.(BCname,DecayClass,
                                                                          AbsoluteDensity,
                                                                          StructuralReductionFactor,
                                                                          CarbonConversionFactor)]
                     , by.x=c("Sp","Decay"), by.y = c("BCname","DecayClass"),
                     all.x = TRUE)
setnames(F_cwd_dc_d1, c("AbsoluteDensity","StructuralReductionFactor","CarbonConversionFactor"),
         c("AbsDens","StrRedFac","CarbConvFac"))

#unknown decay - use averages for Hw
F_cwd_dc_d1[is.na(AbsDens),`:=`(AbsDens = treeCalcs::cwdC_conv_table[BCname =="Hw",mean(AbsoluteDensity)],
                                StrRedFac = 1,
                                CarbConvFac = treeCalcs::cwdC_conv_table[BCname =="Hw",mean(CarbonConversionFactor)])]

#unknown species
F_cwd_dc_d2 <- merge(F_cwd_dc_d[Sp=="U"],avg_conv, by.x = "Decay", by.y = "DecayClass", all.x = TRUE)

F_cwd_dc_d <- rbind(F_cwd_dc_d1, F_cwd_dc_d2)
F_cwd_dc_u <- F_cwd_dc_u[!is.na(AbsDens)]

F_cwd_dc <- rbind(F_cwd_dc_a,F_cwd_dc_u,F_cwd_dc_d)

#calculate carbon:--------------------------------------------------------------------------------
F_cwd_dc[, MgHa:= VolumeHa * AbsDens * StrRedFac * CarbConvFac, by = seq_len(nrow(F_cwd_dc))]

#summarize by treatment, unit and year ----------------------------------------------------------
F_cwd_dc <- merge(F_cwd_dc, DateCreekData::Treatments, by = "Unit", all.x = TRUE)


#return(F_cwd_dc)







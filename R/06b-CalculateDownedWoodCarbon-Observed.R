library(data.table)
library(SummitLakeData)
library(DateCreekData)
library(treeCalcs)

in_path <- file.path("03_out_sortie")
out_path <- file.path("04_out_carbon")

#data
cwd_sl <- "D:/GitHub/SummitLakeData/data-raw/EP1162CWDsurvey2020-2021.csv"
cwd_hor_sl <- "D:/GitHub/SummitLakeData/data-raw/EP1162CWDsurveyTransectLines2020-2021.csv"
cwd_dc <- "D:/GitHub/DateCreekData/data-raw/CWD/"

#SBS--------------------------------------------------------------------------------------------
#calculate volumes from data 
F_cwd_sl <- SummitLakeData::CWD_2021_Vol_calc(CWD_dat = cwd_sl,
                                              Horiz_dat = cwd_hor_sl,
                                              out_carbon_comp = TRUE)

F_cwd_sl <- merge(F_cwd_sl, treeCalcs::cwdC_conv_table[is.na(BECzone)| BECzone=="SBS",
                                                       .(BCname,DecayClass,
                                                         AbsoluteDensity,
                                                         StructuralReductionFactor,
                                                         CarbonConversionFactor)],
                  by.x=c("Sp","Decay"), 
                  by.y = c("BCname","DecayClass"),
                  all.x = TRUE)
setnames(F_cwd_sl, 
         c("AbsoluteDensity","StructuralReductionFactor","CarbonConversionFactor"),
         c("AbsDens","StrRedFac","CarbConvFac"))
F_cwd_sl[is.na(AbsDens)]

#calculate carbon:---------
F_cwd_sl[, MgHa:= VolumeHa * AbsDens * StrRedFac * CarbConvFac, 
         by = seq_len(nrow(F_cwd_sl))]

#get treatment, unit and year
F_cwd_sl <- merge(F_cwd_sl[,.(Unit,Year,Decay,Sp,VolumeHa,MgHa)], 
                  SummitLakeData::Treatments, by.x = "Unit", by.y = "unit")
#when including species decay, need to get the plot totals, then means then sum of means
# sum all the piece volumes and carbon by plot - doesn't apply ot Summit Lake as there's 
# only one plot/unit
plotTots <- F_cwd_sl[, .(plotVol = sum(VolumeHa), plotMg = sum(MgHa)),
                     by = .(treatment, Unit, Year, Sp, Decay)]
# take the mean vol and mg by species decay class and year across plots for each unit
# still the same as no repeats
FS_cwd_sl_sp_d <- plotTots[, .(VolHa = mean(plotVol), MgHa = mean(plotMg)),
                           by = .(treatment, Unit, Year, Sp, Decay)]

#if you want the overall volume of MgHa, then sum all the average values together by unit
FS_cwd_sl <- FS_cwd_sl_sp_d[, .(VolHa = sum(VolHa), MgHa = sum(MgHa)),
                            by = .(treatment, Unit, Year)]

#calc by groups (same as modelled) OPTIONAL ----------------------------
F_cwd_sl <- merge(F_cwd_sl, treeCalcs::cwdC_conv_table[is.na(BECzone)| BECzone=="SBS",
                                                       .(BCname,DecayClass,
                                                         AbsoluteDensity,
                                                         StructuralReductionFactor,
                                                         CarbonConversionFactor)]
                  , by.x=c("Sp","Decay"), by.y = c("BCname","DecayClass"),
                  all.x = TRUE)
setnames(F_cwd_sl, c("AbsoluteDensity","StructuralReductionFactor","CarbonConversionFactor"),
         c("AbsDens","StrRedFac","CarbConvFac"))
F_cwd_sl[is.na(AbsDens)]

#calculate carbon:---------
F_cwd_sl[, MgHa:= VolumeHa * AbsDens * StrRedFac * CarbConvFac, 
         by = seq_len(nrow(F_cwd_sl))]

#get treatment, unit and year
spGroups <- data.table(Sp =c("Sx","Pl","Bl","At","Lw","Fd","Ac","Ep"),
                       SpGrp = c(1,1,1,3,1,1,3,3))
F_cwd_sl_s <- merge(F_cwd_sl, spGroups, by.x = "Sp", by.y = "Sp", 
                    all.x = TRUE)
F_cwd_sl_s[is.na(SpGrp), SpGrp := 1]

decay_grp_dens <- F_cwd_sl_s[ ,.(mnAbsDens = mean(AbsDens),
                                 mnCarbConc = mean(CarbConvFac)),
                              by=c("Decay", "SpGrp")]

F_cwd_sl_s[,`:=`(DecFac = ifelse(Decay==1,1,
                                 ifelse(Decay==2,1,
                                        ifelse(Decay==3,0.8,
                                               ifelse(Decay==4,0.8,
                                                      ifelse(Decay==5,0.412,NA))))))]
F_cwd_sl_s <- merge(decay_grp_dens, F_cwd_sl_s, by.x = c("Decay","SpGrp"), 
                    by.y = c("Decay","SpGrp"))

F_cwd_sl_s[, MgHa := VolumeHa*DecFac*mnAbsDens*mnCarbConc, by=seq_len(nrow(F_cwd_sl_s))]
F_cwd_sl_s <- merge(F_cwd_sl_s[,.(Unit,Year,SpGrp,Decay,VolumeHa,MgHa)], 
                    SummitLakeData::Treatments, 
                    by.x = "Unit",
                    by.y = "unit")


# Outputs
saveRDS(FS_cwd_sl, file.path(out_path,"FS_cwd_sl.RDS"))
saveRDS(F_cwd_sl_s, file.path(out_path,"FS_cwd_sl_dc_sg.RDS")) #by species groups

#ICH---------------------------------------------------------------------------
#calculate volumes from data 

F_cwd_dc <- DateCreekData::CWD_vol_calc(dat_loc = cwd_dc, 
                                        incl_sp_decay = TRUE)
#should add species cleanup to the volume function
F_cwd_dc[, Sp := ifelse(Sp=="u","U",
                        ifelse(Sp == "", "U",
                               ifelse(Sp == "ep", "Ep",
                                      ifelse(Sp == "Act","Ac",Sp))))]

#Round decay estimates - for now rounding up
F_cwd_dc[, Decay := ceiling(Decay)]

#need to get all the zeros for sp-decay combos and merge with species groups for each unique plot:
spTabl <- data.table(Sp = rep(unique(F_cwd_dc[Year == 1992,]$Sp),5),
                     Decay = c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10)))

spGroups <- data.table(Sp =c("Hw","Cw","Ba","U","Bl","Sx","Pl","At","Ac","Ep"),
                       SpGrp = c(1,2,1,1,1,1,1,3,3,3))

spGrTabl <- merge(spTabl, spGroups, by = "Sp")

#every year doesn't have the same number of plots, so need to get the year - plots combos to make
#the master sampling list
plotUnit_UP <- unique(F_cwd_dc[,.(Unit,Year,Unique_plot)])

spGrTabl_up <- cbind(spGrTabl, sort(rep(unique(plotUnit_UP$Unique_plot),50)))
setnames(spGrTabl_up, "V2", "Unique_plot")

plotUnit_UP_sp <- merge(plotUnit_UP, spGrTabl_up, by = c("Unique_plot"), allow.cartesian = TRUE)

F_cwd_dc_all <- merge(F_cwd_dc, plotUnit_UP_sp, 
                      by = c("Unit", "Year","Unique_plot", "Sp","Decay"), all.y = TRUE)
#update NAs to 0s
F_cwd_dc_all[is.na(VolumeHa), VolumeHa := 0]


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
#add treatment
F_cwd_dc <- merge(F_cwd_dc, DateCreekData::Treatments, by = "Unit", all.x = TRUE)


#calculate carbon:--------------------------------------------------------------------------------
F_cwd_dc[, MgHa:= VolumeHa * AbsDens * StrRedFac * CarbConvFac, by = seq_len(nrow(F_cwd_dc))]

#summarize by treatment, unit and year ----------------------------------------------------------
#when including species decay, need to get the plot totals, then means then sum of means
# sum all the piece volumes and carbon by plot
plotTots <- F_cwd_dc[, .(plotVol = sum(VolumeHa), 
                         plotMg = sum(MgHa)),
                     by = .(Treatment, Unit, Year, Sp, Decay, Unique_plot)]

# take the mean vol and mg by species decay class and year across plots for each unit
FS_cwd_dc_sp_d <- plotTots[, .(VolHa = mean(plotVol), 
                               MgHa = mean(plotMg)),
                           by = .(Treatment, Unit, Year, Sp, Decay)]

#if you want the overall volume of MgHa, then sum all the average values together by unit
FS_cwd_dc <- FS_cwd_dc_sp_d[, .(VolHa = sum(VolHa), 
                                MgHa = sum(MgHa)),
                            by = .(Treatment, Unit, Year)]


#calc by groups (same as modelled) OPTIONAL ------------------------------------------

#when including species decay and we want to summarize by decay class and species group,
#need to get the plot totals for each decay class - species group combination, 
#then means then sum of means and sum all the piece volumes and carbon by plot

decay_grp_dens <- F_cwd_dc[ ,.(mnAbsDens = mean(AbsDens),
                               mnCarbConc = mean(CarbConvFac)),
                            by=c("Decay", "SpGrp")]

F_cwd_dc[,`:=`(DecFac = ifelse(Decay==1,1,
                               ifelse(Decay==2,1,
                                      ifelse(Decay==3,0.8,
                                             ifelse(Decay==4,0.8,
                                                    ifelse(Decay==5,0.412,NA))))))]
F_cwd_dc <- merge(decay_grp_dens, F_cwd_dc, by.x = c("Decay","SpGrp"), 
                  by.y = c("Decay","SpGrp"))
F_cwd_dc[, MgHa := VolumeHa * DecFac * mnAbsDens * mnCarbConc, 
         by=seq_len(nrow(F_cwd_dc))]

#summarize by treatment, unit and year ------------------------
plotTots <- F_cwd_dc[, .(plotVol = sum(VolumeHa), plotMg = sum(MgHa)),
                     by = .(Treatment, Unit, Year, SpGrp, Decay, Unique_plot)]

# take the mean vol and mg by species decay class and year across plots for each unit
FS_cwd_dc_sp_d <- plotTots[, .(VolHa = mean(plotVol), MgHa = mean(plotMg)),
                           by = .(Treatment, Unit, Year, SpGrp, Decay)]

#if you want the overall volume of MgHa, then sum all the average values together by unit
setnames(FS_cwd_dc_sp_d, "Decay", "DecayClass")

saveRDS(FS_cwd_dc, file.path(out_path,"FS_cwd_dc.RDS")) #by decay class
saveRDS(FS_cwd_dc_sp_d, file.path(out_path,"FS_cwd_dc_dc_sg.RDS")) #by species groups



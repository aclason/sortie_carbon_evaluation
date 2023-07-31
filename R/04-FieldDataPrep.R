

#SBS---------------------------------------------------------------------------
##### LIVE STAMDING CARBON #########



##### DEAD STAMDING CARBON #########




##### DOWNED WOOD CARBON #########





#ICH---------------------------------------------------------------------------
##### LIVE STAMDING CARBON #########



##### DEAD STAMDING CARBON #########




##### DOWNED WOOD CARBON #########

#calculate volumes from data 
F_cwd_dc <- DateCreekData::CWD_vol_calc(dat_loc = "D:/Github/DateCreekData/data-raw/CWD/", 
                                        incl_sp_decay = TRUE)
#should add species cleanup to the volume function
F_cwd_dc[, Sp := ifelse(Sp=="u","U",
                  ifelse(Sp == "", "U",
                    ifelse(Sp == "ep", "Ep",
                      ifelse(Sp == "Act","Ac",Sp))))]

#calculate carbon ---------------------------------------------------------------------------------
# get wood density, structural reduction factor and carbon conversion factors for all measured CWD
F_cwd_dc <- merge(F_cwd_dc, treeCalcs::cwdC_conv_table[,.(BCname,DecayClass,
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

FS_cwd_dc <- F_cwd_dc[,.(MgHa = mean(MgHa), VolHa = mean(MgHa)),
                    by = c("Treatment","Unit","Yrs_Post")]
ggplot(FS_cwd_dc)+
  geom_line(aes(x=Yrs_Post, y = VolHa, group = Unit, colour = Treatment))

#--------------------------------------------------------------------------------------------------


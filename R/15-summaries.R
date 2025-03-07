



cwd_init <- DateCreekData::CWD_1992_props(dat_loc = "D:/Github/DateCreekData/data-raw/CWD/")
cwd_92 <- file.path("D:/Github/DateCreekData/data-raw/CWD/","CWD_1992.csv")
diams_92 <- DateCreekData::CWD_1992_diams(cwd_92) #I think these are identical functions - can make generic

cwd_post_harvest <- CWD_1993_props(dat_loc = "D:/Github/DateCreekData/data-raw/CWD/")
cwd_93 <- file.path("D:/Github/DateCreekData/data-raw/CWD/","CWD_1993.csv")
diams_93 <- DateCreekData::CWD_1993_diams(cwd_93)

parnames <- data.table(pars = c("SpGrp1LargeClass1InitLogProp","SpGrp1LargeClass2InitLogProp",
                                "SpGrp1LargeClass3InitLogProp","SpGrp1LargeClass4InitLogProp",
                                "SpGrp1LargeClass5InitLogProp","SpGrp2LargeClass1InitLogProp",
                                "SpGrp2LargeClass2InitLogProp","SpGrp2LargeClass3InitLogProp",
                                "SpGrp2LargeClass4InitLogProp","SpGrp2LargeClass5InitLogProp",
                                "SpGrp3LargeClass1InitLogProp","SpGrp3LargeClass2InitLogProp",
                                "SpGrp3LargeClass3InitLogProp","SpGrp3LargeClass4InitLogProp",
                                "SpGrp3LargeClass5InitLogProp","SpGrp1SmallClass1InitLogProp",
                                "SpGrp1SmallClass2InitLogProp","SpGrp1SmallClass3InitLogProp",
                                "SpGrp1SmallClass4InitLogProp","SpGrp1SmallClass5InitLogProp",
                                "SpGrp2SmallClass1InitLogProp","SpGrp2SmallClass2InitLogProp",
                                "SpGrp2SmallClass3InitLogProp","SpGrp2SmallClass4InitLogProp",
                                "SpGrp2SmallClass5InitLogProp","SpGrp3SmallClass1InitLogProp",
                                "SpGrp3SmallClass2InitLogProp","SpGrp3SmallClass3InitLogProp",
                                "SpGrp3SmallClass4InitLogProp","SpGrp3SmallClass5InitLogProp"))

grNames <- CJ(SpGrp = c(1,2,3),
              sizeGr = c("small","large"),
              Decay = c(1,2,3,4,5))
setkey(grNames, sizeGr)
par_gr_init <- cbind(parnames, grNames)



parnames <- data.table(pars = c("SpGrp1LargeClass1PartCutLog","SpGrp1LargeClass2PartCutLog",
                                "SpGrp1LargeClass3PartCutLog","SpGrp1LargeClass4PartCutLog",
                                "SpGrp1LargeClass5PartCutLog","SpGrp2LargeClass1PartCutLog",
                                "SpGrp2LargeClass2PartCutLog","SpGrp2LargeClass3PartCutLog",
                                "SpGrp2LargeClass4PartCutLog","SpGrp2LargeClass5PartCutLog",
                                "SpGrp3LargeClass1PartCutLog","SpGrp3LargeClass2PartCutLog",
                                "SpGrp3LargeClass3PartCutLog","SpGrp3LargeClass4PartCutLog",
                                "SpGrp3LargeClass5PartCutLog","SpGrp1SmallClass1PartCutLog",
                                "SpGrp1SmallClass2PartCutLog","SpGrp1SmallClass3PartCutLog",
                                "SpGrp1SmallClass4PartCutLog","SpGrp1SmallClass5PartCutLog",
                                "SpGrp2SmallClass1PartCutLog","SpGrp2SmallClass2PartCutLog",
                                "SpGrp2SmallClass3PartCutLog","SpGrp2SmallClass4PartCutLog",
                                "SpGrp2SmallClass5PartCutLog","SpGrp3SmallClass1PartCutLog",
                                "SpGrp3SmallClass2PartCutLog","SpGrp3SmallClass3PartCutLog",
                                "SpGrp3SmallClass4PartCutLog","SpGrp3SmallClass5PartCutLog"))

grNames <- CJ(SpGrp = c(1,2,3),
              sizeGr = c("small","large"),
              Decay = c(1,2,3,4,5))
setkey(grNames, sizeGr)
par_gr_pc <- cbind(parnames, grNames)


parnames <- data.table(pars = c("SpGrp1LargeClass1GapCutLog","SpGrp1LargeClass2GapCutLog",
                                "SpGrp1LargeClass3GapCutLog","SpGrp1LargeClass4GapCutLog",
                                "SpGrp1LargeClass5GapCutLog","SpGrp2LargeClass1GapCutLog",
                                "SpGrp2LargeClass2GapCutLog","SpGrp2LargeClass3GapCutLog",
                                "SpGrp2LargeClass4GapCutLog","SpGrp2LargeClass5GapCutLog",
                                "SpGrp3LargeClass1GapCutLog","SpGrp3LargeClass2GapCutLog",
                                "SpGrp3LargeClass3GapCutLog","SpGrp3LargeClass4GapCutLog",
                                "SpGrp3LargeClass5GapCutLog","SpGrp1SmallClass1GapCutLog",
                                "SpGrp1SmallClass2GapCutLog","SpGrp1SmallClass3GapCutLog",
                                "SpGrp1SmallClass4GapCutLog","SpGrp1SmallClass5GapCutLog",
                                "SpGrp2SmallClass1GapCutLog","SpGrp2SmallClass2GapCutLog",
                                "SpGrp2SmallClass3GapCutLog","SpGrp2SmallClass4GapCutLog",
                                "SpGrp2SmallClass5GapCutLog","SpGrp3SmallClass1GapCutLog",
                                "SpGrp3SmallClass2GapCutLog","SpGrp3SmallClass3GapCutLog",
                                "SpGrp3SmallClass4GapCutLog","SpGrp3SmallClass5GapCutLog"))

grNames <- CJ(SpGrp = c(1,2,3),
              sizeGr = c("small","large"),
              Decay = c(1,2,3,4,5))
setkey(grNames, sizeGr)
par_gr_gc <- cbind(parnames, grNames)



parnames <- data.table(pars = c("SpGrp1LargeClass1ClearCutLog","SpGrp1LargeClass2ClearCutLog",
                                "SpGrp1LargeClass3ClearCutLog","SpGrp1LargeClass4ClearCutLog",
                                "SpGrp1LargeClass5ClearCutLog","SpGrp2LargeClass1ClearCutLog",
                                "SpGrp2LargeClass2ClearCutLog","SpGrp2LargeClass3ClearCutLog",
                                "SpGrp2LargeClass4ClearCutLog","SpGrp2LargeClass5ClearCutLog",
                                "SpGrp3LargeClass1ClearCutLog","SpGrp3LargeClass2ClearCutLog",
                                "SpGrp3LargeClass3ClearCutLog","SpGrp3LargeClass4ClearCutLog",
                                "SpGrp3LargeClass5ClearCutLog","SpGrp1SmallClass1ClearCutLog",
                                "SpGrp1SmallClass2ClearCutLog","SpGrp1SmallClass3ClearCutLog",
                                "SpGrp1SmallClass4ClearCutLog","SpGrp1SmallClass5ClearCutLog",
                                "SpGrp2SmallClass1ClearCutLog","SpGrp2SmallClass2ClearCutLog",
                                "SpGrp2SmallClass3ClearCutLog","SpGrp2SmallClass4ClearCutLog",
                                "SpGrp2SmallClass5ClearCutLog","SpGrp3SmallClass1ClearCutLog",
                                "SpGrp3SmallClass2ClearCutLog","SpGrp3SmallClass3ClearCutLog",
                                "SpGrp3SmallClass4ClearCutLog","SpGrp3SmallClass5ClearCutLog"))

grNames <- CJ(SpGrp = c(1,2,3),
              sizeGr = c("small","large"),
              Decay = c(1,2,3,4,5))
setkey(grNames, sizeGr)
par_gr_cc <- cbind(parnames, grNames)


#mean no harvest:
nh_cwd <- cwd_init[Unit %in% DateCreekData::Treatments[Treatment == "NH"]$Unit,
                   .(SpGrp, sizeGr, Decay, propLA)]
nh_cwd[SpGrp == 1 & sizeGr == "large" & Decay ==1]

nh_cwd[, .(prop = mean(propLA)), .(SpGrp, sizeGr, Decay)]
sm_mn <- mean(diams_92[Unit %in% DateCreekData::Treatments[Treatment == "NH"]$Unit & 
                         sizeGr == "small"]$mean_diam)
lg_mn <- mean(diams_92[Unit %in% DateCreekData::Treatments[Treatment == "NH"]$Unit & 
                         sizeGr == "large"]$mean_diam)
propLA = c(sm_mn, lg_mn)











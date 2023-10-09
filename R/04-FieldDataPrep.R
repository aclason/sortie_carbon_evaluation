

#SBS---------------------------------------------------------------------------
#Tree data
summit.lk.dat <- read.csv("D:/Github/SummitLakeData/data-raw/SummitLakeData.csv")

summit.lk.dat <- summit.lk.dat %>% 
  mutate(DBH_09_live = as.numeric(DBH_09_live), 
         DBH_19_live = as.numeric(DBH_19_live))





##### LIVE STAMDING #########



##### DEAD STAMDING  #########




##### DOWNED WOOD VOL/HA #########
#calculate volumes from data 
F_cwd_sl <- SummitLakeData::CWD_vol_calc(dat_loc = "D:/Github/SummitLakeData/data-raw/CWD/", 
                                        incl_sp_decay = TRUE)

F_cwd_sl[, DecayClass:= as.numeric(DecayClass)]



#ICH---------------------------------------------------------------------------
##### LIVE STANDING #########



##### DEAD STAMDING #########




##### DOWNED WOOD VOL/HA #########

#calculate volumes from data 
F_cwd_dc <- DateCreekData::CWD_vol_calc(dat_loc = "D:/Github/DateCreekData/data-raw/CWD/", 
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





#--------------------------------------------------------------------------------------------------


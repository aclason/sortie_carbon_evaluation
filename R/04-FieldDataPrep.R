

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


#--------------------------------------------------------------------------------------------------


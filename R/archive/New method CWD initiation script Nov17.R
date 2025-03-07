library(data.table)

#calculate 1992 CWD:
dat_loc = "//SFP.IDIR.BCGOV/U164/ELILLES$/SORTIE-ND/SORTIE substrate decay and natural regen/CWD initiation/"
CWD_1992<-read.csv(paste0(dat_loc,"CWD_1992.csv"))
CWD_1992<-as.data.table(CWD_1992)
str(CWD_1992)
cwd92 <- CWD_1992_Vol_calc(CWD_dat = paste0(dat_loc,"CWD_1992.csv"), out_carbon_comp = TRUE)

cwd92_d <- CWD_1992_diams(CWD_dat = paste0(dat_loc,"CWD_1992.csv"))
cwd_p <- CWD_1992_props(CWD_dat = paste0(dat_loc,"CWD_1992.csv"))



###################################
###################################
##################################
#Creating new way to calculate proportion log area
###################################
###################################
##################################

#LePage et al. 2000 substrate covers for log proportions
#1.2 and 10.8 are percent cover for fresh log and moss covered log substrates in full canopy sites at Date Creek
#0.2 is rotten wood

#from SORTIE manual, formula to calculate log volume in m2/ha from percent log area (PLA) and mean diameter class (MDBH)
#LV = 1/3 * π * 100 * PLA * (MDBH / 2)
1/3 * pi *100 * (1.2+0.2+10.8)/100 * mean(CWD.1992$Diam_cm)/2 #mean log volume across 1992 data from LePage substrate estimates
#pretty close to values in Ingrid's paper!
#can we solve for PLA for each unit from each unit's volume?
#PLA = LV/(1/3 * π * (MDBH / 2))



#' 1992 CWD volume calculation
#'
#' @param CWD_dat
#'
#' @return
#' @export
#' @details Calculate Volume using the British Columbia Ministry of Forests and Range (2009) formula
#'  CWD volume (m3/ha) = pi^2/8L  *  sum[D2/cos (A)]
#'   Where: 	L = length of total transect (horizontal distance (HD) in m)
#'                                         HD = SD / Square root of [1 + (% slope / 100)2]
#' slope was not measured so assume total transect length to be 90m
#' D = diameter of each piece of CWD (cm)
#' A = tilt angle from horizontal for each piece (degrees)
#' Because tilt angle was not measured in 1992 and 1993, it was assumed to be zero,
#' so that cos (A) = 1 for all pieces in those years.
#'
#' @examples
CWD_1992_Vol_calc <- function(CWD_dat, out_carbon_comp = FALSE){
  #-----------------------Prepare data -----------------------------------------#
  # Import 1992
  CWD.1992<- fread(CWD_dat)

  # Square diameter
  CWD.1992[, D2_cosA:= Diam_cm^2, by = seq_len(nrow(CWD.1992))]
  
  # Category for small and large logs
  CWD.1992[, sizeGr:= ifelse(Diam_cm <20, "small", "large")]
  
  #Species groups
  Group1<-c("Hw","Ba", "Bl","Sx","Pl","U")
  Group3<-c("At","Ac","Ep")
  CWD.1992$SpGrp <-ifelse(CWD.1992$Sp %in% Group1, 1, 
                 ifelse(CWD.1992$Sp %in% Group3, 3, 2 ))

  # Convert individual piece to plot summary for L (length of total transect horizontal distance)
  if(out_carbon_comp == FALSE){
    CWD.1992_plot <- CWD.1992[, .(D2cosA = sum(D2_cosA)),
                              by =c("Year", "Unit", "Block", "Treatment", "Unique_plot", )]
  }else{
    #if volume will be used for SORTIE initiation, need to keep species group, size group and decay class columns
    CWD.1992_plot <- CWD.1992[, .(D2cosA = sum(D2_cosA)),
                              by =c("Year", "Unit","SpGrp",  "sizeGr", "Decay","Block", "Treatment", "Unique_plot")]
  }

  # Volume (m3/ha) calculation (includes transect length(90m))
  CWD.1992_plot[, VolumeHa:= (pi^2/(8*90)) * D2cosA]

  return(CWD.1992_plot)

}

#' Calculate mean diameters by size class for 1992 CWD by Unit
#'
#' @param CWD_dat
#'
#' @return
#' @export
#'
#' @examples
CWD_1992_diams <- function(CWD_dat){
  #-----------------------Prepare data -----------------------------------------#
  # Import 1992
  CWD.1992<- fread(CWD_dat)

  #### Average diameters
  
  # Category for small and large logs
  CWD.1992[, sizeGr:= ifelse(Diam_cm <20, "small", "large")]
  
  #Species groups
  Group1<-c("Hw","Ba", "Bl","Sx","Pl","U")
  Group3<-c("At","Ac","Ep")
  CWD.1992$SpGrp <-ifelse(CWD.1992$Sp %in% Group1, 1, 
                 ifelse(CWD.1992$Sp %in% Group3, 3, 2 ))

  CWD.1992_mnLogs <- CWD.1992[, .(mean_diam = mean(Diam_cm)), by = .(Unit, sizeGr, SpGrp, Decay)]

  return(CWD.1992_mnLogs)

}

cwd92 <- CWD_1992_Vol_calc(CWD_dat = paste0(dat_loc,"CWD_1992.csv"), out_carbon_comp = TRUE)
#for now assuming all Units have 10 plots
cwd92_Unit<-cwd92[,.(VolumeHa = sum(VolumeHa)/10), by = .(Unit,SpGrp,sizeGr,Decay)]
#check calculations
cwd92_Unit[,.(VolumeHa = sum(VolumeHa)), by = .(Unit)]

#reported in Ingrid paper... matches
#0% retention 
  #B4 Mesic  57.7±23.6 
  #D2 Mesic  120.2±18.6 
  #A3 Mesic–subhygric  133.1±37.8 
  #A1 Mesic–submesic  80.3±12.4 
#40% retention 
  #C2 Mesic  89.5±17.0 
  #D4 Mesic 163.3±41.1 
  #B2 Mesic–subhygric  116.0±24.5 
  #B3 Mesic–submesic  50.8±6.8 
#70% retention
  #B5 Mesic  61.7±11.1 
  #D5 Mesic  132.9±18.7 
  #C3 Mesic–subhygric  84.1±11.9 
  #A2 Mesic–submesic  94.3±24.2 
#Unharvested 
  #C1 Mesic  101.6±23.1 
  #D3 Mesic  285.5±44.6 
  #A4 Mesic–subhygric  156.8±35.6 
  #B1 Mesic–submesic  87.2±19.8

cwd92_d <- CWD_1992_diams(CWD_dat = paste0(dat_loc,"CWD_1992.csv"))
cwd92_Unit<-merge(cwd92_Unit, cwd92_d, by = c("Unit", "sizeGr", "SpGrp", "Decay"), all.x = TRUE)
  
cwd92_Unit$PLA<-  cwd92_Unit$VolumeHa /(1/3 * pi * (cwd92_Unit$mean_diam/ 2))

#Percent area logs by whole unit... 
#using mean diameters for every combination
#these seem realistic compared to LePage et al. 2000
cwd92_Unit[,.(PLA = sum(PLA)), by = .(Unit)]

#mean diameter for small and large size classes by unit (across species group and decay class)
# Category for small and large logs
  CWD.1992[, sizeGr:= ifelse(Diam_cm <20, "small", "large")]
  CWD.1992_mnLogs <- CWD.1992[, .(mean_diam_sz = mean(Diam_cm)), by = .(Unit, sizeGr)]

  #Percent log area using average diameters across only unit and size class which is what SORTIE will do
  cwd92_Unit<-merge(cwd92_Unit, CWD.1992_mnLogs, by = c("Unit", "sizeGr"), all.x = TRUE)
  
  cwd92_Unit$PLA_sz<-  cwd92_Unit$VolumeHa /(1/3 * pi * (cwd92_Unit$mean_diam_sz/ 2))

#Percent area logs by whole unit... the two methods are very close so this should be fine.
cwd92_Unit[,.(PLA_sz = sum(PLA_sz)), by = .(Unit)]

#PLA_sz and mean_diam_sz are substrate proportion parameters to initiate SORTIE!!!
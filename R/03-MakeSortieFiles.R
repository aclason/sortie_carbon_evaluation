library(rsortie)
library(data.table)

#SBS---------------------------------------------------------------------------
in_dir <- file.path("02_init_sortie", "02_summit_lake") 

My_basePath <- paste0(in_dir,"/ParameterFiles/BaseFiles/") 
My_newxmlPath <- paste0(in_dir,"/ParameterFiles/") 
My_newvalsPath <- paste0(in_dir,"/ParameterValues/") 

#plots
plots <- list.files(My_newvalsPath, pattern = "summit")

#list files
lstOfFiles <- data.frame("type"=c(0,rep(1,length(plots)),2,3),
                         "name"=c("SBS.xml",
                                  plots,"ds.csv","nci_si_6.csv"))
#add the inits to VariableNames
sl_variable_names <- treelistDfn(initname = "Init.Dens_", numDigits = 1, 
                                 diamMin = 82, diamMax = 90, diamInc = 2) 

rsortie::makeFiles(lstFiles = lstOfFiles, path_basexmls = My_basePath,
            path_newxmls = My_newxmlPath, path_newvals= My_newvalsPath,
            variable_names = sl_variable_names)

# TODO: confirm juvenile growth rate

# start with random draw around the mean to create variation in starting CWD:
cwd_p <- SummitLakeData::cwd_sub_boreal_props(CWD_dat = "./01_data/SBS_cwd_BRogers.csv",
                                              size_thresh = 15,
                                              out_carbon_comp = TRUE)

parnames <- data.table(pars = c("SpGrp1SmallClass1InitLogProp",
                                "SpGrp1SmallClass2InitLogProp","SpGrp1SmallClass3InitLogProp",
                                "SpGrp1SmallClass4InitLogProp","SpGrp1SmallClass5InitLogProp",
                                "SpGrp2SmallClass1InitLogProp","SpGrp2SmallClass2InitLogProp",
                                "SpGrp2SmallClass3InitLogProp","SpGrp2SmallClass4InitLogProp",
                                "SpGrp2SmallClass5InitLogProp","SpGrp3SmallClass1InitLogProp",
                                "SpGrp3SmallClass2InitLogProp","SpGrp3SmallClass3InitLogProp",
                                "SpGrp3SmallClass4InitLogProp","SpGrp3SmallClass5InitLogProp",
                                "SpGrp1LargeClass1InitLogProp","SpGrp1LargeClass2InitLogProp",
                                "SpGrp1LargeClass3InitLogProp","SpGrp1LargeClass4InitLogProp",
                                "SpGrp1LargeClass5InitLogProp","SpGrp2LargeClass1InitLogProp",
                                "SpGrp2LargeClass2InitLogProp","SpGrp2LargeClass3InitLogProp",
                                "SpGrp2LargeClass4InitLogProp","SpGrp2LargeClass5InitLogProp",
                                "SpGrp3LargeClass1InitLogProp","SpGrp3LargeClass2InitLogProp",
                                "SpGrp3LargeClass3InitLogProp","SpGrp3LargeClass4InitLogProp",
                                "SpGrp3LargeClass5InitLogProp"))

grNames <- data.table(SpGrp = rep(c(rep(1,5),rep(2,5),rep(3,5)),2),
                      sizeGr = c(rep("small",15), rep("large",15)), 
                      Decay = rep(seq(1,5),6))
par_gr <- cbind(parnames, grNames)

props_par <- merge(par_gr,
                   cwd_p,
                   by.x = c("SpGrp", "sizeGr", "Decay"),
                   by.y = c("SpGrp", "sizeGr", "Decay Class"),
                   all.x = TRUE)

for(i in 1:length(plots)){
  #draw from normal distribution around the mean
  props_par[, propLog := abs(rnorm(1, mean = propLA, sd = 0.01)), 
             by = seq_len(nrow(props_par))]
  
  behav_name <- data.table(pars = "DetailedSubstrate14", propLog = NA)
  props_pars <- rbind(behav_name, props_par[,.(pars,propLog)])
  
  #Drop the column name from column 1
  setnames(props_pars, c("pars","propLog"), c(" ","Interior_Spruce"))
  
  fwrite(props_pars, paste0(My_newvalsPath, "logProps.csv"))
  
  parFile <- grep(tools::file_path_sans_ext(plots[i]), 
                  grep("ds-nci_si_6.xml",list.files(My_newxmlPath), 
                       value = TRUE), value = TRUE)
  
  lstOfFiles <- data.frame("type"=c(0,1),
                           "name"=c(parFile, "logProps.csv"))
  
  
  makeFiles(lstFiles = lstOfFiles, path_basexmls = My_newxmlPath,
            path_newxmls = My_newxmlPath, path_newvals= My_newvalsPath)
}

# run for 1 time step first
files2run <- grep("si_6-logProps",list.files(My_newxmlPath, full.names = TRUE), 
                  value = TRUE)
#files2run <- grep("SBS-s",list.files(My_newxmlPath, full.names = TRUE), 
 #                 value = TRUE)

updateNumYears(files2run, 100)

runSortiePar(files2run, numcores = length(files2run), sortie_loc = 0)



#ICH---------------------------------------------------------------------------
in_dir <- file.path("02_init_sortie", "01_date_creek") 

#1. Make Sortie files from initial conditions and specifics to each harvest type
#2. Apply the harvest
#3. Update detailed substrate (proportion cwd?)
#4. open the files and save in Sortie gui (file does not compress properly with harvest update)
 
My_basePath <- paste0(in_dir,"/ParameterFiles/BaseFiles/") 
My_newxmlPath <- paste0(in_dir,"/ParameterFiles/") 
My_newvalsPath <- paste0(in_dir,"/ParameterValues/") 

TreatType <- c("CC","HR","LR","NH")
for(tt in 1:length(TreatType)){
  RunType <- TreatType[tt]
  if(RunType=="CC"){
    lstOfFiles <- read.csv(paste0(in_dir,"/FileLists/IniTClearCut.csv"))
  }else if(RunType=="HR"){
    lstOfFiles <- read.csv(paste0(in_dir,"/FileLists/InitHeavyHarvest.csv"))
  }else if(RunType=="LR"){
    lstOfFiles <- read.csv(paste0(in_dir,"/FileLists/InitLightHarvest.csv"))
  }else{ 
    lstOfFiles <- read.csv(paste0(in_dir,"/FileLists/InitNoHarvest.csv"))
  }
  makeFiles(lstFiles = lstOfFiles, path_basexmls = My_basePath,
            path_newxmls = My_newxmlPath, path_newvals= My_newvalsPath)
}

#2. Update the harvests - retained deciduous and brushing
#-----------------------------------------------------------------------------------
Units_path <- "../DateCreekData_NotFunctionsYet/data-raw/Harvests_Plants/UnitBoundaries/"
Gaps_path <- "../DateCreekData_NotFunctionsYet/data-raw/Harvests_Plants/GapCutsDateCreek/"

file_names <- list.files(My_newxmlPath, pattern = ".xml")

DateCreekData::apply_harvest(NewxmlPath = My_newxmlPath, Units_path = Units_path, 
                             Gaps_path = Gaps_path, file_names = file_names)



#3. Update detailed substrate (by unit) -------------------------------------------

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



for(i in 1:length(DateCreekData::Treatments$Unit)){
  
  props_par_init <- merge(par_gr_init,
                          cwd_init[Unit == DateCreekData::Treatments$Unit[i],
                                   .(SpGrp, sizeGr, Decay, propLA)],
                          by = c("SpGrp", "sizeGr", "Decay"),
                          all.x = TRUE)
  props_pars <- props_par_init[,.(pars, propLA)]
  
  sm_mn <- mean(diams_92[Unit == DateCreekData::Treatments$Unit[i] & 
                           sizeGr == "small"]$mean_diam)
  lg_mn <- mean(diams_92[Unit == DateCreekData::Treatments$Unit[i] & 
                           sizeGr == "large"]$mean_diam)
  props_pars <- rbind(props_pars, data.table(pars = c("InitSmallLogsMeanDiam",
                                                      "InitLargeLogsMeanDiam"),
                                             propLA = c(sm_mn, lg_mn)))
  
  
  if(DateCreekData::Treatments[i]$Treatment == "NH") {
    behav_name <- data.table(pars = "DetailedSubstrate19", propLA = NA)
    
  } else if (DateCreekData::Treatments[i]$Treatment == "CC") {
    behav_name <- data.table(pars = "DetailedSubstrate21", propLA = NA)
    
    #if it's a clearcut, update post-harvest proportions
    #only use 75% of the expected logs post-harvest to account for legacy CWD from pre-harvest
    props_par_cc <- merge(
      par_gr_cc,
      cwd_post_harvest[Unit == DateCreekData::Treatments$Unit[i],
                       .(SpGrp, sizeGr, Decay, propLA*0.75)],
      by = c("SpGrp", "sizeGr", "Decay"),
      all.x = TRUE
    )
    setnames(props_par_cc, "V4", "propLA")
    props_pars <- rbind(props_pars, props_par_cc[, .(pars, propLA)])
    
    #getting the mean diameter for < and > 20cm is key to capture volume and carbon
    sm_mn <- mean(diams_93[Unit == DateCreekData::Treatments$Unit[i] & 
                             sizeGr == "small"]$mean_diam)
    lg_mn <- mean(diams_93[Unit == DateCreekData::Treatments$Unit[i] & 
                             sizeGr == "large"]$mean_diam)
    props_pars <- rbind(props_pars, data.table(pars = c("ClearCutSmallLogMeanDiam",
                                                        "ClearCutLargeLogMeanDiam"),
                                               propLA = c(sm_mn, lg_mn)))
    
  } else if (DateCreekData::Treatments[i]$Treatment == "HR") {
    behav_name <- data.table(pars = "DetailedSubstrate20", propLA = NA)
    
    #if it's a heavy removal, update PartCut and GapCut based off proportions
    #only use 50% of the expected logs post-harvest (PC) to account for legacy CWD from pre-harvest
    #only use 75% of the expected logs post-harvest (Gap) to account for legacy CWD from pre-harvest
    #not as much new substrate created after partial harvest, so more legacies to balance out.
    props_par_pc <- merge(
      par_gr_pc,
      cwd_post_harvest[Unit == DateCreekData::Treatments$Unit[i],
                       .(SpGrp, sizeGr, Decay, propLA*0.5)],
      by = c("SpGrp", "sizeGr", "Decay"),
      all.x = TRUE
    )
    props_par_gc <- merge(
      par_gr_gc,
      cwd_post_harvest[Unit == DateCreekData::Treatments$Unit[i],
                       .(SpGrp, sizeGr, Decay, propLA*0.75)],
      by = c("SpGrp", "sizeGr", "Decay"),
      all.x = TRUE
    )
    setnames(props_par_pc, "V4", "propLA")
    setnames(props_par_gc, "V4", "propLA")
    props_pars <- rbind(props_pars, props_par_pc[, .(pars, propLA)],
                        props_par_gc[, .(pars, propLA)])
    
    #getting the mean diameter for < and > 20cm is key to capture volume and carbon
    sm_mn <- mean(diams_93[Unit == DateCreekData::Treatments$Unit[i] & 
                             sizeGr == "small"]$mean_diam)
    lg_mn <- mean(diams_93[Unit == DateCreekData::Treatments$Unit[i] & 
                             sizeGr == "large"]$mean_diam)
    props_pars <- rbind(props_pars, data.table(pars = c("PartCutSmallLogMeanDiam",
                                                        "PartCutLargeLogMeanDiam"),
                                               propLA = c(sm_mn, lg_mn)))
    
    sm_mn <- mean(diams_93[Unit == DateCreekData::Treatments$Unit[i] & 
                             sizeGr == "small"]$mean_diam)
    lg_mn <- mean(diams_93[Unit == DateCreekData::Treatments$Unit[i] & 
                             sizeGr == "large"]$mean_diam)
    props_pars <- rbind(props_pars, data.table(pars = c("GapCutSmallLogMeanDiam",
                                                        "GapCutLargeLogMeanDiam"),
                                               propLA = c(sm_mn, lg_mn)))
    
    
  } else {
    behav_name <- data.table(pars = "DetailedSubstrate20", propLA = NA)
    
    #if it's a light removal, update PartCut proportions
    props_par_pc <- merge(
      par_gr_pc,
      cwd_post_harvest[Unit == DateCreekData::Treatments$Unit[i],
                       .(SpGrp, sizeGr, Decay, propLA*0.5)],
      by = c("SpGrp", "sizeGr", "Decay"),
      all.x = TRUE
    )
    setnames(props_par_pc, "V4", "propLA")
    props_pars <- rbind(props_pars, props_par_pc[, .(pars, propLA)])
    
    #getting the mean diameter for < and > 20cm is key to capture volume and carbon
    sm_mn <- mean(diams_93[Unit == DateCreekData::Treatments$Unit[i] & 
                             sizeGr == "small"]$mean_diam)
    lg_mn <- mean(diams_93[Unit == DateCreekData::Treatments$Unit[i] & 
                             sizeGr == "large"]$mean_diam)
    props_pars <- rbind(props_pars, data.table(pars = c("PartCutSmallLogMeanDiam",
                                                        "PartCutLargeLogMeanDiam"),
                                               propLA = c(sm_mn, lg_mn)))
    
  }
  
  props_pars <- rbind(behav_name, props_pars)
  
  #props_pars[, propLog := propLog, by = seq_len(nrow(props_pars_i))]
  
  #props_pars[is.na(propLog), propLog := " "]
  #diameter sizes - not sure if I'm going to change this - pretty similar
  #c("InitSmallLogsMeanDiam","InitLargeLogsMeanDiam")
  
  #Drop the column name from column 1
  setnames(props_pars, c("pars","propLA"), c(" ","Western_Hemlock"))
  
  fwrite(props_pars, paste0(My_newvalsPath,
                            DateCreekData::Treatments$Unit[i],
                            "logProps.csv"))
  
  parFile <- grep(DateCreekData::Treatments$Unit[i],
                 list.files(My_newxmlPath), value = TRUE)
  
  lstOfFiles <- data.frame("type"=c(0,1),
                           "name"=c(parFile, paste0(DateCreekData::Treatments$Unit[i],
                                                    "logProps.csv")))
  
  
  makeFiles(lstFiles = lstOfFiles, path_basexmls = My_newxmlPath,
            path_newxmls = My_newxmlPath, path_newvals= My_newvalsPath)
}


files2run <- list.files(My_newxmlPath, full.names = TRUE, pattern = "logProps")
updateNumYears(files2run, 100)

runSortiePar(fname = files2run, numcores = 16, sortie_loc=0)


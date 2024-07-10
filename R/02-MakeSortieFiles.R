library(rsortie)
library(data.table)
#SBS---------------------------------------------------------------------------
in_dir <- file.path("02_init_sortie", "02_summit_lake") 

My_basePath <- paste0(in_dir,"ParameterFiles/BaseFiles/") 
My_newxmlPath <- paste0(in_dir,"ParameterFiles/") 
My_newvalsPath <- paste0(in_dir,"ParameterValues/") 

#plots
plots <- list.files(My_newvalsPath, pattern = "summit")

#list files
lstOfFiles <- data.frame("type"=c(0,rep(1,length(plots)),2),
                         "name"=c("SBS.xml",
                                  plots,"ds.csv"))
#add the inits to VariableNames
sl_variable_names <- treelistDfn(initname = "Init.Dens_", numDigits = 1, 
                                 diamMin = 82, diamMax = 88, diamInc = 2) 

rsortie::makeFiles(lstFiles = lstOfFiles, path_basexmls = My_basePath,
            path_newxmls = My_newxmlPath, path_newvals= My_newvalsPath,
            variable_names = sl_variable_names)

# TODO: need to add seedlings - none initiated
# TODO: run for 29 years, not 27
# TODO: add planted seedlings
# TODO: update juvenile growth rate to better reflect productivity?



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
                  grep("ds.xml",list.files(My_newxmlPath), 
                       value = TRUE), value = TRUE)
  
  lstOfFiles <- data.frame("type"=c(0,1),
                           "name"=c(parFile, "logProps.csv"))
  
  
  makeFiles(lstFiles = lstOfFiles, path_basexmls = My_newxmlPath,
            path_newxmls = My_newxmlPath, path_newvals= My_newvalsPath)
}

# run for 1 time step first
files2run <- grep("logProps",list.files(My_newxmlPath, full.names = TRUE), 
                  value = TRUE)
#files2run <- grep("SBS-s",list.files(My_newxmlPath, full.names = TRUE), 
 #                 value = TRUE)


#get regen data


updateNumYears(files2run, 30)



#ICH---------------------------------------------------------------------------
#1. Make Sortie files from initial conditions and specifics to each harvest type
#2. Apply the harvest
#3. Update detailed substrate (proportion cwd?)
#4. open the files and save in Sortie gui (file does not compress properly with harvest update)
 


loc_path <- "D:/GitHub/SORTIEparams/Inputs/ICH/Snags/" 
My_basePath <- paste0(loc_path,"ParameterFiles/BaseFiles/") 
My_newxmlPath <- paste0(loc_path,"ParameterFiles/") 
My_newvalsPath <- paste0(loc_path,"ParameterValues/") 

#each harvest type has specific harvest and other behaviours applied, so need to be 
# build separately in the Sortie GUI prior to parameter updates
TreatType <- c("CC","HR","LR","NH")
for(tt in 1:length(TreatType)){
  RunType <- TreatType[tt]
  if(RunType=="CC"){
    lstOfFiles <- read.csv(paste0(loc_path,"FileLists/IniTClearCut.csv"))
  }else if(RunType=="HR"){
    lstOfFiles <- read.csv(paste0(loc_path,"FileLists/InitHeavyHarvest.csv"))
  }else if(RunType=="LR"){
    lstOfFiles <- read.csv(paste0(loc_path,"FileLists/InitLightHarvest.csv"))
  }else{ 
    lstOfFiles <- read.csv(paste0(loc_path,"FileLists/InitNoHarvest.csv"))
  }
  makeFiles(lstFiles = lstOfFiles, path_basexmls = My_basePath,
            path_newxmls = My_newxmlPath, path_newvals= My_newvalsPath)
}

#2. Update the harvests - retained deciduous and brushing
#-----------------------------------------------------------------------------------
Units_path <- "../DateCreekData_NotFunctionsYet/data-raw/Harvests_Plants/UnitBoundaries/"
Gaps_path <- "../DateCreekData_NotFunctionsYet/data-raw/Harvests_Plants/GapCutsDateCreek/"

AddHarvestWbrush(NewxmlPath = My_newxmlPath, Units_path = Units_path, Gaps_path = Gaps_path,
                 ParamFile_Suffix= "-sn_d_init.xml")



#3. Update detailed substrate (by unit) -------------------------------------------

#move this to DateCreekData package - I'm not sure this is correct - check Erica's updates
#calculate 1992 CWD:
dat_loc = "D:/Github/DateCreekData/data-raw/CWD/"
cwd92 <- DateCreekData::CWD_1992_Vol_calc(CWD_dat = paste0(dat_loc,"CWD_1992.csv"), out_carbon_comp = TRUE)

cwd92_d <- DateCreekData::CWD_1992_diams(CWD_dat = paste0(dat_loc,"CWD_1992.csv"))
cwd_p <- DateCreekData::CWD_1992_props(CWD_dat = paste0(dat_loc,"CWD_1992.csv"))

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

for(i in 1:length(DateCreekData::Treatments$Unit)){
  
  props_par <- merge(par_gr,
                     cwd_p[Unit == DateCreekData::Treatments$Unit[i]],
                     by = c("SpGrp", "sizeGr", "Decay"),
                     all.x = TRUE)
  props_pars <- props_par[,.(pars,propLog)]
  
  props_pars[is.na(propLog), propLog := 0]
  
  if(DateCreekData::Treatments[i]$Treatment == "NH"){
    behav_name <- data.table(pars = "DetailedSubstrate16", propLog = NA)
  }else{
    behav_name <- data.table(pars = "DetailedSubstrate17", propLog = NA)
  }
  
  props_pars <- rbind(behav_name, props_pars)
  
  props_pars[, propLog := propLog, 
             by = seq_len(nrow(props_pars))]
  
  #props_pars[is.na(propLog), propLog := " "]
  #diameter sizes - not sure if I'm going to change this - pretty similar
  #c("InitSmallLogsMeanDiam","InitLargeLogsMeanDiam")
  
  #Drop the column name from column 1
  setnames(props_pars, c("pars","propLog"), c(" ","Western_Hemlock"))
  
  fwrite(props_pars, paste0(My_newvalsPath,
                            DateCreekData::Treatments$Unit[i],
                            "logProps.csv"))
  
  parFile <- grep(DateCreekData::Treatments$Unit[i],
                  grep("sn_d",list.files(My_newxmlPath), 
                       value = TRUE), value = TRUE)
  
  lstOfFiles <- data.frame("type"=c(0,1),
                           "name"=c(parFile, paste0(DateCreekData::Treatments$Unit[i],
                                                    "logProps.csv")))
  
  
  makeFiles(lstFiles = lstOfFiles, path_basexmls = My_newxmlPath,
            path_newxmls = My_newxmlPath, path_newvals= My_newvalsPath)
}

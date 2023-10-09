

#SBS---------------------------------------------------------------------------

loc_path <- "./Inputs/SORTIEruns/SummitLake/" 
My_basePath <- paste0(loc_path,"ParameterFiles/BaseFiles/") 
My_newxmlPath <- paste0(loc_path,"ParameterFiles/") 
My_newvalsPath <- paste0(loc_path,"ParameterValues/") 

#plots
plots <- list.files(My_newvalsPath, pattern = "summit")

#list files
lstOfFiles <- data.frame("type"=c(0,rep(1,length(plots)),2),
                         "name"=c("SBS-0506.xml",
                                  plots,"ds_part.csv"))
#add the inits to VariableNames
sl_variable_names <- treelistDfn(initname = "Init.Dens_", numDigits = 1, 
                                 diamMin = 82, diamMax = 88, diamInc = 2) 

rsortie::makeFiles(lstFiles = lstOfFiles, path_basexmls = My_basePath,
            path_newxmls = My_newxmlPath, path_newvals= My_newvalsPath,
            variable_names = sl_variable_names)

# TODO: need to add seedlings - none initiated
# TODO: run for 29 years, not 27

# start with random draw around the mean to create variation in starting CWD:
dat_loc = "D:/Github/DateCreekData/data-raw/CWD/"
cwd_p <- DateCreekData::CWD_1993_props(CWD_dat = paste0(dat_loc,"CWD_1993.csv"))

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

par_gr <- cbind(par_gr,sort(rep(DateCreekData::Treatments$Unit,
                                nrow(par_gr))))
setnames(par_gr, "V2", "Unit")

props_par <- merge(par_gr,
                   cwd_p,
                   by = c("Unit","SpGrp", "sizeGr", "Decay"),
                   all.x = TRUE)
props_par[is.na(propLog), propLog := 0]
props_par_tr <- merge(props_par, DateCreekData::Treatments,
                      by = "Unit")
#mean of all treatments
mn_props_par_tr <- props_par_tr[, .(mnPropLog = mean(propLog),
                                    sdPropLog = sd(propLog)), 
                                by = c("SpGrp","sizeGr","Decay","pars")]

for(i in 1:length(plots)){
  
  #draw from normal distribution around the mean
  mn_props_par_tr[, propLog := abs(rnorm(1, mean = mnPropLog, sd = 0.001)), 
             by = seq_len(nrow(mn_props_par_tr))]
  
  #mn_props_par_tr[, propLog := runif(1, 
   #                                  min = pmax((mnPropLog - 0.01),0), 
    #                                 max = (mnPropLog + 0.01)), 
     #             by = seq_len(nrow(mn_props_par_tr))]
  
  behav_name <- data.table(pars = "DetailedSubstrate14", propLog = NA)
  props_pars <- rbind(behav_name, mn_props_par_tr[,.(pars,propLog)])
  
  #Drop the column name from column 1
  setnames(props_pars, c("pars","propLog"), c(" ","Interior_Spruce"))
  
  fwrite(props_pars, paste0(My_newvalsPath, "logProps.csv"))
  
  parFile <- grep(tools::file_path_sans_ext(plots[i]), 
                  grep("ds_part.xml",list.files(My_newxmlPath), 
                       value = TRUE), value = TRUE)
  
  lstOfFiles <- data.frame("type"=c(0,1),
                           "name"=c(parFile, "logProps.csv"))
  
  
  makeFiles(lstFiles = lstOfFiles, path_basexmls = My_newxmlPath,
            path_newxmls = My_newxmlPath, path_newvals= My_newvalsPath)
}

# run for 1 time step first
files2run <- grep("logProps",list.files(My_newxmlPath, full.names = TRUE), 
                  value = TRUE)
updateNumYears(files2run, 100)



#ICH---------------------------------------------------------------------------

loc_path <- "./Inputs/ICH/JuvGrowth/" 
My_basePath <- paste0(loc_path,"ParameterFiles/BaseFiles/") 
My_newxmlPath <- paste0(loc_path,"ParameterFiles/") 
My_newvalsPath <- paste0(loc_path,"ParameterValues/") 

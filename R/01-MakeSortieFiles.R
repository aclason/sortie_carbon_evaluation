



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
                                  plots,"m.csv"))
#add the inits to VariableNames
sl_variable_names <- treelistDfn(initname = "Init.Dens_", numDigits = 1, diamMin = 82, diamMax = 88, diamInc = 2) 

rsortie::makeFiles(lstFiles = lstOfFiles, path_basexmls = My_basePath,
            path_newxmls = My_newxmlPath, path_newvals= My_newvalsPath,
            variable_names = sl_variable_names)

# TODO: need to add seedlings - none initiated


#ICH---------------------------------------------------------------------------

loc_path <- "./Inputs/ICH/JuvGrowth/" 
My_basePath <- paste0(loc_path,"ParameterFiles/BaseFiles/") 
My_newxmlPath <- paste0(loc_path,"ParameterFiles/") 
My_newvalsPath <- paste0(loc_path,"ParameterValues/") 

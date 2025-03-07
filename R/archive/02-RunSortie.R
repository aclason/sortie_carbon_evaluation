
library(data.table)
`%dopar%` <- foreach::`%dopar%`

################################ RUN SORTIE ####################################
#SBS---------------------------------------------------------------------------

loc_path <- "./Inputs/SORTIEruns/SummitLake/" 
My_newxmlPath <- paste0(loc_path,"ParameterFiles/") 
files2run <- list.files(in_dir,"6-logProps.xml", full.names = TRUE)

updateNumYears(files2run, 100)

runSortiePar(files2run, numcores = length(files2run), sortie_loc = 0)


#ICH---------------------------------------------------------------------------


#4. Run for 30 years
#-----------------------------------------------------------------------------------
# run for 1 time step first
files2run <- grep("logProps",list.files(My_newxmlPath, full.names = TRUE), 
                  value = TRUE)
updateNumYears(files2run, 30)


#files2run <- list.files("./Inputs/ICH/JuvGrowth/ParameterFiles", 
#                       pattern = "L-jp-stoch.xml|L-jp-stoch_nh.xml", full.names = TRUE)

runSortiePar(fname = files2run, numcores = 16, sortie_loc=0)

 

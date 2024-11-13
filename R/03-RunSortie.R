library(data.table)
library(rsortie)
################################ RUN SORTIE ####################################
#SBS---------------------------------------------------------------------------
#1. update the number of years for the run
#2. run Sortie in parallel. It's built to use one core/run, but future development could
#allow more flexibility in run setup
in_dir <- file.path("02_init_sortie","02_summit_lake","ParameterFiles")

files2run <- list.files(in_dir,"6-logProps.xml", full.names = TRUE)
#files2run <- list.files(My_newxmlPath,"ds_part.xml", full.names = TRUE)
#files2run <- grep("SBS-s",list.files(My_newxmlPath, full.names = TRUE), 
 #                 value = TRUE)

#1. update the number of years for the run
updateNumYears(files2run, 100)

runSortiePar(files2run, numcores = length(files2run), sortie_loc = 0)


#ICH---------------------------------------------------------------------------


#4. Run for 30 years
#-----------------------------------------------------------------------------------
#1. update the number of years for the run
#2. run Sortie in parallel. It's built to use one core/run, but future development could
#allow more flexibility in run setup

loc_path <- "./Inputs/SORTIEruns/SummitLake/" 
My_newxmlPath <- paste0(loc_path,"ParameterFiles/") 

files2run <- grep("logProps",list.files(My_newxmlPath, full.names = TRUE), 
                  value = TRUE)
#1. Update years
updateNumYears(files2run, 30)


#files2run <- list.files("./Inputs/ICH/JuvGrowth/ParameterFiles", 
#                       pattern = "L-jp-stoch.xml|L-jp-stoch_nh.xml", full.names = TRUE)
#2. Run Sortie
runSortiePar(fname = files2run, numcores = 16, sortie_loc=0)

 


library(data.table)
`%dopar%` <- foreach::`%dopar%`

################################ RUN SORTIE ####################################
#SBS---------------------------------------------------------------------------

loc_path <- "./Inputs/SORTIEruns/SummitLake/" 
My_newxmlPath <- paste0(loc_path,"ParameterFiles/") 
files2run <- list.files(My_newxmlPath,"logProps.xml", full.names = TRUE)
files2run <- list.files(My_newxmlPath,"ds_part.xml", full.names = TRUE)

runSortiePar(files2run, numcores = length(files2run), sortie_loc = 0)


#ICH---------------------------------------------------------------------------


 
 

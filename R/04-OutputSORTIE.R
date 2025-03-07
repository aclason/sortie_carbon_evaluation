library(rsortie)
library(doParallel)
library(DateCreekData)

######################### PROCESS SORTIE OUTPUTS ################################
#SBS---------------------------------------------------------------------------
in_dir <- file.path("03_out_sortie", "02_summit_lake")
plots_path <- file.path("02_init_sortie", "02_summit_lake","ParameterValues") 

#plots
plots <- list.files(plots_path, pattern = "summit")

#out_path <- file.path(base_dir, "./Inputs/SORTIEruns/SummitLake/Outputs/")
#paramFiles_run <- file.path(base_dir, "./Inputs/SORTIEruns/SummitLake/ParameterValues/")

plot_root <- stringr::str_split(plots,".csv",
                            simplify = TRUE)[,1]


files_2_ext <- list.files(in_dir, pattern = "ds-nci_si_6_det", full.names = FALSE)

if(!dir.exists(file.path(in_dir,"extracted"))){
  dir.create(file.path(in_dir,"extracted"))
}
#if need to extract files:
extractFiles(itype = 1, exname = paste0(in_dir,"/"), tarnames = files_2_ext)

extFileDir <- file.path(in_dir,"extracted")
treat_acronym <- "ds-nci_si_6"
treat_parse <- paste0(extFileDir,"/",grep("[[:digit:]].xml$",
                                      grep(treat_acronym,list.files(extFileDir),
                                           value=TRUE),value = TRUE))

numcores <- 19
parse_grids <- 1
parse_trees <- 1
# which years to parse
yrs <- seq(0,100) #years vary by plot

t_p <- grep(paste(paste0("_",yrs,".xml"),collapse = "|"),treat_parse, value = TRUE)

#in parallel:--------------

################## make this into a function!!!!!!!!!!!!!!!! ########################

#split treat_parse into treatments for parallel processing
t_p_l <- list()
for(i in 1:length(plot_root)){
  t_p_l[[i]] <- grep(plot_root[[i]],t_p, value = TRUE)
}


cl <- parallel::makeCluster(numcores)
doParallel::registerDoParallel(cl)
parallel::clusterEvalQ(cl, c(library(foreach),library(tidyverse),
                             library(data.table),library(rsortie),
                             library(stringr))) 
parallel::clusterExport(cl=cl, varlist=c("parse_grids","parse_trees",
                                         "treat_acronym","extFileDir",
                                         "t_p_l", "plot_root"))

g_dt_all <- foreach::foreach(i=1:length(t_p_l))%dopar%{
  #g_dt_all <- foreach::foreach(i=1:1)%dopar%{
  g_dt <- data.table()
  t_dt <- data.table()
  
  for(ii in 1:length(t_p_l[[i]])){
    # identify which treatment, year and unit is being parsed
    unn <- plot_root[stringr::str_detect(t_p_l[[i]][ii],plot_root)]
    yr <- sub('\\.xml$', '',stringr::str_split(t_p_l[[i]][ii],"det_")[[1]][2]) 
    print(paste("parsing:",unn,"timestep",yr))
    
    if(parse_grids == 1){
      # parse the output xml grid data
      g <- as.data.table(parseMap(t_p_l[[i]][ii]))
      
      g[, ':='(timestep = yr, Unit = unn)]
      g_dt <- rbind(g_dt, g, fill=TRUE)
    }
    
    if(parse_trees == 1){
      # parse the output xml grid data
      t <- as.data.table(parseXML(t_p_l[[i]][ii]))
      
      t[, ':='(timestep = yr, Unit = unn)]
      t_dt <- rbind(t_dt, t, fill=TRUE)
    }
    
  }
  if(parse_grids == 1){
    fwrite(g_dt, paste0(extFileDir,"/",treat_acronym,"-",unn,"-grids.csv"), append=FALSE)  
  }
  if(parse_trees == 1){
    fwrite(t_dt, paste0(extFileDir,"/",treat_acronym,"-",unn,"-trees.csv"), append=FALSE)  
  }
}

parallel::stopCluster(cl)

yrs <- seq(0,100)
dt_table <- c()
for(ij in 1:length(plot_root)){
  for(i in 1:length(yrs)){
    file_to_read <- paste0(in_dir,"/extracted/ext_SBS-",plot_root[ij],"-ds-nci_si_6_det_",yrs[i])
    dt <- fread(file_to_read,sep="\t", header=T,na.strings = "--", skip=1)
    dt[, ':='(timestep = yrs[i], Unit=plot_root[ij])]
    dt_table <- rbind(dt_table,dt)
  }
  write.csv(dt_table,
            paste0(in_dir,"/extracted/Sortie_ext_",plot_root[ij],".csv"), row.names = FALSE)
}

#----------------------------------------------------------------------------------
#ICH---------------------------------------------------------------------------
#using extract outputs from GUI:
in_dir <- file.path("03_out_sortie", "01_date_creek")

detailed_path <- "./03_out_sortie/01_date_creek/extracted/"
Outputs_path <- "./03_out_sortie/01_date_creek/extracted/"

Outputs_ending <- "_-"
years_to_extract <- seq(0,30)
Units_path <- "../DateCreekData_NotFunctionsYet/data-raw/Harvests_Plants/UnitBoundaries/"
Gaps_path <- "../DateCreekData_NotFunctionsYet/data-raw/Harvests_Plants/GapCutsDateCreek/"

# 7 Subplot tree outputs --------------
DateCreekData::subplot_outputs(out_path = Outputs_path,
                               det_out_path = detailed_path,
                               run_name = Outputs_ending, 
                               Units_path = Units_path, 
                               yrs = years_to_extract,
                               dist_edge = 50, size_subplot = 17.84, 
                               plotting = FALSE)

#--------------------------------------------------------------------------------------
#get the grids:
#out_path <- "../SORTIEparams/Outputs/ICH/CompMort/" 
##paramFiles_run <- "../SORTIEparams/Inputs/ICH/CompMort/ParameterValues/"
out_path <- "./03_out_sortie/01_date_creek/"
run_name <- ""
#paramFiles_run <- "../SORTIEparams/Inputs/ICH/CompMort/ParameterValues/"
plots <- DateCreekData::Treatments$Unit

files_2_ext <- grep(run_name,list.files(out_path, pattern = "det.gz.tar", 
                                     full.names = FALSE), value = TRUE)

if(!dir.exists(paste0(out_path,"extracted/"))){
  dir.create(paste0(out_path,"extracted/"))
}
#if need to extract files:
extractFiles(itype = 1, exname = out_path, tarnames = files_2_ext)

extFileDir <- paste0(out_path,"extracted/")
treat_acronym <- run_name
treat_parse <- paste0(extFileDir,grep("[[:digit:]].xml$",
                                      grep(treat_acronym,list.files(extFileDir),
                                           value=TRUE),value = TRUE))

numcores <- 16
parse_grids <- 1
parse_trees <- 0 #keep this at zero - takes way too long without only passing subplots
# which years to parse
yrs <- seq(0,100) 

t_p <- grep(paste(paste0("_",yrs,".xml"),collapse = "|"),treat_parse, value = TRUE)

#split treat_parse into treatments for parallel processing
t_p_l <- list()
for(i in 1:length(plots)){
  t_p_l[[i]] <- grep(plots[[i]],t_p, value = TRUE)
}


cl <- parallel::makeCluster(numcores)
doParallel::registerDoParallel(cl)
parallel::clusterEvalQ(cl, c(library(foreach),library(tidyverse),
                             library(data.table),library(rsortie),
                             library(stringr))) 
parallel::clusterExport(cl=cl, varlist=c("parse_grids","parse_trees",
                                         "treat_acronym","extFileDir",
                                         "t_p_l"))

g_dt_all <- foreach::foreach(i=1:length(t_p_l))%dopar%{
  #g_dt_all <- foreach::foreach(i=1:1)%dopar%{
  g_dt <- data.table()
  t_dt <- data.table()
  
  for(ii in 1:length(t_p_l[[i]])){
    # identify which treatment, year and unit is being parsed
    unn <- plots[stringr::str_detect(t_p_l[[i]][ii],plots)]
    yr <- sub('\\.xml$', '',stringr::str_split(t_p_l[[i]][ii],"det_")[[1]][2]) 
    print(paste("parsing:",unn,"timestep",yr))
    
    if(parse_grids == 1){
      # parse the output xml grid data
      g <- as.data.table(parseMap(t_p_l[[i]][ii]))
      
      g[, ':='(timestep = yr, Unit = unn)]
      g_dt <- rbind(g_dt, g, fill=TRUE)
    }
    
    if(parse_trees == 1){
      # parse the output xml grid data
      t <- as.data.table(parseXML(t_p_l[[i]][ii]))
      
      t[, ':='(timestep = yr, Unit = unn)]
      t_dt <- rbind(t_dt, t, fill=TRUE)
    }
    
  }
  if(parse_grids == 1){
    fwrite(g_dt, paste0(extFileDir,treat_acronym,"-",unn,"-grids.csv"), append=FALSE)  
  }
  if(parse_trees == 1){
    fwrite(t_dt, paste0(extFileDir,treat_acronym,"-",unn,"-trees.csv"), append=FALSE)  
  }
}

parallel::stopCluster(cl)

#output light grids ---------
outfiles <- list.files(extFileDir, pattern = ".csv", full.names = TRUE)
grid_files <- grep("grids",outfiles, value = TRUE)
grid_dt <- rbindlist(lapply(grid_files, fread))
grid_to_output <- grep("GLI",unique(grid_dt$mapnm), value = TRUE)
glis <- grid_dt[mapnm %in% grid_to_output]

#just output first 30 years

fwrite(glis[timestep <31], paste0(out_path,"glis.csv"))



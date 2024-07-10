library(rsortie)
library(doParallel)

######################### PROCESS SORTIE OUTPUTS ################################
#SBS---------------------------------------------------------------------------
in_dir <- file.path("03_out_sortie", "02_summit_lake")
plots_path <- file.path(in_dir,"ParameterValues") 

#plots
plots <- list.files(plots_path, pattern = "summit")

out_path <- file.path(base_dir, "./Inputs/SORTIEruns/SummitLake/Outputs/")
paramFiles_run <- file.path(base_dir, "./Inputs/SORTIEruns/SummitLake/ParameterValues/")

plot_root <- stringr::str_split(plots,".csv",
                            simplify = TRUE)[,1]


files_2_ext <- list.files(in_dir, pattern = "ds_det", full.names = FALSE)

if(!dir.exists(file.path(in_dir,"extracted"))){
  dir.create(file.path(in_dir,"extracted"))
}
#if need to extract files:
extractFiles(itype = 1, exname = paste0(in_dir,"/"), tarnames = files_2_ext)

extFileDir <- file.path(in_dir,"extracted")
treat_acronym <- "ds"
treat_parse <- paste0(extFileDir,"/",grep("[[:digit:]].xml$",
                                      grep(treat_acronym,list.files(extFileDir),
                                           value=TRUE),value = TRUE))

numcores <- 19
parse_grids <- 1
parse_trees <- 1
# which years to parse
yrs <- seq(0,30) #years vary by plot

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

#----------------------------------------------------------------------------------
#ICH---------------------------------------------------------------------------
out_path <- "../SORTIEparams/Outputs/ICH/CompMort/" 
paramFiles_run <- "../SORTIEparams/Inputs/ICH/CompMort/ParameterValues/"
plots <- DateCreekData::Treatments$Unit

files_2_ext <- grep("-cm",list.files(out_path, pattern = "det.gz.tar", 
                                     full.names = FALSE), value = TRUE)

if(!dir.exists(paste0(out_path,"extracted/"))){
  dir.create(paste0(out_path,"extracted/"))
}
#if need to extract files:
extractFiles(itype = 1, exname = out_path, tarnames = files_2_ext)

extFileDir <- paste0(out_path,"extracted/")
treat_acronym <- "-cm"
treat_parse <- paste0(extFileDir,grep("[[:digit:]].xml$",
                                      grep(treat_acronym,list.files(extFileDir),
                                           value=TRUE),value = TRUE))

numcores <- 16
parse_grids <- 1
parse_trees <- 0 #keep this at zero - takes way too long without only passing subplots
# which years to parse
yrs <- seq(0,30) 

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



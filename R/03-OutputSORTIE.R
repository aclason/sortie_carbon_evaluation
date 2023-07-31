
######################### PROCESS SORTIE OUTPUTS ################################
#SBS---------------------------------------------------------------------------
out_path <- "./Inputs/SORTIEruns/SummitLake/Outputs/" 
paramFiles_run <- "./Inputs/SORTIEruns/SummitLake/ParameterValues/"
plots <- stringr::str_split(list.files(paramFiles_run, pattern = "summit"),".csv",
                            simplify = TRUE)[,1]

files_2_ext <- list.files(out_path, pattern = "det", full.names = FALSE)

if(!dir.exists(paste0(out_path,"extracted/"))){
  dir.create(paste0(out_path,"extracted/"))
}
#if need to extract files:
extractFiles(itype = 1, exname = out_path, tarnames = files_2_ext)

extFileDir <- paste0(out_path,"extracted/")
treat_acronym <- "m"
treat_parse <- paste0(extFileDir,grep("[[:digit:]].xml$",
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

#----------------------------------------------------------------------------------
#ICH---------------------------------------------------------------------------
out_path <- "../SORTIEparams/Outputs/ICH/JuvGrowth/" 
paramFiles_run <- "../SORTIEparams/Inputs/ICH/JuvGrowth/ParameterValues/"
plots <- DateCreekData::Treatments$Unit

files_2_ext <- grep("gli",list.files(out_path, pattern = "det.gz.tar", 
                                     full.names = FALSE), value = TRUE)

if(!dir.exists(paste0(out_path,"extracted/"))){
  dir.create(paste0(out_path,"extracted/"))
}
#if need to extract files:
extractFiles(itype = 1, exname = out_path, tarnames = files_2_ext)

extFileDir <- paste0(out_path,"extracted/")
treat_acronym <- "cal"
treat_parse <- paste0(extFileDir,grep("[[:digit:]].xml$",
                                      grep(treat_acronym,list.files(extFileDir),
                                           value=TRUE),value = TRUE))

numcores <- 16
parse_grids <- 1
parse_trees <- 1
# which years to parse
yrs <- seq(0,30) #years vary by plot

t_p <- grep(paste(paste0("_",yrs,".xml"),collapse = "|"),treat_parse, value = TRUE)




library(sortieCarbon)
library(data.table)

#SBS--------------------------------------------------------------------------------------------
loc_path <- "./Inputs/SORTIEruns/SummitLake/" 
My_newvalsPath <- paste0(loc_path,"ParameterValues/")
out_path <- "./Inputs/SORTIEruns/SummitLake/Outputs/extracted/"

run_name <- "m"
outfiles <- list.files(out_path, pattern = ".csv", full.names = TRUE)
plots <- stringr::str_split(list.files(My_newvalsPath, pattern = "summit"),".csv",
                            simplify = TRUE)[,1]

### live trees
tree_files <- grep("trees",outfiles, value = TRUE)
tree_dt <- rbindlist(lapply(tree_files, fread))

#clip to centre 1 ha:
tree_dt <- tree_dt[X >50 & X <150 & Y >50 & Y <150]

## Basal area
tree_dt[, ':='(BA = treeCalcs::calcBA(DBH))]

## Carbon
sl_sortie <- calcSortieC(outputs = tree_dt, dead = TRUE, BEC = "SBS")



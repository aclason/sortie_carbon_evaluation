library(sortieCarbon)
library(data.table)

#SBS--------------------------------------------------------------------------------------------


### SORTIE ------------------
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


### Field -------------------



#ICH--------------------------------------------------------------------------------------------




### Field -------------------

#get plots for each measurement:
#1992:-------------------------------------------------------
# Count how many plots there are for each treatment unit
# so when averaging carbon/unit later, we can take into account the plots that had zero C
labels1992.cruise <- ddply(dat.1992.cruise[c("Unit", "PlotNum",  "DBH")], .(Unit, PlotNum), numcolwise(length))
labels1992.cruise$DBH <- NULL # this is really a count of the trees in each plot which we don't need in the labels
test.labels <- labels1992.cruise
test.labels$count <- rep(1, length(labels1992.cruise$Unit))
Plot_in_Units92 <- ddply(test.labels, .(Unit), numcolwise(sum))

#1993:-------------------------------------------------------
# Count how many plots there are for each treatment unit
# so when averaging carbon/unit later, we can take into account the plots that had zero C
dat.1993$count<-rep(1, length(dat.1993$Unit ))
Trees_in_Plots<-ddply(dat.1993[c("Unit", "PlotNum", "count")], .(Unit, PlotNum), numcolwise(sum))
Trees_in_Plots$count<-rep(1, length(Trees_in_Plots$Unit ))
Plot_in_Units<-ddply(Trees_in_Plots[c("Unit", "count")], .(Unit), numcolwise(sum))
Plot_in_Units #stands should have 23 or 30 plots (30 for 40% retention treatments)

#2010:-------------------------------------------------------
# Count how many plots there are for each treatment unit
# so when averaging carbon/unit later, we can take into account the plots that had zero C
dat.2010$count<-rep(1, length(dat.2010$Unit ))
Trees_in_Plots<-ddply(dat.2010[c("Unit", "Gd.Pt","Plot.Size", "count")], .(Unit, Gd.Pt,Plot.Size), numcolwise(sum))
Trees_in_Plots$count<-rep(1, length(Trees_in_Plots$Unit ))
Plot_in_Units<-ddply(Trees_in_Plots[c("Unit", "count")], .(Unit), numcolwise(sum))
Plot_in_Units #stands should have 20 or 30 plots (30 for 40% and 70% retention treatments)
# C2 is down 2 plots
#no clear-cut large tree plots

#201x--------------------------------------------------------
# Count how many plots there are for each treatment unit
# so when averaging carbon/unit later, we can take into account the plots that had zero C
dat.2018x$count <- rep(1, length(dat.2018x$Unit))
Trees_in_Plots <- ddply(dat.2018x[c("Unit", "Gd.Pt","Plot.Size", "count")], .(Unit, Gd.Pt,Plot.Size), numcolwise(sum))
Trees_in_Plots$count <- rep(1, length(Trees_in_Plots$Unit))
Plot_in_Units <- ddply(Trees_in_Plots[c("Unit", "count")], .(Unit), numcolwise(sum))
Plot_in_Units

#2022--------------------------------------------------------
# Count how many plots there are for each treatment unit
# so when averaging carbon/unit later, we can take into account the plots that had zero C
#read in plot labels to calculate original number of plots per unit
labels2022<-read.csv("Cleaned 2022 labels.csv", stringsAsFactors = TRUE)
labels2022$count <- rep(1, length(labels2022$Unit))
Plot_in_Units <- ddply(labels2022[c("Unit", "count")], .(Unit), numcolwise(sum))
Plot_in_Units #stands should have 20 or 30 plots (30 for 40% and 70% retention treatments),





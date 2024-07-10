library(data.table)
library(SummitLakeData)
library(DateCreekData)

in_path <- file.path("03_out_sortie")
out_path <- file.path("04_out_carbon")


#SBS--------------------------------------------------------------------------------------------
#to do - rename MgUnit to MgHa

### SORTIE ----------------------------------------------------
My_newvalsPath <- file.path("02_init_sortie","02_summit_lake","ParameterValues")
sum_in_path <- file.path(in_path, "02_summit_lake","extracted")

run_name <- "ds-"
outfiles <- grep(run_name, list.files(sum_in_path, pattern = ".csv", 
                                      full.names = TRUE), value = TRUE)
plots <- stringr::str_split(list.files(My_newvalsPath, pattern = "summit"),".csv",
                            simplify = TRUE)[,1]

tree_files <- grep("trees",outfiles, value = TRUE)
tree_dt <- rbindlist(lapply(tree_files, fread))

#clip to centre 1 ha:
tree_dt <- tree_dt[X >50 & X <150 & Y >50 & Y <150]

## Basal area
#tree_dt[, ':='(BA = treeCalcs::calc_BA(DBH))]

## Carbon
#use allometry to calculate height (From treeCalcs package)
M_trees_sl <- sortieCarbon::calcSortieC(sortie_outputs = tree_dt, dead = TRUE, BEC = "SBS",
                         Ht_from_diam = TRUE)
M_trees_sl[, unit := as.numeric(tstrsplit(Unit, "t", fixed = TRUE)[[2]])][,Unit:=NULL]
M_trees_sl[, Year := 1992 + timestep]


# by unit -------------------
# Live -----
MSL_trees_sl <- M_trees_sl[Type == "Adult" & DBH >=7.5,
                           .(MgUnit = sum(Mg_treeC)), by = .(unit, Year)]
MSL_trees_sl <- merge(SummitLakeData::Treatments, MSL_trees_sl, by = "unit", all = TRUE)
MSL_trees_sl[, State := "Live"]

# Dead -----
MSD_trees_sl <- M_trees_sl[Type == "Snag" & DBH >=7.5,
                           .(MgUnit = sum(Mg_treeC)), by = .(unit, Year)]
MSD_trees_sl <- merge(SummitLakeData::Treatments, MSD_trees_sl, by = "unit", all = TRUE)
MSD_trees_sl[, State := "Dead"]

#output -
saveRDS(MSL_trees_sl, file.path(out_path,"MSL_trees_sl.RDS"))
saveRDS(MSD_trees_sl, file.path(out_path,"MSD_trees_sl.RDS"))


#by species ------------------
# Live -----
MSL_trees_sl_sp <- M_trees_sl[Type == "Adult" & DBH >=7.5,
                              .(MgUnit = sum(Mg_treeC)), by = .(unit, Year, Species)]
MSL_trees_sl_sp <- merge(SummitLakeData::Treatments, MSL_trees_sl_sp, by = c("unit"), all = TRUE)
MSL_trees_sl_sp[, State := "Live"]

# Dead -----
MSD_trees_sl_sp <- M_trees_sl[Type == "Snag" & DBH >=7.5,
                              .(MgUnit = sum(Mg_treeC)), by = .(unit, Year, Species)]
MSD_trees_sl_sp <- merge(SummitLakeData::Treatments, MSD_trees_sl_sp, by = c("unit"), all = TRUE)
MSD_trees_sl_sp[, State := "Dead"]

saveRDS(MSL_trees_sl_sp, file.path(out_path,"MSL_trees_sl_sp.RDS"))
saveRDS(MSD_trees_sl_sp, file.path(out_path,"MSD_trees_sl_sp.RDS"))



### Field ----------------------------------------------------------
# using dima-height relationship - same as Date Creek
F_trees_sl <- SummitLakeData::clean_trees(raw_data = 
                                            "C:/Github/SummitLakeData/data-raw/SummitLakeData.csv")

#get the treatments & remove the unit/year combos that were not sampled:
treats <- SummitLakeData::Treatments[, .(Year = rep(sort(unique(F_trees_sl$Year)), 
                                                    length.out = .N*length(unique(F_trees_sl$Year)))),
                                     by = .(unit, treatment)]
remeas_freq_m <- melt(remeas_freq, id.vars = "PSP", variable.factor = FALSE)
setkey(remeas_freq_m, PSP)
setnames(remeas_freq_m, c("PSP","variable"),c("unit","Year"))
remeas_freq_m[,Year := as.numeric(Year)]
treats_surveyed <- merge(treats, remeas_freq_m[!is.na(value)], by = c("unit","Year"))
treats_surveyed[,value:=NULL]

#only use trees > 7.5cm dbh (moved to 7.5 cutoff in 2009)
F_trees_sl <- F_trees_sl[DBH >= 7.5]

# by unit -------------------
#each plot was 0.05ha in size (12.6m radius)
# Live -----
FSL_trees_sl <- F_trees_sl[State == "Live" & DeadStatus == "NotDown",
                           .(MgUnit = sum(Mg_treeC)*20), by = .(unit, Year, State)]
FSL_trees_sl <- merge(treats_surveyed, FSL_trees_sl, by = c("unit","Year"), all = TRUE)
FSL_trees_sl[is.na(MgUnit), `:=`(MgUnit = 0, State = "Live")]

# Dead -----
FSD_trees_sl <- F_trees_sl[State == "Dead" & DeadStatus == "Dead",
                           .(MgUnit = sum(Mg_treeC)*20), by = .(unit, Year, State)]
FSD_trees_sl <- merge(treats_surveyed, FSD_trees_sl, by = c("unit","Year"), all = TRUE)
FSD_trees_sl[is.na(MgUnit), `:=`(MgUnit = 0, State = "Dead")]

saveRDS(FSL_trees_sl, file.path(out_path,"FSL_trees_sl.RDS"))
saveRDS(FSD_trees_sl, file.path(out_path,"FSD_trees_sl.RDS"))

#by species ------------------
# Live -----
FSL_trees_sl_sp <- F_trees_sl[State == "Live" & DeadStatus == "NotDown",
                           .(MgUnit = sum(Mg_treeC)*20), by = .(unit, Year, State, Species)]
FSL_trees_sl_sp <- merge(treats_surveyed, FSL_trees_sl_sp, by = c("unit","Year"), all = TRUE)
FSL_trees_sl_sp[is.na(MgUnit), `:=`(MgUnit = 0, State = "Live")]

# Dead -----
FSD_trees_sl_sp <- F_trees_sl[State == "Dead" & DeadStatus == "Dead",
                           .(MgUnit = sum(Mg_treeC)*20), by = .(unit, Year, State, Species)]
FSD_trees_sl_sp <- merge(treats_surveyed, FSD_trees_sl_sp, by = c("unit","Year"), all = TRUE)
FSD_trees_sl_sp[is.na(MgUnit), `:=`(MgUnit = 0, State = "Dead")]

saveRDS(FSL_trees_sl_sp, file.path(out_path,"FSL_trees_sl_sp.RDS"))
saveRDS(FSD_trees_sl_sp, file.path(out_path,"FSD_trees_sl_sp.RDS"))



#snag fall?

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





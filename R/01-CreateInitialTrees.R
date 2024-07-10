library(SummitLakeData)
library(DateCreekData)


#SBS---------------------------------------------------------------------------
#1. Tree initial values [SummitLakeData]
#2. add natural regen from Bruce Rogers nearby study
#3. add cwd from Bruce Rogers trial

# add small trees from Bruce Rogers Sx trial:
small_trees <- fread("./Inputs/SORTIEruns/SummitLake/ParameterValues/nat_regen.csv")
#their height and diameter classes don't match SORTIE
small_trees[, sortie_bins := ifelse(Height_m <1.3, "Init.Dens_1",
                                ifelse(Height_m > 1.3 & DBH_cm < 2,"Init.Dens_2.0",
                                   ifelse(Height_m > 1.3 & DBH_cm > 2 & DBH_cm < 4,
                                          "Init.Dens_4.0",
                                          ifelse(Height_m > 1.3 & DBH_cm > 4 & DBH_cm < 6,
                                                     "Init.Dens_6.0",
                                               ifelse(Height_m > 1.3 & DBH_cm > 6 & DBH_cm < 7.5,
                                                              "Init.Dens_8.0",
                                                                NA)))))]
#assuming 5.64m plots
small_trees_sph <- small_trees[,.N/0.01, by=.(Plot,Species,sortie_bins)]
dcast(small_trees_sph, Plot+sortie_bins ~Species)



# add planted seedlings
sortie_files <- list.files(My_newxmlPath, pattern = "summit",full.names = T)

plantTrees <- SummitLakeData::planted_trees(planted_data =
                                              "D:/Github/SummitLakeData/data-raw/Trees/plantedTrees.csv")
setnames(plantTrees, "Plot","unit")
merge(SummitLakeData::Treatments, plantTrees, by = "unit")





#ICH---------------------------------------------------------------------------


#1. Tree initial values [DateCreekData] - cruise data + tallies



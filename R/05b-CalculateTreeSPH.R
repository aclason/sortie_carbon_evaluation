


F_trees_sl <- SummitLakeData::clean_trees(raw_data = 
                                            "D:/Github/SummitLakeData/data-raw/SummitLakeData.csv")
minDBH <- round(min(F_trees_sl$DBH, na.rm = TRUE),0)
maxDBH <- round(max(F_trees_sl$DBH, na.rm = TRUE),0)
# Create a vector of DBH size classes, by 2 cm increments
diam_classes <- seq(minDBH,(maxDBH + 2),
                    by = 2)
# Create column for and fill with DBH bins
for(j in 1:length(diam_classes)){
  F_trees_sl[DBH <= diam_classes[j] & DBH > diam_classes[j] - 2,
             DBH_bin := diam_classes[j]]
}

#add all zeros:
all_poss <- CJ(unique(F_trees_sl$unit), unique(F_trees_sl$Species), unique(F_trees_sl$Year),
               unique(F_trees_sl$State), unique(F_trees_sl$DBH_bin))
setnames(all_poss,c("V1","V2","V3","V4","V5"),
         c("unit","Species","Year","State","DBH_bin"))

plot_ha <- 1/0.05
F_trees_sl[, SPH := 1 * plot_ha]

#merge with sl_dat
sl_poss <- merge(F_trees_sl, all_poss,
                 by = c("unit","Species","Year","State","DBH_bin"),
                 all = T)
sl_poss <- sl_poss[, .(unit, Species, Year, State, DBH_bin, SPH)]
#sl_poss <- sl_poss[!is.na(DBH_bin)]
sl_poss[is.na(SPH), SPH := 0]
sl_sum <- sl_poss[, .(SPH = sum(SPH)), by = .(unit,Species,Year,State,DBH_bin)]


ggplot(data = sl_sum[unit == 12])+
  geom_col(aes(x = DBH_bin, y = SPH, fill = Species))+
  facet_grid(c("unit","Year"))+
  theme(legend.position = "bottom")

################## just do this if looking at SPH #################
#leave it as individuals (for SPH calcs):
minDBH <- round(min(M_trees_sl$DBH, na.rm = TRUE),0)
maxDBH <- round(max(M_trees_sl$DBH, na.rm = TRUE),0)
# Create a vector of DBH size classes, by 2 cm increments
diam_classes <- seq(minDBH,(maxDBH + 2),
                    by = 2)
for(j in 1:length(diam_classes)){
  M_trees_sl[DBH <= diam_classes[j] & DBH > diam_classes[j] - 2,
             DBH_bin := diam_classes[j]]
}

M_trees_sl_sph <- M_trees_sl[, .(SPH = .N), by = .(unit, Year, Type, tree_species, DBH_bin)]

M_trees_sl_sph_sdl <- M_trees_sl_sph[Type == "Seedling"]
M_trees_sl_sph_sap <- M_trees_sl_sph[Type == "Sapling"]
M_trees_sl_sph <- M_trees_sl_sph[Type != "Seedling" & Type != "Sapling"]

#add all zeros:
all_poss <- CJ(unique(M_trees_sl_sph$unit), unique(M_trees_sl_sph$tree_species), 
               unique(M_trees_sl_sph$Year),
               unique(M_trees_sl_sph$Type), unique(M_trees_sl_sph$DBH_bin))
setnames(all_poss,c("V1","V2","V3","V4","V5"),
         c("unit","tree_species","Year","Type","DBH_bin"))

M_trees_sl_sph_a <- merge(M_trees_sl_sph, all_poss,
                          by = c("unit","tree_species","Year","Type","DBH_bin"),
                          all = T)
M_trees_sl_sph_a[is.na(SPH), SPH := 0]
M_trees_sl_sph_a <- M_trees_sl_sph_a[Year == 1992 |
                                       Year == 1994 |
                                       Year == 1997 |
                                       Year == 2009 |
                                       Year == 2018]

#ggplot(data = M_trees_sl_sph_a[unit == 12 & Type == "Adult"])+
#  geom_col(aes(x = DBH_bin, y = SPH, fill = tree_species))+
# facet_wrap(~Year)+
#  theme(legend.position = "bottom")

#ggplot(data = M_trees_sl_sph_a[unit == 3 & Type == "Snag"])+
# geom_col(aes(x = DBH_bin, y = SPH, fill = tree_species))+
#facet_wrap(~Year)+
#theme(legend.position = "bottom")

#ggplot(data = M_trees_sl_sph_a[Type == "Adult"])+
#  geom_col(aes(x = DBH_bin, y = SPH, fill = tree_species))+
# facet_grid(c("unit","Year"))+
#  theme(legend.position = "bottom")
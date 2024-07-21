library(data.table)
library(SummitLakeData)
library(DateCreekData)

in_dir <- "01_data"
out_dir <- file.path("02_init_sortie")
#SBS---------------------------------------------------------------------------
#1. Tree initial values [SummitLakeData]
#2. add natural regen from Bruce Rogers nearby study
#3. Planted trees
#4. write out the parameter values initial tree conditions
SummitLakeData::sl_sortie_inits(dbh_size_class = 2,
                                plot_area = 0.05,
                                raw_data = "D:/Github/SummitLakeData/data-raw/SummitLakeData.csv",
                                in_dir = in_dir,
                                out_dir = out_dir)


#ICH---------------------------------------------------------------------------

#1. Tree initial values [DateCreekData] - cruise data + tallies
# Where to save initial tree and snag conditions
param_path <- "./Inputs/ICH/Snags/ParameterValues/"
param_dir <- file.path("02_init_sortie","01_date_creek","ParameterValues")

tree_dat <- "D:/Github/DateCreekData/data-raw/Trees/1992data.csv"
tally_dat <- "D:/Github/DateCreekData/data-raw/Trees/1992fixed_radius_data_fromTable20_DateCkHandbook.csv"

Live_dt <- createTreeListSortie(cruise_data = tree_dat,
                                fixed_data = tally_dat,
                                liveTrees = TRUE)
Live_dt[,DiamClass := paste0("Init.Dens_",DiamClass)]

Dead_dt <- createTreeListSortie(dateCreek92_data = tree_dat,
                                dateCreek_92_tallies = tally_dat,
                                liveTrees = FALSE)
Dead_dt[,DiamClass := paste0("SnagInit_",DiamClass)]
#1 going to take the sum of all decay classes to start the run
Dead_dt_1 <- Dead_dt[,.(Amabalis_Fir = sum(Amabalis_Fir),
                        Black_Cottonwood = sum(Black_Cottonwood),
                        Hybrid_spruce = sum(Hybrid_spruce), 
                        Lodgepole_Pine = sum(Lodgepole_Pine),
                        Paper_Birch = sum(Paper_Birch),
                        Subalpine_Fir = sum(Subalpine_Fir), 
                        Trembling_Aspen = sum(Trembling_Aspen), 
                        Western_Hemlock = sum(Western_Hemlock),
                        Western_redcedar = sum(Western_redcedar)), 
                     by=c("Unit","DiamClass")]


# read spatial boundaries
## Date Creek Treatments
NH<-c("A4", "B1", "C1", "D3") #No harvest
LR<-c("A2", "B5", "C3", "D5") # light removal (30% BasalArea)
HR<-c("B2", "B3", "C2", "D4") #heavy removal (60% BasalArea)
CC<-c("A1", "A3", "B4", "D2") #Clear-cut (100% removal) with some caveats (some deciduous left standing, one small 
Blocks <- c(NH,LR,HR,CC)

block_bounds <- ReadSpatialBounds("D:/Github/DateCreekData/data-raw/Harvests_Plants/UnitBoundaries/")

#planting data
PlantDensDat <- fread("D:/Github/DateCreekData/data-raw/Harvests_Plants/PlantingDensities.csv", na.strings="n/a", stringsAsFactors = TRUE)
PlantPCDat <- fread("D:/Github/DateCreekData/data-raw/Harvests_Plants/PlantingPercents.csv",na.strings="n/a", 
                    stringsAsFactors = TRUE)
pl_dens <- PlantDensDat[,.(DiamClass ="PlantingDens", Amabalis_Fir=sum(.SD)),by=c("Unit","GM")]


for(j in 1:length(Blocks)){
  Unit_i <- Blocks[j]
  TreatType <- ifelse(Unit_i=="A2"||Unit_i=="B5"||Unit_i== "C3"||Unit_i== "D5","LR",
                      ifelse(Unit_i=="B2"||Unit_i=="B3"||Unit_i=="C2"||Unit_i=="D4","HR",
                             ifelse(Unit_i=="A1"||Unit_i=="A3"||Unit_i=="B4"||Unit_i=="D2","CC","NH")))
  # Get Unit size dimensions
  bb <- st_bbox(st_buffer(block_bounds %>% filter(Unit==Unit_i), dist = 10))
  Side_length <- plyr::round_any(max(abs(bb[1]-bb[3]),abs(bb[2]-bb[4])), accuracy=8, f=ceiling)
  
  # stick live and snag inits together for each unit
  a <- rbind(data.table(DiamClass="na"),Live_dt[Unit==Unit_i], 
             data.table(DiamClass="na"),Dead_dt_1[Unit == Unit_i, .(Unit,DiamClass,Amabalis_Fir,Black_Cottonwood,
                                                                    Hybrid_spruce,Lodgepole_Pine,Paper_Birch,
                                                                    Subalpine_Fir,Trembling_Aspen,Western_Hemlock,
                                                                    Western_redcedar)],fill=TRUE)
  a[,Unit:=NULL]
  # Add the na row beneath the species name (requeired for r-SORTIE parameter file updates)
  #For the harvested treatments:
  if(TreatType=="CC"||TreatType=="HR"||TreatType=="LR"){ 
    d <- a
    
    if(TreatType=="HR"){
      ### For heavy removal (low retention), two plants: 1st in the gaps, 2nd in matrix
      e <- rbind(d,data.table(DiamClass="Plant21",Amabalis_Fir=NA),fill=TRUE)
      pd1 <- rbind(e,PlantPCDat[Unit==Unit_i& GM =="G",
                                .(DiamClass ="PlantingProp_1",Amabalis_Fir, Black_Cottonwood,
                                  Hybrid_spruce,Lodgepole_Pine, Paper_Birch,Subalpine_Fir,
                                  Trembling_Aspen,Western_Hemlock, Western_redcedar)])
      
      ff <- rbind(pd1,pl_dens[Unit==Unit_i& GM=="G",
                              .(DiamClass="PlantingDens_1",Amabalis_Fir)],fill=TRUE)
    }else{
      ### For Clearcuts and low removal (high retention), just one planting
      #Add the planting
      e <- rbind(d,data.table(DiamClass="Plant21",Amabalis_Fir=NA),fill=TRUE)
      pd1 <- rbind(e,PlantPCDat[Unit==Unit_i,.(DiamClass ="PlantingProp",Amabalis_Fir, Black_Cottonwood,
                                               Hybrid_spruce,Lodgepole_Pine, Paper_Birch,Subalpine_Fir,
                                               Trembling_Aspen,Western_Hemlock, Western_redcedar)])
      ff <- rbind(pd1,pl_dens[Unit==Unit_i,.(DiamClass,Amabalis_Fir)],fill=TRUE)
    }
  }else{ #not harvested
    ff <- a
  }
  #Change plot size
  ii <- rbind(ff,data.table(DiamClass="plot_lenX",Amabalis_Fir= round(Side_length,0)),fill=TRUE)
  jj <- rbind(ii,data.table(DiamClass="plot_lenY",Amabalis_Fir= round(Side_length,0)),fill=TRUE)
  
  #Drop the column name from column 1
  setnames(jj, "DiamClass"," ")
  #write out the parameter value updates to file
  write.csv(jj,paste0(param_path,Unit_i,".csv"),quote=TRUE,row.names=FALSE)
}















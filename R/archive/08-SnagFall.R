# Alana Clason

#to assess snag dynamics from sortie runs


#SBS---------------------------------------------------------------------------
out_path <- "./Inputs/SORTIEruns/SummitLake/Outputs/" 
#these are created from detailed output extraction, to match the Date Creek runs below.
#Extracting all live trees from those large runs is slow in R compared to C++ (sortie GUI)

out_files <- list.files(path = out_path, pattern = "summit_trees.csv",
                        full.names = TRUE)
sl_out <-  fread(out_files)

sl_out[, UniqXY := paste0(unit,"_",X,"_",Y)]
setkey(sl_out,UniqXY)

#1. Label the row with whether a snag is created, or snag falls. 
#This won't capture trees that fall without becoming snags

#which trees are snags at some point
ids <- unique(sl_out[Type=="Snag"]$UniqXY)

#just adults and snags where somewhere they becaome snags
sl_out_as <- sl_out[Type == "Adult" | Type =="Snag"]

#this does the state change from adult to snag
sl_out_as[, state_change := Type != data.table::shift(Type, type = "lag", 
                                                     fill = NA), by = "UniqXY"]
sl_out_as[, state_type := ifelse(Type == "Adult","Adult",
                            ifelse(Type == "Snag" & `Dead Code` =="Alive" & #not sure why dead code is diff
                                         state_change == TRUE, "SnagCreate",
                             ifelse(Type == "Snag" & `Dead Code` =="Alive", "Snag",
                              ifelse(Type=="Snag" & `Dead Code` == "Natural", "SnFallNext",
                               ifelse(Type == "Snag" & `Dead Code` == "Harvest",
                                                            "SnagHarvest", NA)))))]
#sl_out_as[,unit:=as.numeric(unit)]
sl_out_as <- merge(sl_out_as, SummitLakeData::Treatments, by ="unit")

#2. Calculate Snag creation and snag fall rate - Plot level
sl_adult <- sl_out_as[state_type=="Adult", .N, by=.(treatment, unit, timestep)]
setnames(sl_adult, "N", "NumAdult")
sl_snagcreate <- sl_out_as[state_type=="SnagCreate", .N, by=.(treatment, unit, timestep)]
setnames(sl_snagcreate, "N", "NumSnags")
sl_snagfall <- sl_out_as[state_type=="SnFallNext", .N, by=.(treatment, unit, timestep)]
setnames(sl_snagfall, "N", "NumSnagFall")

sl_ad_sn <- merge(sl_adult, sl_snagcreate, by=c("treatment","unit","timestep"))
sl_ad_sn <- merge(sl_ad_sn, sl_snagfall, by=c("treatment","unit","timestep"))
sl_ad_sn <- sl_ad_sn[,.(NumAdult, NumSnags, NumSnagFall,
                        SnagRecrRate = (NumSnags/(NumAdult+NumSnags)), 
                        SnagFallRate = (NumSnagFall/(NumSnags+NumSnagFall))),
                     by=c("treatment","unit","timestep")]

#2. Calculate Snag creation and snag fall rate - by species
sl_adult <- sl_out_as[state_type=="Adult", .N, by=.(treatment, unit, timestep,Species)]
setnames(sl_adult, "N", "NumAdult")
sl_snagcreate <- sl_out_as[state_type=="SnagCreate", .N, by=.(treatment, unit, timestep, Species)]
setnames(sl_snagcreate, "N", "NumSnags")
sl_snagfall <- sl_out_as[state_type=="SnFallNext", .N, by=.(treatment, unit, timestep,Species)]
setnames(sl_snagfall, "N", "NumSnagFall")

sl_ad_sn_sp <- merge(sl_adult, sl_snagcreate, by=c("treatment","unit","timestep","Species"))
sl_ad_sn_sp <- merge(sl_ad_sn_sp, sl_snagfall, by=c("treatment","unit","timestep","Species"))
sl_ad_sn_sp <- sl_ad_sn_sp[,.(NumAdult, NumSnags, NumSnagFall,
                        SnagRecrRate = (NumSnags/(NumAdult+NumSnags)), 
                        SnagFallRate = (NumSnagFall/(NumSnags+NumSnagFall))),
                     by=c("treatment","unit","timestep","Species")]

#3. how long are snags standing
#of the snags that fall, how long were they standing? So first figure out which trees fall
fallIDs <- unique(sl_out_as[state_type == "SnFallNext"]$UniqXY)

#then count how many years they stood as snags
for(i in 1:length(fallIDs)){
  sl_out_as[UniqXY == fallIDs[i] & Type == "Snag", TimeAsSnag := .N]
}

sl_snag_time <- sl_out_as[UniqXY %in% fallIDs & state_type == "SnFallNext",
                      .(treatment,unit,Species,DBH,UniqXY,TimeAsSnag)]

#----------------------------------------------------------------------------------
#ICH---------------------------------------------------------------------------

#read in outputs
#these are created from detailed output extraction, then subplot methods in R. Extracting
#all live trees from these large runs is slow in R compared to C++ (sortie GUI)
out_path <- "../SORTIEparams/Outputs/ICH/CompMort/" 

out_files <- list.files(path = out_path, pattern = "-cm.csv",
                        full.names = TRUE)

out_path <- "../SORTIEparams/Outputs/ICH/Snags/" 

out_files <- list.files(path = out_path, pattern = "sn_d_init.csv",
                        full.names = TRUE)


dc_out <-  rbindlist(map(out_files,~ fread(.)), fill = TRUE)

#just adults and snags where somewhere they becaome snags
dc_out_as <- dc_out[Type == "Adult" | Type =="Snag"]

#not all outputs have snag break height
#dc_NH <- dc_out[Treatment=="NH"]
dc_out_as[, UniqXY := paste0(Unit,"_",X,"_",Y)]
setkey(dc_out_as,UniqXY)

#1. Label the row with whether a snag is created, or snag falls. 
#This won't capture trees that fall without becoming snags

#which trees are snags at some point
ids <- unique(dc_out_as[Type=="Snag"]$UniqXY)


#need to make this in parallel if using this approach - very slow - UPDATED...just delete?
#for(i in 1:length(ids)){
  #looking for adults, and snags that will fall
  #dc_NH_as[UniqXY == ids[i], stType := ifelse(Type == "Adult","Adult",
          #                                 ifelse(Type == "Snag" & Dead.Code =="Alive",
         #                                         "Snag",
        #                                          ifelse(Type=="Snag" & Dead.Code == "Natural",
       #                                                  "SnFallNext",
      #                                              ifelse(Type == "Snag" & Dead.Code == "Harvest",
     #                                                      "SnagHarvest",
    #                                                  ifelse(Type == "Sapling","Sapling",
   #                                                        "Seedling")))))]
  # identify if/when snag created from live tree
  #snagCh <- which(dc_NH_as[UniqXY == ids[i]]$Type != 
 #                   dplyr::lag(dc_NH_as[UniqXY == ids[i]]$Type))
 # snagCh <- which(dc_out[UniqXY == ids[i]]$Type != 
                    #dplyr::lag(dc_out[UniqXY == ids[i]]$Type))
#  if(length(snagCh) > 0){
  #  dc_NH_as[UniqXY == ids[i]][snagCh]$stType <- "SnagCreate"  
 # }
#}

#this does the state change from adult to snag
dc_out_as[, state_change := Type != data.table::shift(Type, type = "lag", 
                                                     fill = NA), by = "UniqXY"]
dc_out_as[, state_type := ifelse(Type == "Adult","Adult",
                          ifelse(Type == "Snag" & Dead.Code =="Alive" &
                                   state_change == TRUE, "SnagCreate",
                            ifelse(Type == "Snag" & Dead.Code =="Alive", "Snag",
                              ifelse(Type=="Snag" & Dead.Code == "Natural", "SnFallNext",
                                ifelse(Type == "Snag" & Dead.Code == "Harvest",
                                       "SnagHarvest", NA)))))]


#2. Calculate Snag creation and snag fall rate - Plot level
dc_adult <- dc_out_as[state_type=="Adult", .N, by=.(Treatment, Unit, timestep)]
setnames(dc_adult, "N", "NumAdult")
dc_snagcreate <- dc_out_as[state_type=="SnagCreate", .N, by=.(Treatment, Unit, timestep)]
setnames(dc_snagcreate, "N", "NumSnagCreate")
dc_snagfall <- dc_out_as[state_type=="SnFallNext", .N, by=.(Treatment, Unit, timestep)]
setnames(dc_snagfall, "N", "NumSnagFall")

dc_ad_sn <- merge(dc_adult, dc_snagcreate, by=c("Treatment","Unit","timestep"), all=TRUE)
dc_ad_sn <- merge(dc_ad_sn, dc_snagfall, by=c("Treatment","Unit","timestep"), all=TRUE)
dc_ad_sn <- dc_ad_sn[, lapply(.SD, function(x) ifelse(is.na(x), 0, x))]
dc_ad_sn <- dc_ad_sn[,.(NumAdult, NumSnagCreate, NumSnagFall,
                        SnagRecrRate = (NumSnagCreate/(NumAdult+NumSnagCreate)), 
                        SnagFallRate = (NumSnagFall/(NumSnagCreate+NumSnagFall))),
                     by=c("Treatment","Unit","timestep")]

#2. Calculate Snag creation and snag fall rate - by Species
dc_adult <- dc_out_as[state_type=="Adult", .N, by=.(Treatment, Unit, Species, timestep)]
setnames(dc_adult, "N", "NumAdult")
dc_snagcreate <- dc_out_as[state_type=="SnagCreate", .N, by=.(Treatment, Unit, Species, timestep)]
setnames(dc_snagcreate, "N", "NumSnagCreate")
dc_snagfall <- dc_out_as[state_type=="SnFallNext", .N, by=.(Treatment, Unit, Species, timestep)]
setnames(dc_snagfall, "N", "NumSnagFall")

dc_ad_sn_sp <- merge(dc_adult, dc_snagcreate, by=c("Treatment","Unit","timestep","Species"),
                     all = TRUE)
dc_ad_sn_sp <- merge(dc_ad_sn_sp, dc_snagfall, by=c("Treatment","Unit","timestep","Species"),
                     all = TRUE)
dc_ad_sn_sp <- dc_ad_sn_sp[, lapply(.SD, function(x) ifelse(is.na(x), 0, x))]
dc_ad_sn_sp


dc_ad_sn_sp <- dc_ad_sn_sp[,.(NumAdult, NumSnagCreate, NumSnagFall,
                        SnagRecrRate = (NumSnagCreate/(NumAdult+NumSnagCreate)), 
                        SnagFallRate = (NumSnagFall/(NumSnagCreate+NumSnagFall))),
                     by=c("Treatment","Unit","timestep","Species")]


#3. how long are snags standing
#of the snags that fall, how long were they standing? So first figure out which trees fall
fallIDs <- unique(dc_out_as[state_type == "SnFallNext"]$UniqXY)

#then count how many years they stood as snags
for(i in 1:length(fallIDs)){
  dc_out_as[UniqXY == fallIDs[i] & Type == "Snag", TimeAsSnag := .N]
}

SnagTime <- dc_out_as[UniqXY %in% fallIDs & state_type == "SnFallNext", 
                      .(Treatment,Unit,Species,DBH,UniqXY,TimeAsSnag)]



ggplot(dc_ad_sn)+
  geom_point(aes(x = timestep, y = SnagRecrRate, colour = as.character(Treatment)))+
  geom_smooth(aes(x = timestep, y = SnagRecrRate, colour = as.character(Treatment)),size=1.5)+
  theme_minimal()+
  ylab("Snag Recruitment (adults - snags) Rate (%)")+
  ylim(c(0,100))
#ggsave("D:/Github/sortie_carbonExtensionNote/Outputs/Figures/SnagRecruitRate_newCM.jpg")
ggplot(sl_ad_sn)+
  geom_point(aes(x = timestep, y = SnagRecrRate, colour = as.character(Treatment)))+
  geom_smooth(aes(x = timestep, y = SnagRecrRate, colour = as.character(Treatment)),size=1.5)+
  theme_minimal()+
  ylab("Snag Recruitment (adults - snags) Rate (%)")+
  ylim(c(0,100))
#which trees are dying
ggplot(sl_ad_sn_sp)+
  geom_point(aes(x = timestep, y = SnagRecrRate, colour = Treatment))+
  #geom_line(aes(x = timestep, y = SnagFallRate, colour = Treatment),size=1.5)+
  geom_smooth(aes(x = timestep, y = SnagRecrRate, colour = Treatment),
              method = "lm")+
  theme_minimal()+
  facet_wrap("Species")+
  ylab("Snag Recruitment (adults - snags) Rate (%)")


ggplot(sl_ad_sn)+
  geom_point(aes(x = timestep, y = SnagFallRate, colour = Treatment),size=4)+
  #geom_line(aes(x = timestep, y = SnagFallRate, colour = Treatment),size=1.5)+
  geom_smooth(aes(x = timestep, y = SnagFallRate, colour = Treatment),size=1.5,
              method = "lm")+
  theme_minimal()+
  ylab("Snag Fall Rate (snags - snag fall) (%)")
#ggsave("D:/Github/sortie_carbonExtensionNote/Outputs/Figures/SnagFallRate_newSnagDyn.jpg")

ggplot(sl_ad_sn[timestep<30])+
  geom_point(aes(x = timestep, y = SnagFallRate, colour = Treatment),size=4)+
  #geom_line(aes(x = timestep, y = SnagFallRate, colour = Treatment),size=1.5)+
  geom_smooth(aes(x = timestep, y = SnagFallRate, colour = Treatment),size=1.5,
              method = "lm")+
  theme_minimal()+
  ylab("Snag Fall Rate (snags - snag fall) (%)")

ggplot(sl_ad_sn_sp)+
  geom_point(aes(x = timestep, y = SnagFallRate, colour = Species),size=4)+
  #geom_smooth(aes(x = timestep, y = SnagFallRate))+
  #geom_line(aes(x = timestep, y = SnagFallRate, colour = Species),size=0.5)+
  scale_colour_manual(values = RColorBrewer::brewer.pal(4, "Set3"))+
  theme_minimal()+
  ylab("Snag Fall Rate (snags - snag fall) Rate (%)")+
  facet_wrap("Treatment")

ggplot()+
  geom_point(aes(x = timestep, y = SnagFallRate, colour = as.character(Unit)),size=2, data = dc_ad_sn)+
  #geom_line(aes(x = timestep, y = SnagFallRate, colour = as.character(Unit)),size=1, data = dc_ad_sn)+
  geom_point(aes(x = timestep, y = SnagFallRate, colour = as.character(Unit)),size=2, data = sl_ad_sn)+
  #geom_line(aes(x = timestep, y = SnagFallRate, colour = as.character(Unit)),size=1, data = sl_ad_sn)+
  geom_smooth(aes(x = timestep, y = SnagFallRate),size=2, colour = "red", data = dc_ad_sn)+
  geom_smooth(aes(x = timestep, y = SnagFallRate),size=2, colour = "blue", data = sl_ad_sn)+
  theme_minimal()+
  ylab("Snag Fall Rate (snags - snag fall) Rate (%)")


ggplot(sl_snag_time)+
  geom_boxplot(aes(y = Unit, x = TimeAsSnag, fill = Unit))+
  theme_minimal()+
  ylab("Number of years standing as snag")+
  facet_wrap("Species")
ggsave("D:/Github/sortie_carbonExtensionNote/Outputs/Figures/SnagLongevity.jpg")

ggplot(sl_snag_time)+
  geom_boxplot(aes(y = as.character(Species), x = TimeAsSnag, fill =Species))+
  theme_minimal()+
  ylab("Number of years standing as snag")
facet_wrap("Species")
ggsave("D:/Github/sortie_carbonExtensionNote/Outputs/Figures/SnagLongevity.jpg")


#from SORTIE parameters: 
# tree fall probability (never becomes a snag)
#which trees are dying
ggplot(dc_ad_sn_sp)+
  geom_point(aes(x = timestep, y = SnagRecrRate, colour = Treatment))+
  #geom_line(aes(x = timestep, y = SnagFallRate, colour = Treatment),size=1.5)+
  geom_smooth(aes(x = timestep, y = SnagRecrRate, colour = Treatment),
              method = "lm")+
  theme_minimal()+
  facet_wrap("Species")+
  ylab("Snag Recruitment (adults - snags) Rate (%)")

ggplot(dc_ad_sn_sp)+
  geom_point(aes(y = SnagFallRate, x = timestep, 
                 colour = Treatment))+
  geom_smooth(aes(y = SnagFallRate, x = timestep, 
                  colour = Treatment))+
  facet_wrap(~Species)+
  theme_minimal()+
  ylab("Number of years standing as snag")+
  theme_set(theme_minimal(base_family = "Arial") +  # Change "Arial" to your desired font
              theme(text = element_text(family = "Arial"),  # Change "Arial" to your desired font
                    plot.title = element_text(size = 14, face = "bold"),
                    axis.title = element_text(size = 14, face = "bold"),
                    axis.text = element_text(size = 14, face = "bold"),
                    legend.text = element_text(size = 14, face = "bold"),
                    strip.text = element_text(size = 14, face = "bold"),
                    legend.title = element_text(size = 14, face = "bold")
              ))+
  scale_fill_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )

#Check against the snag run:
unique(NH_tr[Type=="Adult"]$Dead.Code) # no adults were assigned a dead code of natural (or harvest)


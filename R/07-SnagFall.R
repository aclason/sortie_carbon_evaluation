# Alana Clason

#to assess snag dynamics from sortie runs
library(data.table)
library(purrr)
library(dplyr)

in_path <- file.path("03_out_sortie")
out_path <- file.path("05_out_analysis")


#SBS---------------------------------------------------------------------------
My_newvalsPath <- file.path("02_init_sortie","02_summit_lake","ParameterValues")
sum_in_path <- file.path(in_path, "02_summit_lake","extracted")

run_name <- "ds-nci_si_6"
plots <- stringr::str_split(list.files(My_newvalsPath, pattern = "summit"),".csv",
                            simplify = TRUE)[,1]
yrs <- seq(0,100)
#rsortie parsing function is not correctly attributing dead codes, so need to use GUI
# output to calculate snag fall
dt_table <- data.table()
for(j in 1:length(plots)){
  for(i in 1:length(yrs)){
    file_to_read <- paste0(sum_in_path,"/ext_SBS-",plots[j],"-",run_name,"_det_",yrs[i])
    dt <- fread(file_to_read,sep="\t", header=T,na.strings = "--", skip=1)
    dt[, ':='(timestep = yrs[i], Unit= plots[j])]
    #dt <- dt[X >50 & X <150 & Y >50 & Y <150]
    dt_table <- rbind(dt_table,dt)
  }
}
#just adults and snags where somewhere they becaome snags
sl_out_as <- dt_table[Type == "Adult" | Type =="Snag"]

saveRDS(sl_out_as, paste0(file.path(in_path, "02_summit_lake"),"sl_snags.rds"))


# start here to read in snags: -----------------------
sl_out_as <- readRDS(file.path(in_path, "02_summit_lake","sl_snags.rds"))
#clip to centre 1 ha:
#tree_dt <- tree_dt[X >50 & X <150 & Y >50 & Y <150]

sl_out_as[Type == "Snag" & dead == 0]
sl_out_as[, UniqXY := paste0(Unit,"_",X,"_",Y)]
setkey(sl_out_as,UniqXY)

#1. Label the row with whether a snag is created, or snag falls. 
#This won't capture trees that fall without becoming snags
#which trees are snags at some point
ids <- unique(sl_out_as[Type=="Snag"]$UniqXY)


#this does the state change from adult to snag
sl_out_as[, state_change := Type != data.table::shift(Type, type = "lag", 
                                                     fill = NA), by = "UniqXY"]

#if using output processed via SORTIE GUI:
#then there are codes of "alive" and natural applied - 
#I still don't know how codes of snag harvest would be coded
#sl_out_as[, state_type := ifelse(Type == "Adult","Adult",
 #                             ifelse(Type == "Snag" & `Dead Code` =="Alive" & #not sure why dead code is diff
  #                                    state_change == TRUE, "SnagCreate",
   #                             ifelse(Type == "Snag" & `Dead Code` =="Alive", "Snag",
    #                             ifelse(Type=="Snag" & `Dead Code` == "Natural", "SnFallNext",
     #                             ifelse(Type == "Snag" & `Dead Code` == "Harvest",
      #                             "SnagHarvest", NA)))))]

sl_out_as[, state_type := ifelse(Type == "Adult","Adult",
                           ifelse(Type == "Snag" & `Dead Code` =="Alive" & 
                                    state_change == TRUE, "SnagCreate",
                            ifelse(Type == "Snag" & `Dead Code` =="Alive"& 
                                     state_change == FALSE, "Snag",
                             ifelse(Type=="Snag" & `Dead Code` == "Natural"&
                                      state_change == TRUE, "SnagCreate&Fall",
                              ifelse(Type=="Snag" & `Dead Code` == "Natural"&
                                       state_change == FALSE, "SnFallNext",
                               ifelse(Type == "Snag" & `Dead Code` == "Harvest"&
                                        state_change == TRUE, "SnagCreateHarvest",
                                ifelse(Type == "Snag" & `Dead Code` == "Harvest"&
                                         state_change == FALSE, "SnagHarvest", NA)))))))]
                  
sl_treats <- SummitLakeData::Treatments[, .(Unit = paste0("summit_",unit), treatment)]
sl_out_as <- merge(sl_out_as, sl_treats, by.x ="Unit", by.y = "Unit")
setnames(sl_out_as, "treatment", "Treatment")

#2. Calculate Snag creation and snag fall rate - Plot level
sl_adult <- sl_out_as[state_type=="Adult", .N, by=.(Treatment, Unit, timestep)]
setnames(sl_adult, "N", "NumAdult")
sl_snag <- sl_out_as[state_type=="Snag", .N, by=.(Treatment, Unit, timestep)]
setnames(sl_snag, "N", "NumExistSnags")
sl_snagcreate <- sl_out_as[state_type=="SnagCreate"|state_type=="SnagCreate&Fall",
                           .N, by=.(Treatment, Unit, timestep)]
setnames(sl_snagcreate, "N", "NumSnagCreate")
sl_snagfall <- sl_out_as[state_type=="SnFallNext"|state_type=="SnagCreate&Fall",
                         .N, by=.(Treatment, Unit, timestep)]
setnames(sl_snagfall, "N", "NumSnagFall")

sl_ad_sn <- merge(sl_adult, sl_snag, by=c("Treatment","Unit","timestep"), all=TRUE)
sl_ad_sn <- merge(sl_ad_sn, sl_snagcreate, by=c("Treatment","Unit","timestep"), all=TRUE)
sl_ad_sn <- merge(sl_ad_sn, sl_snagfall, by=c("Treatment","Unit","timestep"), all=TRUE)
sl_ad_sn <- sl_ad_sn[, lapply(.SD, function(x) ifelse(is.na(x), 0, x))]
sl_ad_sn <- sl_ad_sn[,.(NumAdult, NumSnagCreate, NumSnagFall,
                        SnagRecrRate = ifelse(NumSnagCreate == 0,0,
                                              (NumSnagCreate/(NumAdult+NumSnagCreate))*100), 
                        SnagFallRate = ifelse(NumSnagFall == 0,0, 
                                              (NumSnagFall/(NumExistSnags +NumSnagCreate+
                                                              NumSnagFall))*100)),
                     by=c("Treatment","Unit","timestep")]

#3. Calculate Snag creation and snag fall rate - by Species
sl_adult <- sl_out_as[state_type=="Adult", .N, by=.(Treatment, Unit, timestep,Species)]
setnames(sl_adult, "N", "NumAdult")
sl_snag <- sl_out_as[state_type=="Snag", .N, by=.(Treatment, Unit, timestep,Species)]
setnames(sl_snag, "N", "NumExistSnags")
sl_snagcreate <- sl_out_as[state_type=="SnagCreate"|state_type=="SnagCreate&Fall",
                           .N, by=.(Treatment, Unit, timestep,Species)]
setnames(sl_snagcreate, "N", "NumSnagCreate")
sl_snagfall <- sl_out_as[state_type=="SnFallNext"|state_type=="SnagCreate&Fall",
                         .N, by=.(Treatment, Unit, timestep,Species)]
setnames(sl_snagfall, "N", "NumSnagFall")


sl_ad_sn_sp <- merge(sl_adult, sl_snag, by=c("Treatment","Unit","timestep","Species"),
                     all = TRUE)
sl_ad_sn_sp <- merge(sl_ad_sn_sp, sl_snagcreate, by=c("Treatment","Unit","timestep","Species"),
                     all = TRUE)
sl_ad_sn_sp <- merge(sl_ad_sn_sp, sl_snagfall, by=c("Treatment","Unit","timestep","Species"),
                     all = TRUE)
sl_ad_sn_sp <- sl_ad_sn_sp[, lapply(.SD, function(x) ifelse(is.na(x), 0, x))]
sl_ad_sn_sp


sl_ad_sn_sp <- sl_ad_sn_sp[,.(NumAdult, NumSnagCreate, NumSnagFall,NumExistSnags,
                               SnagRecrRate = ifelse(NumSnagCreate == 0,0,
                                                     (NumSnagCreate/(NumAdult+NumSnagCreate))*100), 
                               SnagFallRate = ifelse(NumSnagFall == 0,0, 
                                                     (NumSnagFall/(NumExistSnags +NumSnagCreate+
                                                                     NumSnagFall))*100)),
                            by=c("Treatment","Unit","timestep","Species")]

#3. how long are snags standing
#of the snags that fall, how long were they standing? So first figure out which trees fall
fallIDs <- unique(sl_out_as[state_type == "SnFallNext"|state_type=="SnagCreate&Fall"]$UniqXY)

#then count how many years they stood as snags
for(i in 1:length(fallIDs)){
  sl_out_as[UniqXY == fallIDs[i] & Type == "Snag", TimeAsSnag := .N]
}

sl_snag_time <- sl_out_as[UniqXY %in% fallIDs & state_type == "SnFallNext"|
                            UniqXY %in% fallIDs & state_type == "SnagCreate&Fall",
                      .(Treatment,Unit,DBH,UniqXY,TimeAsSnag)]
sl_snag_time[, study := "SummitLake"]

sl_snagTime_sp <- sl_out_as[UniqXY %in% fallIDs & state_type == "SnFallNext"|
                              UniqXY %in% fallIDs & state_type == "SnagCreate&Fall", 
                            .(Treatment,Unit,Species,DBH,UniqXY,TimeAsSnag)]
sl_snagTime_sp[, study := "SummitLake"]

#----------------------------------------------------------------------------------
#ICH---------------------------------------------------------------------------

#read in outputs
#these are created from detailed output extraction, then subplot methods in R. Extracting
#all live trees from these large runs is slow in R compared to C++ (sortie GUI)
dc_in_path <- file.path(in_path, "01_date_creek","extracted")

outfiles <- grep("grids",list.files(dc_in_path, pattern = ".csv", 
                                    full.names = TRUE),
                 value = TRUE, invert = TRUE)
outfiles <- grep("ah",outfiles, invert = T, value = T)
tree_dt <- rbindlist(lapply(outfiles, fread), fill = TRUE)

#just adults and snags where somewhere they becaome snags
dc_out_as <- tree_dt[Type == "Adult" | Type =="Snag"]

#not all outputs have snag break height
#dc_NH <- dc_out[Treatment=="NH"]
dc_out_as[, UniqXY := paste0(Unit,"_",X,"_",Y)]
setkey(dc_out_as,UniqXY)

#1. Label the row with whether a snag is created, or snag falls. 
#This won't capture trees that fall without becoming snags

#which trees are snags at some point
ids <- unique(dc_out_as[Type=="Snag"]$UniqXY)

#this does the state change from adult to snag
dc_out_as[, state_change := Type != data.table::shift(Type, type = "lag", 
                                                     fill = NA), by = "UniqXY"]
#dc_out_as[, state_type := ifelse(Type == "Adult","Adult",
 #                         ifelse(Type == "Snag" & Dead.Code =="Alive" &
  #                                 state_change == TRUE, "SnagCreate",
   #                         ifelse(Type == "Snag" & Dead.Code =="Alive", "Snag",
    #                          ifelse(Type=="Snag" & Dead.Code == "Natural", "SnFallNext",
     #                           ifelse(Type == "Snag" & Dead.Code == "Harvest",
      #                                 "SnagHarvest", NA)))))]

dc_out_as[, state_type := ifelse(Type == "Adult","Adult",
                           ifelse(Type == "Snag" & Dead.Code =="Alive" & 
                                    state_change == TRUE, "SnagCreate",
                            ifelse(Type == "Snag" & Dead.Code =="Alive"& 
                                     state_change == FALSE, "Snag",
                             ifelse(Type=="Snag" & Dead.Code == "Natural"&
                                      state_change == TRUE, "SnagCreate&Fall",
                              ifelse(Type=="Snag" & Dead.Code == "Natural"&
                                       state_change == FALSE, "SnFallNext",
                                ifelse(Type == "Snag" & Dead.Code == "Harvest"&
                                        state_change == TRUE, "SnagCreateHarvest",
                                  ifelse(Type == "Snag" & Dead.Code == "Harvest"&
                                           state_change == FALSE, "SnagHarvest", NA)))))))]

#2. Calculate Snag creation and snag fall rate - Plot level
dc_adult <- dc_out_as[state_type=="Adult", .N, by=.(Treatment, Unit, timestep)]
setnames(dc_adult, "N", "NumAdult")
dc_snag <- dc_out_as[state_type=="Snag", .N, by=.(Treatment, Unit, timestep)]
setnames(dc_snag, "N", "NumExistSnags")
dc_snagcreate <- dc_out_as[state_type=="SnagCreate"|state_type=="SnagCreate&Fall", 
                           .N, by=.(Treatment, Unit, timestep)]
setnames(dc_snagcreate, "N", "NumSnagCreate")
dc_snagfall <- dc_out_as[state_type=="SnFallNext"|state_type=="SnagCreate&Fall", 
                         .N, by=.(Treatment, Unit, timestep)]
setnames(dc_snagfall, "N", "NumSnagFall")
dc_ad_sn <- merge(dc_adult, dc_snag, by=c("Treatment","Unit","timestep"), all = TRUE)
dc_ad_sn <- merge(dc_ad_sn, dc_snagcreate, by=c("Treatment","Unit","timestep"), all=TRUE)
dc_ad_sn <- merge(dc_ad_sn, dc_snagfall, by=c("Treatment","Unit","timestep"), all=TRUE)
dc_ad_sn <- dc_ad_sn[, lapply(.SD, function(x) ifelse(is.na(x), 0, x))]
dc_ad_sn <- dc_ad_sn[,.(NumAdult, NumSnagCreate, NumExistSnags, NumSnagFall,
                        SnagRecrRate = ifelse(NumSnagCreate == 0,0,
                                              (NumSnagCreate/(NumAdult+NumSnagCreate))*100), 
                        SnagFallRate = ifelse(NumSnagFall == 0,0, 
                                              (NumSnagFall/(NumExistSnags + NumSnagCreate+
                                                              NumSnagFall))*100)),
                     by=c("Treatment","Unit","timestep")]


#2. Calculate Snag creation and snag fall rate - by Species
dc_adult <- dc_out_as[state_type=="Adult", .N, by=.(Treatment, Unit, timestep, Species)]
setnames(dc_adult, "N", "NumAdult")
dc_snag <- dc_out_as[state_type=="Snag", .N, by=.(Treatment, Unit, timestep, Species)]
setnames(dc_snag, "N", "NumExistSnags")
dc_snagcreate <- dc_out_as[state_type=="SnagCreate"|state_type=="SnagCreate&Fall", 
                           .N, by=.(Treatment, Unit, timestep, Species)]
setnames(dc_snagcreate, "N", "NumSnagCreate")
dc_snagfall <- dc_out_as[state_type=="SnFallNext"|state_type=="SnagCreate&Fall", 
                         .N, by=.(Treatment, Unit, timestep, Species)]
setnames(dc_snagfall, "N", "NumSnagFall")

dc_ad_sn_sp <- merge(dc_adult, dc_snag, by=c("Treatment","Unit","timestep","Species"),
                     all = TRUE)
dc_ad_sn_sp <- merge(dc_ad_sn_sp, dc_snagcreate, by=c("Treatment","Unit","timestep","Species"),
                     all = TRUE)
dc_ad_sn_sp <- merge(dc_ad_sn_sp, dc_snagfall, by=c("Treatment","Unit","timestep","Species"),
                     all = TRUE)
dc_ad_sn_sp <- dc_ad_sn_sp[, lapply(.SD, function(x) ifelse(is.na(x), 0, x))]
dc_ad_sn_sp

dc_ad_sn_sp <- dc_ad_sn_sp[,.(NumAdult, NumSnagCreate, NumExistSnags, NumSnagFall,
                              SnagRecrRate = ifelse(NumSnagCreate == 0,0,
                                                    (NumSnagCreate/(NumAdult+NumSnagCreate))*100), 
                              SnagFallRate = ifelse(NumSnagFall == 0,0, 
                                                    (NumSnagFall/(NumExistSnags + NumSnagCreate+
                                                                    NumSnagFall))*100)),
                           by=c("Treatment","Unit","timestep","Species")]


#3. how long are snags standing
#of the snags that fall, how long were they standing? So first figure out which trees fall
fallIDs <- unique(dc_out_as[state_type == "SnFallNext"|state_type == "SnagCreate&Fall"]$UniqXY)

#then count how many years they stood as snags
for(i in 1:length(fallIDs)){
  dc_out_as[UniqXY == fallIDs[i] & Type == "Snag", TimeAsSnag := .N]
}

dc_snagTime_sp <- dc_out_as[UniqXY %in% fallIDs & state_type == "SnFallNext"|
                              UniqXY %in% fallIDs & state_type == "SnagCreate&Fall", 
                      .(Treatment,Unit,Species,DBH,UniqXY,TimeAsSnag)]

dc_snagTime <- dc_out_as[UniqXY %in% fallIDs & state_type == "SnFallNext"|
                           UniqXY %in% fallIDs & state_type == "SnagCreate&Fall", 
                      .(Treatment,Unit,DBH,UniqXY,TimeAsSnag)]
dc_snagTime[, study := "DateCreek"]
dc_snagTime_sp[,study := "DateCreek"]


# Figures: --------------------------------------------------
snag_time <- rbind(sl_snag_time,dc_snagTime)
snag_time[,.N, by = .(Treatment)]

sl_snag_time[, Treatment := factor(Treatment, levels = c("light/no", "med", "heavy"))]
dc_snagTime[, Treatment := factor(Treatment, levels = c("NH","LR", "HR", "CC"))]

theme_set(theme_minimal(base_family = "Arial") +  # Change "Arial" to your desired font
            theme(
              text = element_text(family = "Arial"),  # Change "Arial" to your desired font
              plot.title = element_text(size = 14, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 14, face = "bold"),
              legend.text = element_text(size = 14, face = "bold"),
              strip.text = element_text(size = 14, face = "bold"),
              legend.title = element_text(size = 14, face = "bold")
            ))

# Snag longevity ---------
ggplot()+
  geom_boxplot(aes(y = TimeAsSnag, x = Treatment, fill = as.character(Treatment)),
               data = sl_snag_time)+
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("Low retention", "Medium retention", "High retention")
  ) +
  scale_x_discrete(
    labels = c("heavy" = "Low retention", "med" = "Medium retention", "light/no" = "High retention")
  ) +
  ylim(c(0,100))+
  coord_cartesian() +
  labs(y = "Snag longevity (Years)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  theme(legend.position = "none")
ggsave(filename = "SBS_snag_long.png",width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)

ggplot()+
  geom_boxplot(aes(y = TimeAsSnag, x = Treatment, fill = as.character(Treatment)),
               data = dc_snagTime)+
  theme_minimal()+
  scale_fill_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  scale_x_discrete(
    labels = c("NH" = "No harvest", "LR" = "High retention", 
               "HR" = "Medium retention", "CC" = "No retention")
  ) +
  ylim(c(0,100))+
  coord_cartesian() +
  labs(y = "Snag longevity (Years)",
       col = "Treatment",
       fill = "Treatment",
       shape = "Treatment"
  ) +
  theme(legend.position = "none",
        text = element_text(family = "Arial"),  # Change "Arial" to your desired font
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold")
        )
ggsave(filename = "ICH_snag_long.png",width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)


#Snag recruitment -------------------------------------
ggplot(sl_ad_sn)+
  geom_point(aes(x = timestep, y = SnagRecrRate, colour = Treatment))+
  geom_smooth(aes(x = timestep, y = SnagRecrRate, colour = Treatment, 
                  fill = Treatment),size=1.5)+
  theme_minimal()+
  scale_color_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  labs(y = "Snag Recruitment Rate (%)",
       x = "time since harvest",
       col = "Treatment",
       fill = "Treatment",
       shape = "Treatment"
  ) +
  ylim(c(0,5))+
  theme(legend.position = "bottom",
        text = element_text(family = "Arial"),  # Change "Arial" to your desired font
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold")
  )
ggsave(filename = "SBS_snag_recruit.png",width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)


ggplot(dc_ad_sn)+
  geom_point(aes(x = timestep, y = SnagRecrRate, colour = Treatment))+
  geom_smooth(aes(x = timestep, y = SnagRecrRate, colour = Treatment, 
                  fill = Treatment),size=1.5)+
  theme_minimal()+
  scale_fill_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  labs(y = "Snag Recruitment Rate (%)",
       x = "time since harvest",
       col = "Treatment",
       fill = "Treatment",
       shape = "Treatment"
  ) +
  ylim(c(0,5))+
  theme(legend.position = "bottom",
        text = element_text(family = "Arial"),  # Change "Arial" to your desired font
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold")
  )
ggsave(filename = "ICH_snag_recruit.png",width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)


# Snag fall rate -------------------------
ggplot(sl_ad_sn)+
  geom_point(aes(x = timestep, y = SnagFallRate, colour = Treatment))+
  geom_smooth(aes(x = timestep, y = SnagFallRate, colour = Treatment, 
                  fill = Treatment),size=1.5)+
  theme_minimal()+
  scale_color_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  ylim(c(0,50))+
  labs(y = "Snag Fall Rate (%)",
       x = "time since harvest",
       col = "Treatment",
       fill = "Treatment",
       shape = "Treatment"
  ) +
  theme(legend.position = "bottom",
        text = element_text(family = "Arial"),  # Change "Arial" to your desired font
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold")
  )
ggsave(filename = "SBS_snag_fall rate.png",width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)


ggplot(dc_ad_sn)+
  geom_point(aes(x = timestep, y = SnagFallRate, colour = Treatment))+
  geom_smooth(aes(x = timestep, y = SnagFallRate, colour = Treatment, 
                  fill = Treatment),size=1.5)+
  theme_minimal()+
  scale_fill_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  labs(y = "Snag Fall Rate (%)",
       x = "time since harvest",
       col = "Treatment",
       fill = "Treatment",
       shape = "Treatment"
  ) +
  ylim(c(0,50))+
  theme(legend.position = "bottom",
        text = element_text(family = "Arial"),  # Change "Arial" to your desired font
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold")
  )
ggsave(filename = "ICH_snag_fall rate.png",
       path = file.path(out_path), device='png', dpi=1200)


# Figures by species --------------------------------------------------
snag_time_sp <- rbind(sl_snagTime_sp,dc_snagTime_sp)
snag_time_sp[,.N, by = .(Treatment)]

sl_snagTime_sp[, Treatment := factor(Treatment, levels = c("light/no", "med", "heavy"))]
dc_snagTime_sp[, Treatment := factor(Treatment, levels = c("NH","LR", "HR", "CC"))]

theme_set(theme_minimal(base_family = "Arial") +  # Change "Arial" to your desired font
            theme(
              text = element_text(family = "Arial"),  # Change "Arial" to your desired font
              plot.title = element_text(size = 14, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 10, face = "bold"),
              legend.text = element_text(size = 14, face = "bold"),
              strip.text = element_text(size = 14, face = "bold"),
              legend.title = element_text(size = 14, face = "bold")
            ))

# Snag longevity ---------
ggplot()+
  geom_boxplot(aes(y = TimeAsSnag, x = Treatment, fill = as.character(Treatment)),
               data = sl_snagTime_sp[Species == "Interior_Spruce"|Species == "Subalpine_Fir"])+
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("Low retention", "Medium retention", "High retention")
  ) +
  scale_x_discrete(
    labels = c("heavy" = "Low retention", "med" = "Medium retention", "light/no" = "High retention")
  ) +
  ylim(c(0,100))+
  coord_cartesian() +
  labs(y = "Snag longevity (Years)",
       col = "Treatment",
       fill = "Treatment",
       shape = "Treatment"
  ) +
  facet_wrap(~recode(Species, 
                     "Interior_Spruce" = "Hybrid spruce",
                     "Subalpine_Fir" = "Subalpine fir"))+
  theme(legend.position = "none")
ggsave(filename = "SBS_snag_long_sp.png",width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)

ggplot() +
  geom_boxplot(aes(y = TimeAsSnag, x = Species, fill = Species),
               data = sl_snagTime_sp[Species %in% c("Interior_Spruce", "Subalpine_Fir")]) +
  scale_fill_manual(
    values = c(
      "Interior_Spruce" = "#6C4191",
      "Subalpine_Fir" = "#66BBBB"
    ),
    labels = c(
      "Interior_Spruce" = "Hybrid spruce",
      "Subalpine_Fir" = "Subalpine fir"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "Interior_Spruce" = "Hybrid spruce",
      "Subalpine_Fir" = "Subalpine fir"
    )
  ) +
  ylim(c(0, 100)) +
  coord_cartesian() +
  labs(y = "Snag longevity (Years)",
       x = "Species",
       fill = "Species") +
  facet_wrap(~Treatment, labeller = as_labeller(c(
    "light/no" = "High retention",
    "med" = "Medium retention",
    "heavy" = "Low retention"
  ))) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(family = "Arial"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14, face = "bold")
  )
ggsave(filename = "SBS_snag_long_sp_opt2.png",width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)


sp_incl <- c("Western_Hemlock","Western_redcedar","Hybrid_spruce","Amabalis_Fir",
             "Lodgepole_Pine")
ggplot()+
  geom_boxplot(aes(y = TimeAsSnag, x = Treatment, fill = as.character(Treatment)),
               data = dc_snagTime_sp[Species %in% sp_incl])+
  theme_minimal()+
  scale_fill_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  scale_x_discrete(
    labels = c("NH" = "No harvest", "LR" = "High retention", 
               "HR" = "Medium retention", "CC" = "No retention")
  ) +
  ylim(c(0,100))+
  coord_cartesian() +
  labs(y = "Snag longevity (Years)",
       col = "Treatment",
       fill = "Treatment",
       shape = "Treatment"
  ) +
 facet_wrap(c("Species"), labeller = as_labeller(c("Amabalis_Fir" = "Amabilis fir",
                                                  "Western_redcedar" = "Western cedar",
                                                  "Lodgepole_Pine" = "Lodgepole pine",
                                                  "Hybrid_spruce" = "Hybrid spruce",
                                                  "Western_Hemlock" = "Western hemlock")))+
  theme(legend.position = "none",
        text = element_text(family = "Arial"),  # Change "Arial" to your desired font
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold")
  )
ggsave(filename = "ICH_snag_long_sp.png",width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)

ggplot() +
  geom_boxplot(aes(y = TimeAsSnag, x = Species, fill = Species),
               data = dc_snagTime_sp[Species %in% sp_incl]) +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "Amabalis_Fir" = "#F0C808",
      "Western_redcedar" = "#6C4191",
      "Lodgepole_Pine" = "#66BBBB",
      "Hybrid_spruce" = "#DD4444",
      "Western_Hemlock" = "#228B22"
    ),
    labels = c(
      "Amabalis_Fir" = "Amabilis fir",
      "Western_redcedar" = "Western cedar",
      "Lodgepole_Pine" = "Lodgepole pine",
      "Hybrid_spruce" = "Hybrid spruce",
      "Western_Hemlock" = "Western hemlock"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "Amabalis_Fir" = "Amabilis fir",
      "Western_redcedar" = "Western cedar",
      "Lodgepole_Pine" = "Lodgepole pine",
      "Hybrid_spruce" = "Hybrid spruce",
      "Western_Hemlock" = "Western hemlock"
    )
  ) +
  ylim(c(0, 100)) +
  coord_cartesian() +
  labs(y = "Snag longevity (Years)",
       x = "Species",
       fill = "Species") +
  facet_wrap(~Treatment, labeller = as_labeller(c(
    "NH" = "No harvest",
    "LR" = "High retention",
    "HR" = "Medium retention",
    "CC" = "No retention"
  ))) +
  theme(
    legend.position = "nons",
    text = element_text(family = "Arial"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14, face = "bold")
  )
ggsave(filename = "ICH_snag_long_sp_opt2.png",width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)

#Snag recruitment -------------------------------------
ggplot(sl_ad_sn_sp[Species == "Interior_Spruce"|Species == "Subalpine_Fir"])+
  geom_point(aes(x = timestep, y = SnagRecrRate, colour = Treatment))+
  geom_smooth(aes(x = timestep, y = SnagRecrRate, colour = Treatment, 
                  fill = Treatment),size=1.5)+
  theme_minimal()+
  scale_color_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  labs(y = "Snag Recruitment Rate (%)",
       x = "time since harvest",
       col = "Treatment",
       fill = "Treatment",
       shape = "Treatment"
  ) +
  ylim(c(0,2))+
  facet_wrap(~recode(Species, 
                     "Interior_Spruce" = "Hybrid spruce",
                     "Subalpine_Fir" = "Subalpine fir"))+
  theme(legend.position = "bottom",
        text = element_text(family = "Arial"),  # Change "Arial" to your desired font
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold")
  )
ggsave(filename = "SBS_snag_recruit_sp.png",width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)

sp_incl <- c("Western_Hemlock","Western_redcedar","Hybrid_spruce","Amabalis_Fir",
             "Lodgepole_Pine")
ggplot(dc_ad_sn_sp[Species %in% sp_incl])+
  geom_point(aes(x = timestep, y = SnagRecrRate, colour = Treatment))+
  geom_smooth(aes(x = timestep, y = SnagRecrRate, colour = Treatment, 
                  fill = Treatment),size=1.5)+
  theme_minimal()+
  scale_fill_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  labs(y = "Snag Recruitment Rate (%)",
       x = "time since harvest",
       col = "Treatment",
       fill = "Treatment",
       shape = "Treatment"
  ) +
  facet_wrap(c("Species"), labeller = as_labeller(c("Amabalis_Fir" = "Amabilis fir",
                                                    "Western_redcedar" = "Western cedar",
                                                    "Lodgepole_Pine" = "Lodgepole pine",
                                                    "Hybrid_spruce" = "Hybrid spruce",
                                                    "Western_Hemlock" = "Western hemlock")))+
  ylim(c(0,100))+
  theme(legend.position = "bottom",
        text = element_text(family = "Arial"),  # Change "Arial" to your desired font
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold")
  )
ggsave(filename = "ICH_snag_recruit_sp.png",width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)


# Snag fall rate -------------------------
ggplot(sl_ad_sn_sp[Species %in% c("Interior_Spruce", "Subalpine_Fir")]) +
  geom_point(aes(x = timestep, y = SnagFallRate, colour = Treatment)) +
  geom_smooth(aes(x = timestep, y = SnagFallRate, colour = Treatment, 
                  fill = Treatment), size = 1.5) +
  theme_minimal() +
  scale_color_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  ylim(c(0, 100)) +
  labs(y = "Snag Fall Rate (%)",
       x = "time since harvest",
       col = "Treatment",
       fill = "Treatment",
       shape = "Treatment") +
  facet_wrap(~recode(Species, 
                     "Interior_Spruce" = "Hybrid spruce",
                     "Subalpine_Fir" = "Subalpine fir")) +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Arial"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14, face = "bold")
  )
ggsave(filename = "SBS_snag_fall rate.png",width = 7.91, height = 5.61,
       path = file.path(out_path), device='png', dpi=1200)


ggplot(dc_ad_sn_sp[Species %in% c("Amabalis_Fir", "Western_redcedar", "Lodgepole_Pine", "Hybrid_spruce", "Western_Hemlock")]) +
  geom_point(aes(x = timestep, y = SnagFallRate, colour = Treatment)) +
  geom_smooth(aes(x = timestep, y = SnagFallRate, colour = Treatment, 
                  fill = Treatment), size = 1.5) +
  theme_minimal() +
  scale_fill_manual(
    values = c("#F0C808", "#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH", "LR", "HR", "CC"),
    labels = c("No harvest", "High retention", "Medium retention", "No retention")
  ) +
  scale_color_manual(
    values = c("#F0C808", "#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH", "LR", "HR", "CC"),
    labels = c("No harvest", "High retention", "Medium retention", "No retention")
  ) +
  labs(
    y = "Snag Fall Rate (%)",
    x = "time since harvest",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  ylim(c(0, 100)) +
  facet_wrap(~recode(Species,
                     "Amabalis_Fir" = "Amabilis fir",
                     "Western_redcedar" = "Western cedar",
                     "Lodgepole_Pine" = "Lodgepole pine",
                     "Hybrid_spruce" = "Hybrid spruce",
                     "Western_Hemlock" = "Western hemlock")) +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Arial"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14, face = "bold")
  )
ggsave(filename = "ICH_snag_fall rate_sp.png",
       path = file.path(out_path), device='png', dpi=1200)





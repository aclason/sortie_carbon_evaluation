# Erica Lilles

#to visualize snag dynamics from sortie runs
library(data.table)
library(ggplot2)
library(patchwork)
library(dplyr)

in_path <- file.path("03_out_sortie")
out_path <- file.path("05_out_analysis")

sl_snagTime_sp <- readRDS(file.path(in_path, "sl_snagTime_sp.rds"))
dc_snagTime_sp <- readRDS(file.path(in_path, "dc_snagTime_sp.rds"))

#------------------------------------------------------------------------------------------

##Erica section of code

sl_snagTime_sp$SizeClass <-
  ifelse(sl_snagTime_sp$DBH < 25, "Small", "Large")

dc_snagTime_sp$SizeClass <-
  ifelse(dc_snagTime_sp$DBH < 25, "Small", "Large")



#Summit Lake data Treatment wasn't a factor and getting plotted in different order than date creek, so adjusting this



sl_snagTime_sp$Treatment <- as.factor(sl_snagTime_sp$Treatment)

sl_snagTime_sp$Treatment <-
  factor(sl_snagTime_sp$Treatment, levels = c("light/no", "med", "heavy"))



#SUMMIT LAKE FIGURES

#Figure showing overlap in snag longevity is nearly perfect among treatments - not included

ggplot(aes(
  x = TimeAsSnag,
  group =  c(Treatment),
  fill = as.character(Treatment)
),

data = sl_snagTime_sp[Species == "Interior_Spruce" |
                        Species == "Subalpine_Fir"]) +
  
  geom_density(adjust = 1.5) +
  
  scale_x_log10() +
  
  #scale_x_continuous(trans="log10")+
  
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("light/no", "med", "heavy"),
    
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  
  facet_wrap(
    ~ recode(
      Species,
      
      "Interior_Spruce" = "Hybrid spruce",
      
      "Subalpine_Fir" = "Subalpine fir"
    ) + SizeClass,
    ncol = 4
  ) +
  
  geom_segment(data = sl_snagTime_sp[Species == "Interior_Spruce" |
                                       Species == "Subalpine_Fir"],
               aes(
                 y = 0,
                 yend = .05,
                 x = mean(TimeAsSnag),
                 xend = mean(TimeAsSnag)
               ),
               lty = "21") +
  
  coord_flip() +
  
  scale_y_reverse() +
  
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )



#####

#### large and small opposite axes

####

p_Sx_Sm <-
  ggplot(aes(x = TimeAsSnag, group =  Treatment, fill = Treatment),
         
         data = sl_snagTime_sp[Species == "Interior_Spruce" &
                                 SizeClass == "Small"]) +
  
  geom_density(adjust = 1.5, alpha = 0.6) +
  
  scale_x_log10(limits = c(1, 80), breaks = c(1, 3, 10, 30, 80)) +
  
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("light/no", "med", "heavy"),
    
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  
  geom_segment(data = sl_snagTime_sp[Species == "Interior_Spruce" &
                                       SizeClass == "Small"],
               
               aes(
                 y = 0,
                 yend = 1,
                 x = mean(TimeAsSnag),
                 xend = mean(TimeAsSnag)
               ),
               lty = 2) +
  
  geom_segment(data = sl_snagTime_sp[Species == "Interior_Spruce" &
                                       SizeClass == "Small"],
               
               aes(
                 y = 0,
                 yend = 0.65,
                 x = mean(TimeAsSnag) + sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) + sd(TimeAsSnag)
               ),
               lty = "21") +
  
  geom_segment(data = sl_snagTime_sp[Species == "Interior_Spruce" &
                                       SizeClass == "Small"],
               
               aes(
                 y = 0,
                 yend = 0.2,
                 x = mean(TimeAsSnag) - sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) - sd(TimeAsSnag)
               ),
               lty = "21") +
  
  coord_flip(ylim = c(1.2, 0)) +
  
  scale_y_reverse() +
  
  labs(x = "Snag longevity (years)", y = "DBH < 25") +
  
  theme(
    legend.position = "bottom",
    legend.justification.bottom = "left",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    
    legend.text = element_text(size = 12, face = "bold"),
    
    legend.title = element_text(size = 12, face = "bold")
  )





p_Sx_Lg <-
  ggplot(aes(x = TimeAsSnag, group =  Treatment, fill = Treatment),
         
         data = sl_snagTime_sp[Species == "Interior_Spruce" &
                                 SizeClass == "Large"]) +
  
  geom_density(adjust = 1.5, alpha = 0.6) +
  
  scale_x_log10(limits = c(1, 80), breaks = c(1, 3, 10, 30, 80)) +
  
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("light/no", "med", "heavy"),
    
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  
  geom_segment(data = sl_snagTime_sp[Species == "Interior_Spruce" &
                                       SizeClass == "Large"],
               
               aes(
                 y = 0,
                 yend = 1,
                 x = mean(TimeAsSnag),
                 xend = mean(TimeAsSnag)
               ),
               lty = 2) +
  
  geom_segment(data = sl_snagTime_sp[Species == "Interior_Spruce" &
                                       SizeClass == "Large"],
               
               aes(
                 y = 0,
                 yend = 0.75,
                 x = mean(TimeAsSnag) + sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) + sd(TimeAsSnag)
               ),
               lty = "21") +
  
  geom_segment(data = sl_snagTime_sp[Species == "Interior_Spruce" &
                                       SizeClass == "Large"],
               
               aes(
                 y = 0,
                 yend = 0.4,
                 x = mean(TimeAsSnag) - sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) - sd(TimeAsSnag)
               ),
               lty = "21") +
  
  coord_flip(ylim = c(0, 1.2)) +
  
  #scale_y_reverse()+
  
  ggtitle('Interior Spruce') +
  
  labs(y = "DBH >25") +
  
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )





p_Bl_Sm <-
  ggplot(aes(x = TimeAsSnag, group =  Treatment, fill = Treatment),
         
         data = sl_snagTime_sp[Species == "Subalpine_Fir" &
                                 SizeClass == "Small"]) +
  
  geom_density(adjust = 1.5, alpha = 0.6) +
  
  scale_x_log10(limits = c(1, 80), breaks = c(1, 3, 10, 30, 80)) +
  
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("light/no", "med", "heavy"),
    
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  
  geom_segment(data = sl_snagTime_sp[Species == "Subalpine_Fir" &
                                       SizeClass == "Small"],
               
               aes(
                 y = 0,
                 yend = 1,
                 x = mean(TimeAsSnag),
                 xend = mean(TimeAsSnag)
               ),
               lty = 2) +
  
  geom_segment(data = sl_snagTime_sp[Species == "Subalpine_Fir" &
                                       SizeClass == "Small"],
               
               aes(
                 y = 0,
                 yend = 0.75,
                 x = mean(TimeAsSnag) + sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) + sd(TimeAsSnag)
               ),
               lty = "21") +
  
  geom_segment(data = sl_snagTime_sp[Species == "Subalpine_Fir" &
                                       SizeClass == "Small"],
               
               aes(
                 y = 0,
                 yend = 0.4,
                 x = mean(TimeAsSnag) - sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) - sd(TimeAsSnag)
               ),
               lty = "21") +
  
  coord_flip(ylim = c(1.2, 0)) +
  
  scale_y_reverse() +
  
  labs(y = "DBH < 25") +
  
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )



p_Bl_Lg <-
  ggplot(aes(x = TimeAsSnag, group =  Treatment, fill = Treatment),
         
         data = sl_snagTime_sp[Species == "Subalpine_Fir" &
                                 SizeClass == "Large"]) +
  
  geom_density(adjust = 1.5, alpha = 0.6) +
  
  ggtitle('Subalpine Fir') +
  
  scale_x_log10(limits = c(1, 80), breaks = c(1, 3, 10, 30, 80)) +
  
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("light/no", "med", "heavy"),
    
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  
  geom_segment(data = sl_snagTime_sp[Species == "Subalpine_Fir" &
                                       SizeClass == "Large"],
               
               aes(
                 y = 0,
                 yend = 1,
                 x = mean(TimeAsSnag),
                 xend = mean(TimeAsSnag)
               ),
               lty = 2) +
  
  geom_segment(data = sl_snagTime_sp[Species == "Subalpine_Fir" &
                                       SizeClass == "Large"],
               
               aes(
                 y = 0,
                 yend = 0.75,
                 x = mean(TimeAsSnag) + sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) + sd(TimeAsSnag)
               ),
               lty = "21") +
  
  geom_segment(data = sl_snagTime_sp[Species == "Subalpine_Fir" &
                                       SizeClass == "Large"],
               
               aes(
                 y = 0,
                 yend = 0.4,
                 x = mean(TimeAsSnag) - sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) - sd(TimeAsSnag)
               ),
               lty = "21") +
  
  coord_flip(ylim = c(0, 1.2)) +
  
  #scale_y_reverse()+
  
  labs(y = "DBH >25") +
  
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )

snag_long <- p_Sx_Sm | p_Sx_Lg |  p_Bl_Sm |  p_Bl_Lg

snag_long

ggsave(
  filename = "SBS_snag_long_sp_size.png",
  width = 7.91,
  height = 5.61,
  
  path = file.path(out_path),
  device = 'png',
  dpi = 1200
)





#calculate snag longevity by snag decay class

#Snag decay class code is lost the year the snag is tagged to fall, so must find

#decay class from previous year

fallIDs_sl <-
  unique(sl_out_as[state_type == "SnFallNext" |
                     state_type == "SnagCreate&Fall"]$UniqXY)



decay_class_summary <-
  sl_out_as[UniqXY %in% fallIDs_sl & is.na(SnagDecayClass) == FALSE,
            
            .(Treatment,
              Unit,
              Species,
              DBH,
              UniqXY,
              SnagDecayClass,
              TimeAsSnag)] %>%
  
  group_by(UniqXY) %>% summarise(DecayClass_max = max(SnagDecayClass, na.rm = TRUE))



sl_snagTime_sp_decay <-
  sl_out_as[UniqXY %in% fallIDs_sl & state_type == "SnFallNext" |
              
              UniqXY %in% fallIDs_sl &
              state_type == "SnagCreate&Fall",
            
            .(Treatment, Unit, Species, DBH, UniqXY, TimeAsSnag)]

sl_snagTime_sp_decay <-
  merge(sl_snagTime_sp_decay,
        decay_class_summary,
        by = "UniqXY",
        all.x = TRUE)



sl_snagTime_sp_decay$SizeClass <-
  ifelse(sl_snagTime_sp_decay$DBH < 25, "Small", "Large")



#Figure showing decay class differences in snag longevity - not included



ggplot(aes(
  x = TimeAsSnag,
  group =  c(DecayClass_max),
  fill = as.character(DecayClass_max)
),

data = sl_snagTime_sp_decay[Species == "Interior_Spruce" |
                              Species == "Subalpine_Fir"]) +
  
  geom_density(adjust = 1.5, alpha = 0.8) +
  
  scale_x_log10(limits = c(1, 80), breaks = c(1, 3, 10, 30, 80)) +
  
  facet_wrap(
    ~ recode(
      Species,
      
      "Interior_Spruce" = "Hybrid spruce",
      
      "Subalpine_Fir" = "Subalpine fir"
    ),
    ncol = 2
  ) +
  
  coord_flip() +
  
  scale_y_reverse() +
  
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )



#boxplot version - not included

ggplot() +
  
  geom_boxplot(aes(
    y = TimeAsSnag,
    x = DecayClass_max,
    fill = as.character(DecayClass_max)
  ),
  
  data = sl_snagTime_sp_decay[Species == "Interior_Spruce" |
                                Species == "Subalpine_Fir"]) +
  
  scale_x_log10(limits = c(1, 80), breaks = c(1, 3, 10, 30, 80)) +
  
  facet_wrap(
    ~ recode(
      Species,
      
      "Interior_Spruce" = "Hybrid spruce",
      
      "Subalpine_Fir" = "Subalpine fir"
    ),
    ncol = 2
  ) +
  
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )



#individual figures by species to add to overall figure

Sx_decay <- ggplot() +
  
  geom_boxplot(aes(
    y = TimeAsSnag,
    x = DecayClass_max,
    fill = as.character(DecayClass_max)
  ),
  
  data = sl_snagTime_sp_decay[Species == "Interior_Spruce"]) +
  
  scale_fill_manual(values = c("gray24", "gray47", "gray58", "gray80", "gray90")) +
  
  scale_y_log10(limits = c(1, 80), breaks = c(1, 3, 10, 30, 80)) +
  
  labs(x = "Decay class") +
  
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

Sx_decay



Bl_decay <- ggplot() +
  
  geom_boxplot(aes(
    y = TimeAsSnag,
    x = DecayClass_max,
    fill = as.character(DecayClass_max)
  ),
  
  data = sl_snagTime_sp_decay[Species == "Subalpine_Fir"]) +
  
  scale_fill_manual(values = c("gray24", "gray47", "gray58", "gray80", "gray90")) +
  
  scale_y_log10(limits = c(1, 80), breaks = c(1, 3, 10, 30, 80)) +
  
  labs(x = "Decay class") +
  
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

Bl_decay



patchwork2 <-
  p_Sx_Sm | p_Sx_Lg | Sx_decay |  p_Bl_Sm |  p_Bl_Lg | Bl_decay

patchwork2

ggsave(
  filename = "SBS_snag_long_sp_size_decay.png",
  width = 7.91,
  height = 5.61,
  
  path = file.path(out_path),
  device = 'png',
  dpi = 1200
)





#DATE CREEK FIGURES

#Figure showing overlap in snag longevity is nearly perfect among treatments - not included

ggplot(aes(
  x = TimeAsSnag,
  group =  c(Treatment),
  fill = as.character(Treatment)
),

data = dc_snagTime_sp[Species == "Western_Hemlock" |
                        Species == "Western_redcedar"]) +
  
  geom_density(adjust = 1.5) +
  
  scale_x_log10() +
  
  #scale_x_continuous(trans="log10")+
  
  #scale_fill_manual(
  
  #  values = c("#6C4191", "#66BBBB", "#DD4444"),
  
  #  breaks = c("light/no", "med", "heavy"),
  
  #  labels = c("Low retention", "Medium retention", "High retention") ) +
  
facet_wrap(
  ~ recode(
    Species,
    
    "Western_Hemlock" = "Western hemlock",
    
    "Western_redcedar" = "Western redcedar"
  ) + SizeClass,
  ncol = 4
) +
  
  #geom_segment(data=sl_snagTime_sp[Species == "Interior_Spruce"|Species == "Subalpine_Fir"], aes(y=0,yend=.05, x=mean(TimeAsSnag), xend=mean(TimeAsSnag)), lty="21")+
  
  coord_flip() +
  
  scale_y_reverse() +
  
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )



#####

#### large and small opposite axes

####

p_Hw_Sm <-
  ggplot(aes(x = TimeAsSnag, group =  Treatment, fill = Treatment),
         
         data = dc_snagTime_sp[Species == "Western_Hemlock" &
                                 SizeClass == "Small"]) +
  
  geom_density(adjust = 1.5, alpha = 0.6) +
  
  scale_x_log10(limits = c(1, 110), breaks = c(1, 3, 10, 30, 110)) +
  
  scale_fill_manual(
    values = c("#F0C808", "#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("NH", "LR", "HR", "CC"),
    
    labels = c(
      "No harvest",
      "High retention",
      "Medium retention",
      "No retention"
    )
  ) +
  
  geom_segment(data = dc_snagTime_sp[Species == "Western_Hemlock" &
                                       SizeClass == "Small"],
               
               aes(
                 y = 0,
                 yend = 1.2,
                 x = mean(TimeAsSnag),
                 xend = mean(TimeAsSnag)
               ),
               lty = 2) +
  
  geom_segment(data = dc_snagTime_sp[Species == "Western_Hemlock" &
                                       SizeClass == "Small"],
               
               aes(
                 y = 0,
                 yend = 0.65,
                 x = mean(TimeAsSnag) + sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) + sd(TimeAsSnag)
               ),
               lty = "21") +
  
  geom_segment(data = dc_snagTime_sp[Species == "Western_Hemlock" &
                                       SizeClass == "Small"],
               
               aes(
                 y = 0,
                 yend = 0.4,
                 x = mean(TimeAsSnag) - sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) - sd(TimeAsSnag)
               ),
               lty = "21") +
  
  coord_flip(ylim = c(1.2, 0)) +
  
  scale_y_reverse() +
  
  labs(x = "Snag longevity (years)", y = "DBH < 25") +
  
  theme(
    legend.position = "bottom",
    legend.justification.bottom = "left",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    
    legend.text = element_text(size = 12, face = "bold"),
    
    legend.title = element_text(size = 12, face = "bold")
  )

p_Hw_Sm



p_Hw_Lg <-
  ggplot(aes(x = TimeAsSnag, group =  Treatment, fill = Treatment),
         
         data = dc_snagTime_sp[Species == "Western_Hemlock" &
                                 SizeClass == "Large"]) +
  
  geom_density(adjust = 1.5, alpha = 0.6) +
  
  scale_x_log10(limits = c(1, 80), breaks = c(1, 3, 10, 30, 80)) +
  
  scale_fill_manual(
    values = c("#F0C808", "#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("NH", "LR", "HR", "CC"),
    
    labels = c(
      "No harvest",
      "High retention",
      "Medium retention",
      "No retention"
    )
  ) +
  
  geom_segment(data = dc_snagTime_sp[Species == "Western_Hemlock" &
                                       SizeClass == "Large"],
               
               aes(
                 y = 0,
                 yend = 1,
                 x = mean(TimeAsSnag),
                 xend = mean(TimeAsSnag)
               ),
               lty = 2) +
  
  geom_segment(data = dc_snagTime_sp[Species == "Western_Hemlock" &
                                       SizeClass == "Large"],
               
               aes(
                 y = 0,
                 yend = 0.7,
                 x = mean(TimeAsSnag) + sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) + sd(TimeAsSnag)
               ),
               lty = "21") +
  
  geom_segment(data = dc_snagTime_sp[Species == "Western_Hemlock" &
                                       SizeClass == "Large"],
               
               aes(
                 y = 0,
                 yend = 0.35,
                 x = mean(TimeAsSnag) - sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) - sd(TimeAsSnag)
               ),
               lty = "21") +
  
  coord_flip(ylim = c(0, 1.2)) +
  
  #scale_y_reverse()+
  
  ggtitle('Western hemlock') +
  
  labs(y = "DBH >25") +
  
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )

p_Hw_Lg



p_Cw_Sm <-
  ggplot(aes(x = TimeAsSnag, group =  Treatment, fill = Treatment),
         
         data = dc_snagTime_sp[Species == "Western_redcedar" &
                                 SizeClass == "Small"]) +
  
  geom_density(adjust = 1.5, alpha = 0.6) +
  
  scale_x_log10(limits = c(1, 80), breaks = c(1, 3, 10, 30, 80)) +
  
  scale_fill_manual(
    values = c("#F0C808", "#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("NH", "LR", "HR", "CC"),
    
    labels = c(
      "No harvest",
      "High retention",
      "Medium retention",
      "No retention"
    )
  ) +
  
  geom_segment(data = dc_snagTime_sp[Species == "Western_redcedar" &
                                       SizeClass == "Small"],
               
               aes(
                 y = 0,
                 yend = 1,
                 x = mean(TimeAsSnag),
                 xend = mean(TimeAsSnag)
               ),
               lty = 2) +
  
  geom_segment(data = dc_snagTime_sp[Species == "Western_redcedar" &
                                       SizeClass == "Small"],
               
               aes(
                 y = 0,
                 yend = 0.75,
                 x = mean(TimeAsSnag) + sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) + sd(TimeAsSnag)
               ),
               lty = "21") +
  
  geom_segment(data = dc_snagTime_sp[Species == "Western_redcedar" &
                                       SizeClass == "Small"],
               
               aes(
                 y = 0,
                 yend = 0.5,
                 x = mean(TimeAsSnag) - sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) - sd(TimeAsSnag)
               ),
               lty = "21") +
  
  coord_flip(ylim = c(1.2, 0)) +
  
  scale_y_reverse() +
  
  labs(y = "DBH < 25") +
  
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

p_Cw_Sm



p_Cw_Lg <-
  ggplot(aes(x = TimeAsSnag, group =  Treatment, fill = Treatment),
         
         data = dc_snagTime_sp[Species == "Western_redcedar" &
                                 SizeClass == "Large"]) +
  
  geom_density(adjust = 1.5, alpha = 0.6) +
  
  ggtitle('Western redcedar') +
  
  scale_x_log10(limits = c(1, 80), breaks = c(1, 3, 10, 30, 80)) +
  
  scale_fill_manual(
    values = c("#F0C808", "#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("NH", "LR", "HR", "CC"),
    
    labels = c(
      "No harvest",
      "High retention",
      "Medium retention",
      "No retention"
    )
  ) +
  
  geom_segment(data = dc_snagTime_sp[Species == "Western_redcedar" &
                                       SizeClass == "Large"],
               
               aes(
                 y = 0,
                 yend = 1,
                 x = mean(TimeAsSnag),
                 xend = mean(TimeAsSnag)
               ),
               lty = 2) +
  
  geom_segment(data = dc_snagTime_sp[Species == "Western_redcedar" &
                                       SizeClass == "Large"],
               
               aes(
                 y = 0,
                 yend = 0.75,
                 x = mean(TimeAsSnag) + sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) + sd(TimeAsSnag)
               ),
               lty = "21") +
  
  geom_segment(data = dc_snagTime_sp[Species == "Western_redcedar" &
                                       SizeClass == "Large"],
               
               aes(
                 y = 0,
                 yend = 0.4,
                 x = mean(TimeAsSnag) - sd(TimeAsSnag),
                 xend = mean(TimeAsSnag) - sd(TimeAsSnag)
               ),
               lty = "21") +
  
  coord_flip(ylim = c(0, 1.2)) +
  
  #scale_y_reverse()+
  
  labs(y = "DBH >25") +
  
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )

p_Cw_Lg





#calculate snag longevity by snag decay class

#Snag decay class code is lost the year the snag is tagged to fall, so must find

#decay class from previous year

fallIDs_dc <-
  unique(dc_out_as[state_type == "SnFallNext" |
                     state_type == "SnagCreate&Fall"]$UniqXY)



decay_class_summary_dc <-
  dc_out_as[UniqXY %in% fallIDs_dc & is.na(SnagDecayClass) == FALSE,
            
            .(Treatment,
              Unit,
              Species,
              DBH,
              UniqXY,
              SnagDecayClass,
              TimeAsSnag)] %>%
  
  group_by(UniqXY) %>% summarise(DecayClass_max = max(SnagDecayClass, na.rm = TRUE))





dc_snagTime_sp_decay <-
  dc_out_as[UniqXY %in% fallIDs_dc & state_type == "SnFallNext" |
              
              UniqXY %in% fallIDs_dc &
              state_type == "SnagCreate&Fall",
            
            .(Treatment, Unit, Species, DBH, UniqXY, TimeAsSnag)]

dc_snagTime_sp_decay <-
  merge(dc_snagTime_sp_decay,
        decay_class_summary_dc,
        by = "UniqXY",
        all.x = TRUE)



dc_snagTime_sp_decay$SizeClass <-
  ifelse(dc_snagTime_sp_decay$DBH < 25, "Small", "Large")



#Figure showing decay class differences in snag longevity - boxplot version

#individual figures by species to add to overall figure

Hw_decay <- ggplot() +
  
  geom_boxplot(aes(
    y = TimeAsSnag,
    x = DecayClass_max,
    fill = as.character(DecayClass_max)
  ),
  
  data = dc_snagTime_sp_decay[Species == "Western_Hemlock"]) +
  
  scale_fill_manual(values = c("gray24", "gray47", "gray58", "gray80", "gray90")) +
  
  scale_y_log10(limits = c(1, 110), breaks = c(1, 3, 10, 30, 110)) +
  
  labs(x = "Decay class") +
  
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

Hw_decay



Cw_decay <- ggplot() +
  
  geom_boxplot(aes(
    y = TimeAsSnag,
    x = DecayClass_max,
    fill = as.character(DecayClass_max)
  ),
  
  data = dc_snagTime_sp_decay[Species == "Western_redcedar"]) +
  
  scale_fill_manual(values = c("gray24", "gray47", "gray58", "gray80", "gray90")) +
  
  scale_y_log10(limits = c(1, 110), breaks = c(1, 3, 10, 30, 110)) +
  
  labs(x = "Decay class") +
  
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

Cw_decay



patchwork2 <-
  p_Hw_Sm | p_Hw_Lg | Hw_decay |  p_Cw_Sm |  p_Cw_Lg | Cw_decay

patchwork2

ggsave(
  filename = "ICH_snag_long_sp_size_decay.png",
  width = 7.91,
  height = 5.61,
  
  path = file.path(out_path),
  device = 'png',
  dpi = 1200
)





############

###############

##############



#4. Calculate Snag creation and snag fall rate - by Species and size class

#ICH

dc_out_as$SizeClass <- ifelse(dc_out_as$DBH < 25, "Small", "Large")

dc_adult.sc <-
  dc_out_as[state_type == "Adult", .N, by = .(Treatment, Unit, timestep, Species, SizeClass)]

setnames(dc_adult.sc, "N", "NumAdult")

dc_snag.sc <-
  dc_out_as[state_type == "Snag", .N, by = .(Treatment, Unit, timestep, Species, SizeClass)]

setnames(dc_snag.sc, "N", "NumExistSnags")

dc_snagcreate.sc <-
  dc_out_as[state_type == "SnagCreate" | state_type == "SnagCreate&Fall",
            
            .N, by = .(Treatment, Unit, timestep, Species, SizeClass)]

setnames(dc_snagcreate.sc, "N", "NumSnagCreate")

dc_snagfall.sc <-
  dc_out_as[state_type == "SnFallNext" | state_type == "SnagCreate&Fall",
            
            .N, by = .(Treatment, Unit, timestep, Species, SizeClass)]

setnames(dc_snagfall.sc, "N", "NumSnagFall")

dc_treefall.sc <- dc_out_as[state_type == "SnagCreate&Fall",
                            
                            .N, by = .(Treatment, Unit, timestep, Species, SizeClass)]

setnames(dc_treefall.sc, "N", "NumTreeFall")



dc_ad_sn_sp.sc <-
  merge(
    dc_adult.sc,
    dc_snag.sc,
    by = c("Treatment", "Unit", "timestep", "Species", "SizeClass"),
    
    all = TRUE
  )

dc_ad_sn_sp.sc <-
  merge(
    dc_ad_sn_sp.sc,
    dc_snagcreate.sc,
    by = c("Treatment", "Unit", "timestep", "Species", "SizeClass"),
    
    all = TRUE
  )

dc_ad_sn_sp.sc <-
  merge(
    dc_ad_sn_sp.sc,
    dc_snagfall.sc,
    by = c("Treatment", "Unit", "timestep", "Species", "SizeClass"),
    
    all = TRUE
  )

dc_ad_sn_sp.sc <-
  merge(
    dc_ad_sn_sp.sc,
    dc_treefall.sc,
    by = c("Treatment", "Unit", "timestep", "Species", "SizeClass"),
    
    all = TRUE
  )

dc_ad_sn_sp.sc <-
  dc_ad_sn_sp.sc[, lapply(.SD, function(x)
    ifelse(is.na(x), 0, x))]

dc_ad_sn_sp.sc



dc_ad_sn_sp.sc_calcs <-
  dc_ad_sn_sp.sc[, .(
    NumAdult,
    NumSnagCreate,
    NumExistSnags,
    NumSnagFall,
    NumTreeFall,
    
    SnagRecrRate = ifelse(NumSnagCreate == 0, 0,
                          
                          (NumSnagCreate /
                             (
                               NumAdult + NumSnagCreate
                             )) * 100),
    
    SnagFallRate = ifelse(NumSnagFall == 0, 0,
                          
                          (
                            NumSnagFall / (NumExistSnags + NumSnagCreate + NumSnagFall)
                          ) * 100),
    
    TreeFallRate = ifelse(NumTreeFall == 0, 0,
                          
                          (NumTreeFall /
                             (
                               NumAdult + NumSnagCreate
                             )) * 100)
  ),
  
  by = c("Treatment", "Unit", "timestep", "Species", "SizeClass")]



###Looking into tree fall

dc_treefall.sc #only aspen in year 8 and 10 in clear-cuts are codes as SnagCreate&Fall... these must be brushed

treefall_all <- dc_out_as[state_type == "SnagCreate&Fall"]

unique(treefall_all$Dead.Code)

unique(treefall_all$Treatment)

unique(treefall_all$Species)

unique(treefall_all$timestep)

range(treefall_all$DBH)





# Figures by species and size class--------------------------------------------------



#DATE CREEK SNAG RECRUITMENT



#changing scales between 3 groups - both size class

p1 <-
  ggplot(dc_ad_sn_sp.sc_calcs[Species %in% c("Western_Hemlock", "Western_redcedar", "Hybrid_spruce")]) +
  
  coord_cartesian(ylim = c(0, 1.5)) +
  
  geom_smooth(
    aes(
      x = timestep,
      y = SnagRecrRate,
      colour = Treatment,
      lty = SizeClass,
      
      fill = Treatment
    ),
    size = 1.5,
    alpha = 0.2
  ) +
  
  theme_minimal() +
  
  scale_fill_manual(
    values = c("#F0C808", "#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("NH", "LR", "HR", "CC"),
    
    labels = c(
      "No harvest",
      "High retention",
      "Medium retention",
      "No retention"
    )
  ) +
  
  scale_color_manual(
    values = c("#F0C808", "#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("NH", "LR", "HR", "CC"),
    
    labels = c(
      "No harvest",
      "High retention",
      "Medium retention",
      "No retention"
    )
  ) +
  
  labs(
    y = "Mortality (%)",
    
    x = "time since harvest",
    
    col = "Treatment",
    
    fill = "Treatment",
    
    shape = "Treatment"
  ) +
  
  facet_wrap(c("Species"), labeller = as_labeller(
    c(
      "Western_redcedar" = "Western cedar",
      
      "Hybrid_spruce" = "Hybrid spruce",
      
      "Western_Hemlock" = "Western hemlock"
    )
  )) +
  
  theme(
    legend.position = "none",
    
    text = element_text(family = "Arial"),
    # Change "Arial" to your desired font
    
    plot.title = element_text(size = 14, face = "bold"),
    
    axis.title.x = element_blank(),
    
    axis.text.x = element_blank(),
    
    axis.title.y = element_text(size = 14, face = "bold"),
    
    axis.text.y = element_text(size = 14, face = "bold"),
    
    legend.text = element_text(size = 14, face = "bold"),
    
    strip.text = element_text(size = 14, face = "bold"),
    
    legend.title = element_text(size = 14, face = "bold")
  )

p1



p2 <-
  ggplot(dc_ad_sn_sp.sc_calcs[Species %in% c("Amabalis_Fir", "Subalpine_Fir", "Lodgepole_Pine")]) +
  
  coord_cartesian(ylim = c(0, 7)) +
  
  geom_smooth(
    aes(
      x = timestep,
      y = SnagRecrRate,
      colour = Treatment,
      lty = SizeClass,
      
      fill = Treatment
    ),
    size = 1.5,
    alpha = 0.2
  ) +
  
  theme_minimal() +
  
  scale_fill_manual(
    values = c("#F0C808", "#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("NH", "LR", "HR", "CC"),
    
    labels = c(
      "No harvest",
      "High retention",
      "Medium retention",
      "No retention"
    )
  ) +
  
  scale_color_manual(
    values = c("#F0C808", "#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("NH", "LR", "HR", "CC"),
    
    labels = c(
      "No harvest",
      "High retention",
      "Medium retention",
      "No retention"
    )
  ) +
  
  labs(
    y = "Mortality (%)",
    
    x = "Time since harvest (years)",
    
    col = "Treatment",
    
    fill = "Treatment",
    
    shape = "Treatment"
  ) +
  
  facet_wrap(c("Species"), labeller = as_labeller(
    c(
      "Amabalis_Fir" = "Amabilis fir",
      
      "Lodgepole_Pine" = "Lodgepole pine",
      
      "Subalpine_Fir" = "Subalpine fir"
    )
  )) +
  
  theme(
    legend.position = "none",
    
    text = element_text(family = "Arial"),
    # Change "Arial" to your desired font
    
    plot.title = element_text(size = 14, face = "bold"),
    
    axis.title.x = element_blank(),
    
    axis.text.x = element_blank(),
    
    axis.title.y = element_text(size = 14, face = "bold"),
    
    axis.text.y = element_text(size = 14, face = "bold"),
    
    legend.text = element_text(size = 14, face = "bold"),
    
    strip.text = element_text(size = 14, face = "bold"),
    
    legend.title = element_text(size = 14, face = "bold")
  )

p2



#deciduous species don't have enough trees to separate by size class so going back to no size class dataset

p3 <-
  ggplot(dc_ad_sn_sp[Species %in% c("Black_Cottonwood", "Paper_Birch", "Trembling_Aspen")]) +
  
  coord_cartesian(ylim = c(0, 13)) +
  
  guides (lty = "none") +
  
  geom_smooth(
    aes(
      x = timestep,
      y = SnagRecrRate,
      colour = Treatment,
      
      fill = Treatment
    ),
    size = 1.5,
    alpha = 0.2
  ) +
  
  theme_minimal() +
  
  scale_fill_manual(
    values = c("#F0C808", "#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("NH", "LR", "HR", "CC"),
    
    labels = c(
      "No harvest",
      "High retention",
      "Medium retention",
      "No retention"
    )
  ) +
  
  scale_color_manual(
    values = c("#F0C808", "#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("NH", "LR", "HR", "CC"),
    
    labels = c(
      "No harvest",
      "High retention",
      "Medium retention",
      "No retention"
    )
  ) +
  
  labs(
    y = "Mortality (%)",
    
    x = "Time since harvest (years)",
    
    col = "Treatment",
    
    fill = "Treatment",
    
    shape = "Treatment"
  ) +
  
  facet_wrap(c("Species"), labeller = as_labeller(
    c(
      "Black_Cottonwood" = "Black cottonwood",
      
      "Paper_Birch" = "Paper birch",
      
      "Trembling_Aspen" = "Trembling aspen"
    )
  )) +
  
  theme(
    legend.position = "bottom",
    
    text = element_text(family = "Arial"),
    # Change "Arial" to your desired font
    
    plot.title = element_text(size = 14, face = "bold"),
    
    axis.title = element_text(size = 14, face = "bold"),
    
    axis.text = element_text(size = 14, face = "bold"),
    
    legend.text = element_text(size = 12, face = "bold"),
    
    strip.text = element_text(size = 14, face = "bold"),
    
    legend.title = element_text(size = 12, face = "bold")
  )

p3



p1 / p2 / p3



ggsave(
  filename = "ICH_snag_recruit_sp_sc.png",
  width = 7.91,
  height = 7,
  
  path = file.path(out_path),
  device = 'png',
  dpi = 1200
)





#SBS Snag creation and snag fall rate - by Species and size class

sl_out_as$SizeClass <- ifelse(sl_out_as$DBH < 25, "Small", "Large")

sl_adult.sc <-
  sl_out_as[state_type == "Adult", .N, by = .(Treatment, Unit, timestep, Species, SizeClass)]

setnames(sl_adult.sc, "N", "NumAdult")

sl_snag.sc <-
  sl_out_as[state_type == "Snag", .N, by = .(Treatment, Unit, timestep, Species, SizeClass)]

setnames(sl_snag.sc, "N", "NumExistSnags")

sl_snagcreate.sc <-
  sl_out_as[state_type == "SnagCreate" | state_type == "SnagCreate&Fall",
            
            .N, by = .(Treatment, Unit, timestep, Species, SizeClass)]

setnames(sl_snagcreate.sc, "N", "NumSnagCreate")

sl_snagfall.sc <-
  sl_out_as[state_type == "SnFallNext" | state_type == "SnagCreate&Fall",
            
            .N, by = .(Treatment, Unit, timestep, Species, SizeClass)]

setnames(sl_snagfall.sc, "N", "NumSnagFall")

sl_treefall.sc <- sl_out_as[state_type == "SnagCreate&Fall",
                            
                            .N, by = .(Treatment, Unit, timestep, Species, SizeClass)]

setnames(sl_treefall.sc, "N", "NumTreeFall")



sl_ad_sn_sp.sc <-
  merge(
    sl_adult.sc,
    sl_snag.sc,
    by = c("Treatment", "Unit", "timestep", "Species", "SizeClass"),
    
    all = TRUE
  )

sl_ad_sn_sp.sc <-
  merge(
    sl_ad_sn_sp.sc,
    sl_snagcreate.sc,
    by = c("Treatment", "Unit", "timestep", "Species", "SizeClass"),
    
    all = TRUE
  )

sl_ad_sn_sp.sc <-
  merge(
    sl_ad_sn_sp.sc,
    sl_snagfall.sc,
    by = c("Treatment", "Unit", "timestep", "Species", "SizeClass"),
    
    all = TRUE
  )

sl_ad_sn_sp.sc <-
  merge(
    sl_ad_sn_sp.sc,
    sl_treefall.sc,
    by = c("Treatment", "Unit", "timestep", "Species", "SizeClass"),
    
    all = TRUE
  )

sl_ad_sn_sp.sc <-
  sl_ad_sn_sp.sc[, lapply(.SD, function(x)
    ifelse(is.na(x), 0, x))]

sl_ad_sn_sp.sc



sl_ad_sn_sp.sc_calcs <-
  sl_ad_sn_sp.sc[, .(
    NumAdult,
    NumSnagCreate,
    NumExistSnags,
    NumSnagFall,
    NumTreeFall,
    
    SnagRecrRate = ifelse(NumSnagCreate == 0, 0,
                          
                          (NumSnagCreate /
                             (
                               NumAdult + NumSnagCreate
                             )) * 100),
    
    SnagFallRate = ifelse(NumSnagFall == 0, 0,
                          
                          (
                            NumSnagFall / (NumExistSnags + NumSnagCreate + NumSnagFall)
                          ) * 100),
    
    TreeFallRate = ifelse(NumTreeFall == 0, 0,
                          
                          (NumTreeFall /
                             (
                               NumAdult + NumSnagCreate
                             )) * 100)
  ),
  
  by = c("Treatment", "Unit", "timestep", "Species", "SizeClass")]



###Looking into tree fall

sl_treefall.sc #none for Summit Lake





# Figures by species and size class--------------------------------------------------



#SUMMIT LAKE SNAG RECRUITMENT



#changing scales between 3 groups - both size class

p1 <-
  ggplot(sl_ad_sn_sp.sc_calcs[Species %in% c("Subalpine_Fir", "Interior_Spruce")]) +
  
  coord_cartesian(ylim = c(0, 0.8)) +
  
  guides (lty = "none") +
  
  geom_smooth(
    aes(
      x = timestep,
      y = SnagRecrRate,
      colour = Treatment,
      lty = SizeClass,
      
      fill = Treatment
    ),
    size = 1.5,
    alpha = 0.2
  ) +
  
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
  
  labs(
    y = "Mortality (%)",
    
    x = "Time since harvest (years)",
    
    col = "Treatment",
    
    fill = "Treatment",
    
    shape = "Treatment"
  ) +
  
  facet_wrap(c("Species"), labeller = as_labeller(
    c("Subalpine_Fir" = "Subalpine fir",
      
      "Interior_Spruce" = "Interior spruce")
  )) +
  
  theme(
    legend.position = "bottom",
    
    text = element_text(family = "Arial"),
    # Change "Arial" to your desired font
    
    plot.title = element_text(size = 14, face = "bold"),
    
    axis.title = element_text(size = 14, face = "bold"),
    
    axis.text = element_text(size = 14, face = "bold"),
    
    legend.text = element_text(size = 14, face = "bold"),
    
    strip.text = element_text(size = 14, face = "bold"),
    
    legend.title = element_text(size = 14, face = "bold")
  )

p1



ggsave(
  filename = "SBS_snag_recruit_sp_sc.png",
  width = 7.91,
  height = 3,
  
  path = file.path(out_path),
  device = 'png',
  dpi = 1200
)

#SBS means and sd

mortality_summary <- sl_ad_sn_sp.sc_calcs %>%
  
  group_by(Species, Treatment, SizeClass) %>%
  
  summarise(
    SnagRecrRate_mean = mean(SnagRecrRate, na.rm = TRUE),
    
    SnagRecrRate_sd = sd(SnagRecrRate, na.rm = TRUE),
    
    SnagRecrRate_min = min(SnagRecrRate, na.rm = TRUE),
    
    SnagRecrRate_max = max(SnagRecrRate, na.rm = TRUE)
  )

mortality_summary



sl_ad_sn_sp.sc_calcs[timestep == 100] %>%
  
  group_by(Species, Treatment, SizeClass) %>%
  
  summarise(
    SnagRecrRate_mean = mean(SnagRecrRate, na.rm = TRUE),
    
    SnagRecrRate_sd = sd(SnagRecrRate, na.rm = TRUE),
    
    SnagRecrRate_min = min(SnagRecrRate, na.rm = TRUE),
    
    SnagRecrRate_max = max(SnagRecrRate, na.rm = TRUE)
  )



#ICH means and sd

mortality_summary <-
  dc_ad_sn_sp.sc_calcs[Species %in% sp_incl |
                         Species == "Subalpine_Fir"] %>%
  
  group_by(Species, Treatment, SizeClass) %>%
  
  summarise(
    SnagRecrRate_mean = mean(SnagRecrRate, na.rm = TRUE),
    
    SnagRecrRate_sd = sd(SnagRecrRate, na.rm = TRUE),
    
    SnagRecrRate_min = min(SnagRecrRate, na.rm = TRUE),
    
    SnagRecrRate_max = max(SnagRecrRate, na.rm = TRUE)
  )

mortality_summary



dc_ad_sn_sp.sc_calcs[timestep == 100] %>%
  
  group_by(Species, Treatment, SizeClass) %>%
  
  summarise(
    SnagRecrRate_mean = mean(SnagRecrRate, na.rm = TRUE),
    
    SnagRecrRate_sd = sd(SnagRecrRate, na.rm = TRUE),
    
    SnagRecrRate_min = min(SnagRecrRate, na.rm = TRUE),
    
    SnagRecrRate_max = max(SnagRecrRate, na.rm = TRUE)
  )



#Snag longevity stats for text

sl_snagTime_sp_decay[Species %in% c("Subalpine_Fir", "Interior_Spruce")] %>%
  
  group_by(Species, DecayClass_max) %>%
  
  summarise(
    SnagLongevity_mean = mean(TimeAsSnag, na.rm = TRUE),
    
    SnagLongevity_sd = sd(TimeAsSnag, na.rm = TRUE),
    
    SnagLongevity_max = max(TimeAsSnag, na.rm = TRUE)
  )



sl_snagTime_sp_decay$SizeClass <-
  ifelse(sl_snagTime_sp_decay$DBH < 25, "Small", "Large")

sl_snagTime_sp_decay[Species %in% c("Subalpine_Fir", "Interior_Spruce")] %>%
  
  group_by(Species, SizeClass) %>%
  
  summarise(
    SnagLongevity_mean = mean(TimeAsSnag, na.rm = TRUE),
    
    SnagLongevity_sd = sd(TimeAsSnag, na.rm = TRUE),
    
    SnagLongevity_max = max(TimeAsSnag, na.rm = TRUE)
  )



sl_snagTime_sp_decay[Species %in% c("Subalpine_Fir", "Interior_Spruce")] %>%
  
  group_by(SizeClass) %>%
  
  summarise(
    SnagLongevity_mean = mean(TimeAsSnag, na.rm = TRUE),
    
    SnagLongevity_sd = sd(TimeAsSnag, na.rm = TRUE),
    
    SnagLongevity_max = max(TimeAsSnag, na.rm = TRUE)
  )





dc_snagTime_sp_decay[Species %in% c("Western_Hemlock", "Western_redcedar")] %>%
  
  group_by(Species, DecayClass_max) %>%
  
  summarise(
    SnagLongevity_mean = mean(TimeAsSnag, na.rm = TRUE),
    
    SnagLongevity_sd = sd(TimeAsSnag, na.rm = TRUE),
    
    SnagLongevity_max = max(TimeAsSnag, na.rm = TRUE)
  )



dc_snagTime_sp_decay[Species %in% c("Western_Hemlock", "Western_redcedar")] %>%
  
  group_by(Species, SizeClass) %>%
  
  summarise(
    SnagLongevity_mean = mean(TimeAsSnag, na.rm = TRUE),
    
    SnagLongevity_sd = sd(TimeAsSnag, na.rm = TRUE),
    
    SnagLongevity_max = max(TimeAsSnag, na.rm = TRUE)
  )



dc_snagTime_sp_decay[Species %in% c("Western_Hemlock", "Western_redcedar")] %>%
  
  group_by(SizeClass) %>%
  
  summarise(
    SnagLongevity_mean = mean(TimeAsSnag, na.rm = TRUE),
    
    SnagLongevity_sd = sd(TimeAsSnag, na.rm = TRUE),
    
    SnagLongevity_max = max(TimeAsSnag, na.rm = TRUE)
  )





#Snag fall

# Snag fall rate -------------------------

#SBS

#rows with no snags have zero for snag fall but shouldn't be included in figures because

#there were no snags to fall

sl_snagfall_only <-
  sl_ad_sn_sp.sc_calcs[NumSnagCreate + NumExistSnags > 0]



ggplot(sl_snagfall_only[Species %in% c("Subalpine_Fir", "Interior_Spruce")]) +
  
  geom_smooth(
    aes(
      x = timestep,
      y = SnagFallRate,
      colour = Treatment,
      
      fill = Treatment,
      lty = SizeClass
    ),
    size = 1.5,
    alpha = 0.2
  ) +
  
  theme_minimal() +
  
  coord_cartesian(ylim = c(0, 15)) +
  
  facet_wrap(c("Species"), labeller = as_labeller(
    c("Subalpine_Fir" = "Subalpine fir",
      
      "Interior_Spruce" = "Interior spruce")
  )) +
  
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
  
  labs(
    y = "Snag Fall Rate (%)",
    
    x = "Time since harvest (years)",
    
    col = "Treatment",
    
    fill = "Treatment",
    
    shape = "Treatment"
  ) +
  
  theme(
    legend.position = "bottom",
    
    text = element_text(family = "Arial"),
    # Change "Arial" to your desired font
    
    plot.title = element_text(size = 14, face = "bold"),
    
    axis.title = element_text(size = 14, face = "bold"),
    
    axis.text = element_text(size = 14, face = "bold"),
    
    legend.text = element_text(size = 12, face = "bold"),
    
    strip.text = element_text(size = 14, face = "bold"),
    
    legend.title = element_text(size = 12, face = "bold")
  ) +
  
  guides (lty = "none")

ggsave(
  filename = "SBS_snag_fall rate by species and size class.png",
  width = 7.91,
  height = 4,
  
  path = file.path(out_path),
  device = 'png',
  dpi = 1200
)



#ICH

#rows with no snags have zero for snag fall but shouldn't be included in figures because

#there were no snags to fall

dc_snagfall_only <-
  dc_ad_sn_sp.sc_calcs[NumSnagCreate + NumExistSnags > 0]



ggplot(dc_snagfall_only[Species %in% c("Western_Hemlock", "Western_redcedar")]) +
  
  geom_smooth(
    aes(
      x = timestep,
      y = SnagFallRate,
      colour = Treatment,
      
      fill = Treatment,
      lty = SizeClass
    ),
    size = 1.5,
    alpha = 0.2
  ) +
  
  theme_minimal() +
  
  coord_cartesian(ylim = c(0, 15)) +
  
  facet_wrap(c("Species"), labeller = as_labeller(
    c("Western_Hemlock" = "Western hemlock",
      
      "Western_redcedar" = "Western redcedar")
  )) +
  
  scale_fill_manual(
    values = c("#F0C808", "#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("NH", "LR", "HR", "CC"),
    
    labels = c(
      "No harvest",
      "High retention",
      "Medium retention",
      "No retention"
    )
  ) +
  
  scale_color_manual(
    values = c("#F0C808", "#6C4191", "#66BBBB", "#DD4444"),
    
    breaks = c("NH", "LR", "HR", "CC"),
    
    labels = c(
      "No harvest",
      "High retention",
      "Medium retention",
      "No retention"
    )
  ) +
  
  labs(
    y = "Snag Fall Rate (%)",
    
    x = "Time since harvest (years)",
    
    col = "Treatment",
    
    fill = "Treatment",
    
    shape = "Treatment"
  ) +
  
  theme(
    legend.position = "bottom",
    
    text = element_text(family = "Arial"),
    # Change "Arial" to your desired font
    
    plot.title = element_text(size = 14, face = "bold"),
    
    axis.title = element_text(size = 14, face = "bold"),
    
    axis.text = element_text(size = 14, face = "bold"),
    
    legend.text = element_text(size = 12, face = "bold"),
    
    strip.text = element_text(size = 14, face = "bold"),
    
    legend.title = element_text(size = 12, face = "bold")
  ) +
  
  guides (lty = "none")

ggsave(
  filename = "ICH_snag_fall rate by species and size class.png",
  width = 7.91,
  height = 4,
  
  path = file.path(out_path),
  device = 'png',
  dpi = 1200
)



#Snag fall rate stats for text

sl_snagfall_only[Species %in% c("Subalpine_Fir", "Interior_Spruce")] %>%
  
  group_by(Species, SizeClass) %>%
  
  summarise(
    SnagFall_mean = mean(SnagFallRate, na.rm = TRUE),
    
    SnagFall_sd = sd(SnagFallRate, na.rm = TRUE)
  )



sl_snagfall_only[Species %in% c("Subalpine_Fir", "Interior_Spruce")] %>%
  
  group_by(SizeClass) %>%
  
  summarise(
    SnagFall_mean = mean(SnagFallRate, na.rm = TRUE),
    
    SnagFall_sd = sd(SnagFallRate, na.rm = TRUE)
  )



dc_snagfall_only[Species %in% c("Western_Hemlock", "Western_redcedar")] %>%
  
  group_by(Species, SizeClass) %>%
  
  summarise(
    SnagFall_mean = mean(SnagFallRate, na.rm = TRUE),
    
    SnagFall_sd = sd(SnagFallRate, na.rm = TRUE)
  )



dc_snagfall_only[Species %in% c("Trembling_Aspen", "Black_Cottonwood", "Paper_Birch")] %>%
  
  group_by(Species, SizeClass) %>%
  
  summarise(
    SnagFall_mean = mean(SnagFallRate, na.rm = TRUE),
    
    SnagFall_sd = sd(SnagFallRate, na.rm = TRUE)
  )



dc_snagfall_only %>%
  
  group_by(SizeClass) %>%
  
  summarise(
    SnagFall_mean = mean(SnagFallRate, na.rm = TRUE),
    
    SnagFall_sd = sd(SnagFallRate, na.rm = TRUE)
  )







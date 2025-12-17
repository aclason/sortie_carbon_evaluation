# A.Clason & E. Lilles
library(ggplot2)
library(data.table)
library(dplyr)
source(file.path("R","00-utils","utils.R"))


in_path <- "04_out_carbon"
out_path <- "05_out_analysis"

# Import data ----------------------------------------------------------------------
# SBS ------
MSL_trees_sl <- readRDS(file.path(in_path,"MSL_trees_sl.RDS"))
FSL_trees_sl <- readRDS(file.path(in_path,"FSL_trees_sl.RDS"))

MSL_trees_sl_sp <- readRDS(file.path(in_path,"MSL_trees_sl_sp.RDS"))
FSL_trees_sl_sp <- readRDS(file.path(in_path,"FSL_trees_sl_sp.RDS"))
#pre-harvest Basal area for SBS
SL_preharvest <- fread(file.path("01_data","SL_preHarvest_BA.csv"))


# ICH -------
MSL_trees_dc <- readRDS(file.path(in_path,"MSL_trees_dc.RDS"))
FSL_trees_dc <- readRDS(file.path(in_path,"FSL_trees_dc.RDS"))

MSL_trees_dc_sp <- readRDS(file.path(in_path,"MSL_trees_dc_sp.RDS"))
FSL_trees_dc_sp <- readRDS(file.path(in_path,"FSL_trees_dc_sp.RDS"))
#Tass runs for ICH
TASS <- read.csv(file.path("01_data","TASS_carbon_projections_DateCreek.csv"))


# Prep data -----------------------------------------------------------------------
# SBS ------
setnames(SL_preharvest, "Plot","Unit")
DC_preharvest <- fread(file.path("01_data","DC_preHarvest_BA.csv"))

MFL_trees_sl <- merge(FSL_trees_sl, MSL_trees_sl, by = c("Unit","Treatment","Year","State"),
                      all.x = TRUE)
setnames(MFL_trees_sl, c("MgHa.x","MgHa.y","BaHa.x","BaHa.y"), 
         c("MgHa_obs","MgHa_pred","BaHa_obs","BaHa_pred"))

# summary by treatment and year
MSL_trees_sl_sum_ty <- Rmisc::summarySE(data = MSL_trees_sl, 
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year"))
MSL_trees_sl_sum_ty <- MSL_trees_sl_sum_ty[MSL_trees_sl_sum_ty$Year < 2093,]
FSL_trees_sl_sum_ty <- Rmisc::summarySE(data = FSL_trees_sl, 
                                        measurevar = "MgHa", 
                                        groupvars = c("Treatment","Year"))

#ICH --------
MFL_trees_dc <- merge(FSL_trees_dc, MSL_trees_dc, by = c("Unit","Treatment","Year","State"),
                      all.x = TRUE)
setnames(MFL_trees_dc, c("MgHa.x","MgHa.y", "BAHa.x", "BAHa.y"),
         c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred"))


MSL_trees_dc_sum <- Rmisc::summarySE(data = MSL_trees_dc, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Year", "Treatment"))

#TASS projections
TASS$MgHa <- rowMeans(TASS[, 2:5])
TASS$MgHa_OAFs <- rowMeans(TASS[, 6:9])
TASS$Year <- TASS$Age + 1992
TASS$Treatment <- rep("TASS", length(TASS$Age))
TASS$N <- rep(4, length(TASS$Age))
TASS$sd <- rep(NA, length(TASS$Age))
TASS$se <- rep(NA, length(TASS$Age))
TASS$ci <- rep(NA, length(TASS$Age))

MSL_trees_dc_sum_TASS <- rbind(MSL_trees_dc_sum, 
                               TASS[c("Year", "Treatment", "N", "MgHa",  "sd", "se", "ci")])
TASS_OAFs <- TASS[c("Year", "Treatment", "N", "MgHa_OAFs",  "sd", "se", "ci")]
TASS_OAFs$MgHa <- TASS_OAFs$MgHa_OAFs
TASS_OAFs$Treatment <- rep("TASS_OAFs", length(TASS_OAFs$Year))
MSL_trees_dc_sum_TASS <- rbind(MSL_trees_dc_sum_TASS, 
                               TASS_OAFs[c("Year", "Treatment", "N", "MgHa",  "sd", "se", "ci")])

# Pre-harvest baselines ----------------------------------------------------------------------------
# SBS -----
# estimate pre-harvest carbon form Carbon-BA relationship
model_C_est <- lm(data = MFL_trees_sl, MgHa_obs ~ BaHa_obs)
anova(model_C_est)
summary(model_C_est)

#using the slope and intercept from the above linear model to estimate pre-harvest carbon
SL_baseline_fnct <- function(BasalArea) {-3.2 + 2.33 * BasalArea}

SL_preharvest$MgHa_pre <-
  SL_baseline_fnct(SL_preharvest$pre_harv_BA)

BArecoveryYear <-
  merge(setorder(MSL_trees_sl, Unit, Year) ,
        SL_preharvest[, .(Unit, pre_harv_BA)],
        by = "Unit",
        all.x = TRUE)

BArecoveryYear$RecoverYN <-
  ifelse(BArecoveryYear$BaHa > BArecoveryYear$pre_harv_BA,
         "Y",
         "N")

BArecoveryYear.1 <-
  subset(BArecoveryYear, BArecoveryYear$RecoverYN == "Y")

Rec_year <- BArecoveryYear.1[, .(first_year= Year[1]), by = c("Unit", "Treatment")]

Rec_year %>% 
  group_by(Treatment) %>%
  summarise(
    first_year_mean = mean(first_year, na.rm = TRUE),
    first_year_sd   = sd(first_year, na.rm = TRUE),
  ) 

#Summit lake treatments - pre-harvest basal area varied among treatments
SL_preharvest <-
  merge(SL_preharvest,  SummitLakeData::Treatments, by.x = "Unit", by.y = "unit")
setnames(SL_preharvest, "treatment", "Treatment")
#there's no Regression_SL object

SL_preharvest %>%
  group_by(Treatment) %>%
  summarise(
    preBA_mean = mean(pre_harv_BA, na.rm = TRUE),
    preBA_sd   = sd(pre_harv_BA, na.rm = TRUE),
  ) 

#Carbon recovery time
CrecoveryYear_SL <-
  merge(setorder(MSL_trees_sl, Unit, Year),
        SL_preharvest[, .(Unit, MgHa_pre)],
        by = "Unit",
        all.x = TRUE)

CrecoveryYear_SL$RecoverYN <-
  ifelse(CrecoveryYear_SL$MgHa > CrecoveryYear_SL$MgHa_pre, "Y", "N")
CrecoveryYear_SL.1 <-
  subset(CrecoveryYear_SL, CrecoveryYear_SL$RecoverYN == "Y")
CrecoveryYear_SL.1$diff <-
  (CrecoveryYear_SL.1$MgHa_pre - CrecoveryYear_SL.1$MgHa)

C_Rec_year_SL <-
  CrecoveryYear_SL.1[, .(first_year = Year[1]), by = c("Unit", "Treatment")]
C_Rec_year_SL$NumYears2recovery <- C_Rec_year_SL$first_year - 1992
C_Rec_year_SL %>% #D5 never recovers its carbon in SORTIE
  group_by(Treatment) %>%
  summarise(
    first_year_mean = mean(first_year, na.rm = TRUE),
    first_year_sd   = sd(first_year, na.rm = TRUE),
    NumYears2recovery = mean(NumYears2recovery, na.rm = TRUE)
  )


# ICH --------------------------------------------------------
#using raw data 
Baseline_DC <- subset(MFL_trees_dc, MFL_trees_dc$Year == 1992)

BArecoveryYear <-
  merge(setorder(MSL_trees_dc[Year != 1992,], Unit, Year), 
        Baseline_DC[, .(Unit, BAHa_obs)], all.x = TRUE)
BArecoveryYear$RecoverYN <-
  ifelse(BArecoveryYear$BAHa > BArecoveryYear$BAHa_obs, "Y", "N")
BArecoveryYear.1 <-
  subset(BArecoveryYear, BArecoveryYear$RecoverYN == "Y")
BArecoveryYear.1$diff <-
  (BArecoveryYear.1$BAHa_obs - BArecoveryYear.1$BAHa)

Rec_year <-
  BArecoveryYear.1[, .(first_year = Year[1]), by = c("Unit", "Treatment")]

Rec_year %>% #D5 never recovers its basal area in SORTIE
  group_by(Treatment) %>%
  summarise(
    first_year_mean = mean(first_year, na.rm = TRUE),
    first_year_sd   = sd(first_year, na.rm = TRUE),
  ) 

#Carbon recovery time
CrecoveryYear_DC <-
  merge(setorder(MSL_trees_dc[Year != 1992,], Unit, Year), 
        Baseline_DC[, .(Unit, MgHa_obs)], all.x = TRUE)
CrecoveryYear_DC$RecoverYN <-
  ifelse(CrecoveryYear_DC$MgHa > CrecoveryYear_DC$MgHa_obs, "Y", "N")
CrecoveryYear_DC.1 <-
  subset(CrecoveryYear_DC, CrecoveryYear_DC$RecoverYN == "Y")
CrecoveryYear_DC.1$diff <-
  (CrecoveryYear_DC.1$MgHa_obs - CrecoveryYear_DC.1$MgHa)

C_Rec_year_DC <-
  CrecoveryYear_DC.1[, .(first_year = Year[1]), by = c("Unit", "Treatment")]
C_Rec_year_DC$NumYears2recovery <- C_Rec_year_DC$first_year - 1992

C_Rec_year_DC %>% #D5 never recovers its carbon in SORTIE
  group_by(Treatment) %>%
  summarise(
    first_year_mean = mean(first_year, na.rm = TRUE),
    first_year_sd   = sd(first_year, na.rm = TRUE),
    NumYears2recovery = mean(NumYears2recovery, na.rm = TRUE)
  )

#Recovery time for carbon across treatment units and ecosystems ------------------------------------
#SBS -------
#I'm assuming it's this object:CrecoveryYear_SL
SL_CrecoveryYear <- CrecoveryYear_SL[Year == 1992 | Year == 1994] 

SL_CrecoveryYear_wide <-
  dcast(SL_CrecoveryYear, Unit + Treatment ~ Year, value.var = "MgHa")
SL_CrecoveryYear_wide$postMgHa <-
  ifelse(
    is.na(SL_CrecoveryYear_wide$'1992') == FALSE,
    SL_CrecoveryYear_wide$'1992',
    SL_CrecoveryYear_wide$'1994'
  )
SL_CrecoveryYear_wide <-
  merge(SL_CrecoveryYear_wide[, .(Unit, Treatment, postMgHa)],
        C_Rec_year_SL,
        by = c("Unit", "Treatment"))
SL_CrecoveryYear_wide <-
  merge(
    SL_CrecoveryYear_wide,
    SL_preharvest,
    by = c("Unit","Treatment"),
    all.x = TRUE
  )
setnames(SL_CrecoveryYear_wide, c("MgHa_pre","postMgHa"),
         c("MgHa_preHarv","MgHa_postHarv"))
SL_CrecoveryYear_wide[,PercentLost_C :=  100 - (MgHa_postHarv / MgHa_preHarv* 100) ]
SL_CrecoveryYear_wide[,pre_harv_BA :=NULL]

# ICH -------
DC_C_yr1 <- CrecoveryYear_DC[Year == 1993]
setnames(DC_C_yr1, "MgHa","MgHa_postHarv")
DC_CrecoveryYear_wide <-
  merge(DC_C_yr1[, .(Unit, Treatment, MgHa_postHarv)],
        C_Rec_year_DC,
        by = c("Unit", "Treatment"))
DC_C_yr0 <- FSL_trees_dc[Year == 1992]
setnames(DC_C_yr0, "MgHa","MgHa_preHarv")
DC_CrecoveryYear_wide <-
  merge(
    DC_CrecoveryYear_wide,
    DC_C_yr0[, .(Unit, Treatment, MgHa_preHarv)],
    by = c("Unit", "Treatment"),
    all.x = TRUE
  )
DC_CrecoveryYear_wide[, PercentLost_C := 100 - (MgHa_postHarv / MgHa_preHarv*100)]


DC_CrecoveryYear_wide[,`:=`(MgHaLost_C =  ifelse(MgHa_preHarv - MgHa_postHarv < 0,0,
                                                 MgHa_preHarv - MgHa_postHarv), 
                            Ecosystem = "ICH")]
SL_CrecoveryYear_wide[,`:=`(MgHaLost_C = ifelse(MgHa_preHarv - MgHa_postHarv < 0,0,
                                                MgHa_preHarv - MgHa_postHarv), 
                            Ecosystem = "SBS")]

# Put together ------------------------------
CrecoveryAll <- rbind(DC_CrecoveryYear_wide, SL_CrecoveryYear_wide)
CrecoveryAll[,PercentLost_C := ifelse(PercentLost_C < 0, 0, PercentLost_C)]

Crecovery_noCC <- CrecoveryAll[Treatment != "CC"] #Dropping clear-cuts from figure
harvest_intensity <- c("NH", "LR", "HR", "CC", "light/no", "med", "heavy")
harvest_intensity_labels <- c("No harvest", "High retention", "Medium retention", "No retention",
                              "High retention", "Medium retention", "Low retention")

Crecovery_noCC[, retention := ifelse(Treatment %in% harvest_intensity, 
                                   harvest_intensity_labels[match(Treatment, harvest_intensity)], 
                                   NA_character_)]
colors <- c("#F0C808", "#6C4191", "#66BBBB", "#DD4444")
harvest_intensity <- c("No harvest", "High retention", "Medium retention","Low retention")

shapes <- c(22, 21) 
ecosystem <- c("ICH", "SBS")
ecosystem_labels <- c("ICH", "SBS")
#just partial harvests
Crecovery_PC <- Crecovery_noCC[retention != "No harvest"]
Crecovery_PC[,.(mn_yrs = mean(NumYears2recovery),
                sd_yrs = sd(NumYears2recovery)), 
             by = .(Ecosystem, retention)]

# -------------------------------------------------------------------------------------------------
#Figures -------------------

#carbon recovery compared to baseline:
#SBS: Figure 6A ----
LineValue_SL <- mean(SL_preharvest$MgHa_pre) #mean of SBS pre-harvest MgHa

ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSL_trees_sl_sum_ty, aes(col = Treatment), size = 1.2) +
  #scale_x_continuous(breaks = seq(0, 28, by = 2), expand = expansion(mult = c(0, 0.05))) +
  scale_colour_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  scale_fill_manual(
    values = c("#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  ) +
  coord_cartesian(ylim = c(0, 160), xlim = c(1990,2087)) +
  labs(
    x = "Year",
    y = "Live carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSL_trees_sl_sum_ty,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  geom_point(
    data = FSL_trees_sl,
    aes(shape = Treatment),
    size = 1 ,
    position = position_dodge(width = 3)
  )+
  scale_shape_manual(
    values = c(24, 23, 21),
    breaks = c("light/no", "med", "heavy"),
    labels = c("High retention", "Medium retention", "Low retention")
  )+
  annotate("segment", x = 1985, xend = 2092, y = LineValue_SL, 
           yend = LineValue_SL, color = "gray", lty = 2, size = 1.2)+
  theme(legend.position = "bottom")

ggsave(filename = "SBS_live_trees_carbonRecovery.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)


#ICH Figure 6B ----
LineValue_DC <- mean(Baseline_DC$MgHa_obs)

ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSL_trees_dc_sum, aes(col = Treatment), size = 1.2) +
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  scale_fill_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444"),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  ) +
  coord_cartesian(ylim = c(0, 250)) +
  labs(
    x = "Year",
    y = "Live carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSL_trees_dc_sum,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  geom_point(
    data = FSL_trees_dc,
    aes(shape = Treatment),
    size = 1 ,
    position = position_dodge(width = 3)
  )+
  scale_shape_manual(
    values = c(24, 23, 21, 25),
    breaks = c("NH","LR", "HR", "CC"),
    labels = c("No harvest","High retention", "Medium retention", "No retention")
  )+
  annotate("segment", x = 1992, xend = 2092, y = LineValue_DC, 
           yend = LineValue_DC, color = "gray", lty = 2, size = 1.2)+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave(filename = "ICH_live_trees_carbonRecovery.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)

# Figure S2-1
ggplot(NULL,
       aes(x = Year, y = MgHa, fill = Treatment, group = Treatment)) +
  geom_line(data = MSL_trees_dc_sum_TASS, aes(col = Treatment), size = 1.2) +
  #geom_line(data = TASS,  aes(x = Year, y = MgHa, col = "gray"), lty = 2)+
  scale_color_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444", "grey", "black"),
    breaks = c("NH","LR", "HR", "CC", "TASS", "TASS_OAFs"),
    labels = c("No harvest","High retention", "Medium retention", "No retention", "TASS", "TASS_OAFs")
  ) +
  scale_fill_manual(
    values = c("#F0C808","#6C4191", "#66BBBB", "#DD4444", "grey", "black"),
    breaks = c("NH","LR", "HR", "CC", "TASS", "TASS_OAFs"),
    labels = c("No harvest","High retention", "Medium retention", "No retention", "TASS", "TASS_OAFs")
  ) +
  coord_cartesian(ylim = c(0, 250), xlim = c(1999, 2092)) +
  labs(
    x = "Year",
    y = "Live carbon (Mg/ha)",
    col = "Treatment",
    fill = "Treatment",
    shape = "Treatment"
  ) +
  geom_ribbon(
    data = MSL_trees_dc_sum,
    aes(ymin = MgHa - ci, ymax = MgHa + ci),
    alpha = 0.1,
    linetype = "solid",
    color = "grey"
  ) +
  geom_point(
    data = FSL_trees_dc,
    aes(shape = Treatment),
    size = 1 ,
    position = position_dodge(width = 3)
  )+
  scale_shape_manual(values = c(24, 23, 21, 25, 26),
                     breaks = c("NH","LR", "HR", "CC", "TASS"), 
                     labels = c("No harvest","High retention", "Medium retention", "No retention",
                                "TASS"))+
  annotate("segment", x = 1992, xend = 2092, y = LineValue_DC, 
           yend = LineValue_DC, color = "gray", lty = 2, size = 1.2)+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave(filename = "ICH_live_trees_TASS.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)


# Figure 7 - Carbon recovery against harvest intensity (MgHa)
fit_sep <- lm(NumYears2recovery ~ MgHaLost_C:Ecosystem - 1, data = Crecovery_PC)
fit_common <- lm(NumYears2recovery ~ MgHaLost_C - 1, data = Crecovery_PC)
anova(fit_common, fit_sep)
AIC(fit_common, fit_sep)
summary(fit_sep)
fit_sep_r2 <- round(summary(fit_sep)$r.squared,2)
fit_common_r2 <- round(summary(fit_common)$r.squared,2)

pred_com <- data.frame(MgHaLost_C = seq(0, 150, length.out = 100))
pred_com$Yrs2rec_com <- predict(fit_common, newdata = pred_com)
pred_sep <- data.table::CJ(
  MgHaLost_C = seq(0, 150, length.out = 100),
  Ecosystem = c("ICH", "SBS")
)
pred_sep$Yrs2rec_sep <- predict(fit_sep, newdata = pred_sep)

label_com <- bquote(italic(combined) ~ ": " ~ y == .(round(coef(fit_common), 2)) * x)
r2_com <- round(summary(fit_common)$r.squared, 2)
com_r <- bquote(italic(R)^2 == .(r2_com))

#modelled separate:
coefs <- coef(fit_sep)
slope_ICH <- round(coefs["MgHaLost_C:EcosystemICH"], 2)
slope_SBS <- round(coefs["MgHaLost_C:EcosystemSBS"], 2)
r2_sep <- round(summary(fit_sep)$r.squared, 2)

# Create labels
label_ICH <- bquote(italic(ICH): ~ y == .(slope_ICH)*x)
label_SBS <- bquote(italic(SBS): ~ y == .(slope_SBS)*x)
sep_r <- bquote(italic(R)^2 == .(r2_sep))


ggplot() +
  geom_point(aes(x = MgHaLost_C, y = NumYears2recovery, 
                 color = retention, fill = retention, shape = Ecosystem),
             size = 3, data = Crecovery_PC) +
  geom_line(data = pred_com, aes(x = MgHaLost_C, y = Yrs2rec_com),
            color = "grey50", linewidth = 1, linetype = "dashed") +
  geom_line(data = pred_sep, aes(x = MgHaLost_C, y = Yrs2rec_sep, group = Ecosystem),
            color = "black", linewidth = 1) +
  scale_color_manual(values = colors,
                     breaks = harvest_intensity) +
  scale_fill_manual(values = colors,
                    breaks = harvest_intensity) +
  scale_shape_manual(values = shapes,
                     breaks = ecosystem,
                     labels = ecosystem_labels) +
  coord_cartesian(xlim = c(0, 150), ylim = c(0, 150)) +
  labs(
    x = "Carbon removed Mg/Ha",
    y = "Years to carbon recovery",
    color = "Harvest Intensity",
    fill = "Harvest Intensity",
    shape = "Ecosystem"
  )+
  annotate("text", x = 120, y = 140, label = label_ICH, size = 5) +
  annotate("text", x = 120, y = 132, label = label_SBS, size = 5) +
  annotate("text", x = 120, y = 126, label = sep_r, size = 5) +
  annotate("text", x = 70, y = 134, label = label_com, size = 5) +
  annotate("text", x = 70, y = 128, label = com_r, size = 5)

ggsave(filename = "CarbonRecoveryTime.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)


# percent ----------------------------------------
fit_sep <- lm(NumYears2recovery ~ PercentLost_C:Ecosystem - 1, data = Crecovery_PC)
fit_common <- lm(NumYears2recovery ~ PercentLost_C - 1, data = Crecovery_PC)
anova(fit_common, fit_sep)
AIC(fit_common, fit_sep)
summary(fit_sep)
fit_sep_r2 <- round(summary(fit_sep)$r.squared,2)
fit_common_r2 <- round(summary(fit_common)$r.squared,2)

pred_com <- data.frame(PercentLost_C = seq(0, 150, length.out = 100))
pred_com$Yrs2rec_com <- predict(fit_common, newdata = pred_com)
pred_sep <- data.table::CJ(
  PercentLost_C = seq(0, 150, length.out = 100),
  Ecosystem = c("ICH", "SBS")
)
pred_sep$Yrs2rec_sep <- predict(fit_sep, newdata = pred_sep)

label_com <- bquote(italic(combined) ~ ": " ~ y == .(round(coef(fit_common), 2)) * x)
r2_com <- round(summary(fit_common)$r.squared, 2)
com_r <- bquote(italic(R)^2 == .(r2_com))

#modelled separate:
coefs <- coef(fit_sep)
slope_ICH <- round(coefs["PercentLost_C:EcosystemICH"], 2)
slope_SBS <- round(coefs["PercentLost_C:EcosystemSBS"], 2)
r2_sep <- round(summary(fit_sep)$r.squared, 2)

# Create labels
label_ICH <- bquote(italic(ICH): ~ y == .(slope_ICH)*x)
label_SBS <- bquote(italic(SBS): ~ y == .(slope_SBS)*x)
sep_r <- bquote(italic(R)^2 == .(r2_sep))


ggplot() +
  geom_point(aes(x = PercentLost_C, y = NumYears2recovery, 
                 color = retention, fill = retention, shape = Ecosystem),
             size = 3, data = Crecovery_PC) +
  geom_line(data = pred_com, aes(x = PercentLost_C, y = Yrs2rec_com),
            color = "grey50", linewidth = 1, linetype = "dashed") +
  geom_line(data = pred_sep, aes(x = PercentLost_C, y = Yrs2rec_sep, group = Ecosystem),
            color = "black", linewidth = 1) +
  scale_color_manual(values = colors,
                     breaks = harvest_intensity) +
  scale_fill_manual(values = colors,
                    breaks = harvest_intensity) +
  scale_shape_manual(values = shapes,
                     breaks = ecosystem,
                     labels = ecosystem_labels) +
  coord_cartesian(xlim = c(0, 110), ylim = c(0, 110)) +
  labs(
    x = "Carbon removed %",
    y = "Years to carbon recovery",
    color = "Harvest Intensity",
    fill = "Harvest Intensity",
    shape = "Ecosystem"
  )+
  annotate("text", x = 60, y = 110, label = label_ICH, size = 5) +
  annotate("text", x = 60, y = 105, label = label_SBS, size = 5) +
  annotate("text", x = 60, y = 100, label = sep_r, size = 5) +
  annotate("text", x = 20, y = 108, label = label_com, size = 5) +
  annotate("text", x = 20, y = 103, label = com_r, size = 5)

ggsave(filename = "CarbonRecoveryTime_per.jpg",
       path = file.path(out_path), device='jpeg', dpi=1000)




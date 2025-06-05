# A.Clason & E. Lilles
library(ggplot2)
library(data.table)
library(dplyr)
source(file.path("R","00-utils","utils.R"))


in_path <- "04_out_carbon"
out_path <- "05_out_analysis"

# SBS -------------------------------------------------------------------------------------------
MSL_trees_sl <- readRDS(file.path(in_path,"MSL_trees_sl.RDS"))
FSL_trees_sl <- readRDS(file.path(in_path,"FSL_trees_sl.RDS"))

MSL_trees_sl_sp <- readRDS(file.path(in_path,"MSL_trees_sl_sp.RDS"))
FSL_trees_sl_sp <- readRDS(file.path(in_path,"FSL_trees_sl_sp.RDS"))

#Basal area recovery time
#there was no pre-harvest basal area data for Unit 15 so it is dropped from recovery analysis
SL_preharvest <- fread(file.path("01_data","SL_preHarvest_BA.csv"))
setnames(SL_preharvest, "Plot","Unit")
DC_preharvest <- fread(file.path("01_data","DC_preHarvest_BA.csv"))



MFL_trees_sl <- merge(FSL_trees_sl, MSL_trees_sl, by = c("Unit","Treatment","Year","State"),
                      all.x = TRUE)
setnames(MFL_trees_sl, c("MgHa.x","MgHa.y","BaHa.x","BaHa.y"), 
         c("MgHa_obs","MgHa_pred","BaHa_obs","BaHa_pred"))
MF_trees_sl_sp <- merge(FSL_trees_sl_sp, MSL_trees_sl_sp, 
                        by = c("Unit","Treatment","Year","State","Species"),
                        all.x = TRUE)
setnames(MF_trees_sl_sp, c("MgHa.x","MgHa.y","BaHa.x","BaHa.y"), 
         c("MgHa_obs","MgHa_pred","BaHa_obs","BaHa_pred"))
MF_trees_sl_sp[is.na(MgHa_pred), MgHa_pred := 0]


# ICH ----------------------------------------------------------------------------------------
MSL_trees_dc <- readRDS(file.path(in_path,"MSL_trees_dc.RDS"))
FSL_trees_dc <- readRDS(file.path(in_path,"FSL_trees_dc.RDS"))

MSL_trees_dc_sp <- readRDS(file.path(in_path,"MSL_trees_dc_sp.RDS"))
FSL_trees_dc_sp <- readRDS(file.path(in_path,"FSL_trees_dc_sp.RDS"))

MFL_trees_dc <- merge(FSL_trees_dc, MSL_trees_dc, by = c("Unit","Treatment","Year","State"),
                      all.x = TRUE)
setnames(MFL_trees_dc, c("MgHa.x","MgHa.y", "BAHa.x", "BAHa.y"),
         c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred"))

MF_trees_dc_sp <- merge(FSL_trees_dc_sp, MSL_trees_dc_sp, 
                        by = c("Unit","Treatment","Year","State","Species"),
                        all.x = TRUE)
setnames(MF_trees_dc_sp, c("MgHa.x","MgHa.y", "BAHa", "BaHa"),
         c("MgHa_obs","MgHa_pred","BAHa_obs", "BAHa_pred"))
MSL_trees_dc_sum <- Rmisc::summarySE(data = MSL_trees_dc, 
                                     measurevar = "MgHa", 
                                     groupvars = c("Year", "Treatment"))

# SBS ----------------------------------------
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

#plot(SL_CrecoveryYear_wide$PercentLost_C,
     #SL_CrecoveryYear_wide$NumYears2recovery)

# ICH --------------------------------------------------------
#using raw data
Baseline_DC <- subset(MFL_trees_dc, MFL_trees_dc$Year == 1992)

#OR using same equation approach as Summit Lake?
#

#
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

# Together -----------------------------------------------------------------------------------------
#Recovery time for carbon across treatment units and ecosystems figure
# SBS
#Recovery time for carbon across treatment units and ecosystems figure
# data to be brought in on Date Creek section below
SL_CrecoveryYear <- CrecoveryYear_SL[Year == 1992 | Year == 1994] #I'm assuming it's this object:CrecoveryYear_SL

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

# ICH
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
Crecovery_PC <- Crecovery_noCC[retention != "No harvest"]
# ---------------------------------------------------------------------------------
# analysis and figures

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




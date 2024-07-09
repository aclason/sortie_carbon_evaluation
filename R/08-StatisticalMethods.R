# A.Clason
library(equivalence)
source(file.path("R","00-utils","utils.R"))
library(ggplot2)
library(data.table)

in_path <- "04_out_carbon"

#Live trees -------------------------------------------------------------------------
#Date creek


#summit lake
MSL_trees_sl <- readRDS(file.path(in_path,"MSL_trees_sl.RDS"))
FSL_trees_sl <- readRDS(file.path(in_path,"FSL_trees_sl.RDS"))
MSL_trees_sl_sp <- readRDS(file.path(in_path,"MSL_trees_sl_sp.RDS"))
FSL_trees_sl_sp <- readRDS(file.path(in_path,"FSL_trees_sl_sp.RDS"))

ggplot()+
  geom_line(aes(x = Year, y = MgUnit, group = as.factor(unit), color = treatment), 
            data = MSL_trees_sl)+
  geom_abline(slope = 0, intercept = 56.7754, linetype = "dashed", color = "grey")+
  geom_point(aes(x = Year, y = MgUnit, color = treatment), 
             data = FSL_trees_sl)

mean(MSL_trees_sl[Year ==1992 & treatment == "light/no"]$MgUnit)
#56 MgHa at 1992
mean(MSL_trees_sl[Year == 2012 & treatment == "med"]$MgUnit)
#by 20 years after harvest, med treatment regained carbon
mean(MSL_trees_sl[Year == 2019 & treatment == "heavy"]$MgUnit)
#by 35 years after harvest, heavy treatment regained carbon

#just modelled - all 100 years
model1 <- lm(MgUnit ~ treatment + Species + Year, MSL_trees_sl_sp)
summary(model1)

#at 100 years - what is the difference in carbon in treatments
summary(lm(MgUnit ~ treatment-1, MSL_trees_sl[Year == 2091]))
anova(lm(MgUnit ~ treatment-1, MSL_trees_sl[Year == 2091]))

ggplot()+
  geom_boxplot(aes(y = MgUnit, x =treatment, fill = treatment), 
               data = MSL_trees_sl[Year == 2091])
ggplot()+
  geom_boxplot(aes(y = MgUnit, x =treatment, fill = treatment), 
               data = MSL_trees_sl[Year == 2051])
ggplot()+
  geom_boxplot(aes(y = MgUnit, x =treatment, fill = treatment), 
               data = MSL_trees_sl[Year == 1992])
# after 100 years, the carbon in the no/light treatment remains significantly
# higher than med & heavy treatments, but the heavy and medium treatments are
# no longer significantly different? 
# looks like at about 50 years, the medium and heavy harvests merge

ggplot()+
  geom_line(aes(x = Year, y = MgUnit, group = as.factor(unit), 
                color = treatment), 
            data = MSL_trees_sl_sp[Species == "Bl"|Species == "Sx"])+
  geom_jitter(aes(x = Year, y = MgUnit, color = treatment), 
              data = FSL_trees_sl_sp[Species == "Bl"|Species == "Sx"])+
  facet_wrap(~Species)


MF_trees_sl <- merge(FSL_trees_sl, MSL_trees_sl, by = c("unit","treatment","Year","State"),
                     all.x = TRUE)
setnames(MF_trees_sl, c("MgUnit.x","MgUnit.y"), c("MgHa_obs","MgHa_pred"))

MF_trees_sl_sp <- merge(FSL_trees_sl_sp, MSL_trees_sl_sp, by = c("unit","treatment","Year","State","Species"),
                     all.x = TRUE)
setnames(MF_trees_sl_sp, c("MgUnit.x","MgUnit.y"), c("MgHa_obs","MgHa_pred"))
MF_trees_sl_m <- melt(MF_trees_sl, 
                         id.vars = c("unit", "treatment", "Year", "State"),
                         measure.vars = c("MgHa_obs", "MgHa_pred"),
                         variable.name = "Type", 
                         value.name = "MgHa")
MF_trees_sl_m[, Type := ifelse(Type == "MgHa_obs", "obs", "pred")]
MF_trees_sl_sp_m <- melt(MF_trees_sl_sp, 
                              id.vars = c("unit", "treatment", "Year", "State", "Species"),
                              measure.vars = c("MgHa_obs", "MgHa_pred"),
                              variable.name = "Type", 
                              value.name = "MgHa")
MF_trees_sl_sp_m[, Type := ifelse(Type == "MgHa_obs", "obs", "pred")]

ggplot()+
  geom_point(aes(x = MgHa_pred, y = MgHa_obs, group = as.factor(unit), color = treatment), 
            data = MF_trees_sl)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey")+
  xlim(c(0,120))+
  ylim(c(0,120))




#positive value - actual is 18.8 MgHa higher than predicted

cohen_d(FSL_trees_sl[Year == 2019]$MgUnit, 
        MSL_trees_sl[Year == 2019]$MgUnit)
#effect size - 


equi_result(FSL_trees_sl[Year == 2019]$MgUnit, 
            MSL_trees_sl[Year == 2019]$MgUnit,
            eq_margin = 0.5)
#not rejected == they are not different

equi_boot(actual = FSL_trees_sl[Year == 2019]$MgUnit, 
          predicted = MSL_trees_sl[Year == 2019]$MgUnit,
          n_bootstraps = 1000,
          eq_margin = 0.5)


#Dead trees -------------------------------------------------------------------------
#Summit lake
MSD_trees_sl <- readRDS(file.path(in_path,"MSD_trees_sl.RDS"))
FSD_trees_sl <- readRDS(file.path(in_path,"FSD_trees_sl.RDS"))
ggplot()+
  geom_line(aes(x = Year, y = MgUnit, group = as.factor(unit), color = treatment), 
            data = MSD_trees_sl)+
  geom_point(aes(x = Year, y = MgUnit, color = treatment), 
             data = FSD_trees_sl)

#Stats -------------------------------------------------------------------------

years <- c(1992, 1994, 1997, 2009, 2019, "All Years")

results <- lapply(years, function(year) {
  data <- select_years(year, data = MF_trees_sl)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Year = years, results_df)
results_df
res_m <- data.table(melt(results_df, id.vars = "Year"))

ggplot()+
  geom_line(aes(x = as.factor(Year), y = as.numeric(value), 
                group = as.factor(variable), color = variable), 
            data = res_m[Year != "All Years"])+
  geom_point(aes(x = as.factor(Year), y = as.numeric(value), 
                group = as.factor(variable), color = variable, size =2), 
            data = res_m[Year == "All Years"])
  
  


sp <- c("Bl","Sx")

results <- lapply(sp, function(sp) {
  data <- select_sp(sp, data = MF_trees_sl_sp)
  sapply(stat_functions, function(f) f(data))
})
results_df <- do.call(rbind, results)
results_df <- data.frame(Species = sp, results_df)
results_df

library(lme4)

#only the years surveyed
model1 <- lm(MgHa ~ Type + treatment + Species + Year, MF_trees_sl_sp_m)
model2 <- lm(MgHa ~ Type + treatment * Species + Year, MF_trees_sl_sp_m)
model3 <- lm(MgHa ~ Type * treatment * Species + Year, MF_trees_sl_sp_m)
model4 <- lm(MgHa ~ Type * treatment * Species * Year, MF_trees_sl_sp_m)
model5 <- lm(MgHa ~ Type * treatment + Species * Year, MF_trees_sl_sp_m)
model6 <- lm(MgHa ~ Type + treatment * Species * Year, MF_trees_sl_sp_m)
model7 <- lm(MgHa ~ Type + treatment + Species * Year, MF_trees_sl_sp_m)
model8 <- lm(MgHa ~ Type * Species + treatment + Year, MF_trees_sl_sp_m)
model9 <- lm(MgHa ~ Species + treatment + Year, MF_trees_sl_sp_m)
model10 <- lm(MgHa ~ treatment, MF_trees_sl_sp_m)

AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10)
summary(model4)

model1 <- lm(MgHa ~ Type + treatment + Year, MF_trees_sl_m)
model2 <- lm(MgHa ~ Type * treatment + Year, MF_trees_sl_m)
model3 <- lm(MgHa ~ Type * treatment * Year, MF_trees_sl_m)
model4 <- lm(MgHa ~ Type * Year + treatment, MF_trees_sl_m)

AIC(model1, model2, model3, model4)
summary(model1)




#Date creek

#CWD ----------------------------------------------------------------------------------

#Summit Lake
MS_cwd_sl <- readRDS(file.path(in_path,"MS_cwd_sl.RDS"))
MS_cwd_sl[,Unit:=as.numeric(unit)] #deal with this earlier
FS_cwd_sl <- readRDS(file.path(in_path,"FS_cwd_sl.RDS"))

#Date Creek
MS_cwd_dc <- readRDS(file.path(in_path,"MS_cwd_dc.RDS"))
FS_cwd_dc <- readRDS(file.path(in_path,"FS_cwd_dc.RDS"))


setkey(FS_cwd_sl,Unit)
setkey(MS_cwd_sl,Unit)

bias(FS_cwd_sl[timestep==28 & Unit!= 4 & Unit != 15]$MgHa, MS_cwd_sl[timestep==27]$MgHa, 
     n = 17)
#positive value - actual is higher than predicted

rmse(#FS_cwd_sl[timestep==29]$MgHa, MS_cwd_sl[timestep==29]$MgHa, 
     FS_cwd_sl[timestep==28 & Unit!= 4 & Unit != 15]$MgHa, MS_cwd_sl[timestep==27]$MgHa,
     n = 17)
#positive value - actual is 8.2MgHa higher than predicted

cohen_d(FS_cwd_sl[timestep==28 & Unit!= 4 & Unit != 15]$MgHa, MS_cwd_sl[timestep==27]$MgHa)
#effect size - 


equi_result(FS_cwd_sl[timestep==29]$MgHa, MS_cwd_sl[timestep==29]$MgHa,
            eq_margin = 0.5)
#not rejected == they are not different
equi_result(FS_cwd_sl[timestep==28 & Unit!= 4 & Unit != 15]$MgHa, MS_cwd_sl[timestep==27]$MgHa,
            eq_margin = 0.5)

equi_boot(actual = FS_cwd_sl[timestep==28 & Unit!= 4 & Unit != 15]$MgHa, 
          predicted = MS_cwd_sl[timestep==27]$MgHa,
          n_bootstraps = 1000,
          eq_margin = 0.5)









# Mixed effects models
DC_L_C_Hw_lmer <- lmer(C_unit ~ DataSource * Treatment + (1|Unit/timestep), DC_L_C_Hw_u_mod2)

model1 <- lmer(C_unit ~ DataSource * Treatment + (1|Unit/timestep), dc_s_f)
model2 <- lmer(C_unit ~ DataSource + Treatment + (1|Unit/timestep), dc_s_f)
# Is there a difference in carbon predicted between field and model, and does that change
# over time and with treatment
model3 <- lmer(C_unit ~ DataSource + Treatment + timestep + (1|Unit), dc_s_f)

#could put both datasets together for this and then the response is the difference
#instead of the absolute?
model4 <- lmer(C_diff ~ param_file + harv_int + timestep + (1|Unit), dc_sl_s_f)

#does Sortie perform better when initiated before harvest or after? (compare right after harvest?)


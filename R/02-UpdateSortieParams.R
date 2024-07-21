library(data.table)
source(file.path("R","00-utils","utils.R"))

in_dir <- file.path("02_init_sortie") 


#SBS---------------------------------------------------------------------------
sbs_in_dir <- file.path(in_dir, "02_summit_lake","Productivity") 


NCI.adults.generic <- fread(file.path(sbs_in_dir,"00_generic_nci.csv"))
si_rel_mc2 <- fread(file.path(sbs_in_dir,"si_relative_to_mc2.csv"))
#The function to define the maximum growth rate based on the formula for model fitting:
#NCI.MaxGrowth <- NCI.MaxGrowth.Est * edaphic**NCI.Fert.eff

# So we need to get the edaphic equivalent for the SBSmk1 site and species compared to an 01 mc2 site
si_rel_mc2[BGC == "SBSmk1" & Sp == "Pl"]
#in this case - depending on the species, 
#mc2_mn_ratio is the productivity difference between mc2 01 site series and the ss-sp combo of interest

ss_eq <- data.table(Sp_name = c("Interior_Spruce", "Subalpine_Fir", "Lodgepole_Pine"),
                    ss_est = c(6,6,6)) # while the site index data suggests it should be a mc2 equivalent of ss 4 

for(i in 1:nrow(ss_eq)){
  mc2_01_est <- ((Typical_SS[SS == ss_eq[i]$ss_est]$SNR/6)+
                   (Typical_SS[SS == ss_eq[i]$ss_est]$SMR/6))*0.5
  nci_val <- NCI.adults.generic[V1 == "NCI.MaxGrowth.Est", .(get(ss_eq[i]$Sp_name))] * 
    mc2_01_est**NCI.adults.generic[V1 == "NCI.Fert.eff",.(get(ss_eq[i]$Sp_name))]
  ss_eq[Sp_name == ss_eq$Sp_name[i], nci_est := nci_val]
}

NCI.adults <- NCI.adults.generic[V1 != "NCI.Fert.eff"]
NCI.adults[V1 == "NCI.MaxGrowth.Est", V1 := "NCI.MaxGrowth"]
NCI.adults[V1 == "NCI.MaxGrowth", `:=`(Interior_Spruce = ss_eq[Sp_name == "Interior_Spruce"]$nci_est,
                                      Lodgepole_Pine = ss_eq[Sp_name == "Lodgepole_Pine"]$nci_est,
                                      Subalpine_Fir = ss_eq[Sp_name == "Subalpine_Fir"]$nci_est)]

fwrite(NCI.adults,
       "D:/Github/sortie_carbonExtensionNote/02_init_sortie/02_summit_lake/ParameterValues/nci_si_6.csv")

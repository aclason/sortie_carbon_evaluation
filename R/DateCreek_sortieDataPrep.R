library(data.table)
library(treeCalcs)
library(tidyverse)


# Sortie output pathways
# linking to the most recent Date Creek outputs files
path_outputs <- "D:/Github/SORTIEparams/Outputs/ICH/Snags/"
#most recent run with snags included at initiation
outputs <- grep("_L",list.files(path_outputs, "-sn_br.csv", full.names = TRUE),invert=TRUE, value=TRUE)
calcTree <- TRUE #is carbon already calculated?? could change this to an exist?
inclSnags <- TRUE

CfileName <- "D:/Github/DateCreekC/Outputs/DC_sn_br_TreeC.csv"
C_plotFileName <- "D:/Github/DateCreekC/Outputs/DC_sn_br_PlotC.csv"

function(){
if(calcTree == TRUE){
    #calculate carbon
    DT_list <- map(outputs, fread)
    DT <- rbindlist(DT_list, fill = TRUE)
    names(DT)[names(DT) == "SubPlot"] <- "PlotNumber"
    
    #DT <- subset(DT, DT$DBH > 5) #not sure why she did this part-unless to match field?
    DT[, Class := ifelse(Dead.Code=="Alive" & Type!="Snag",1,3)]
    DT$Species[DT$Species == "Western_redcedar"] <- "Cw"
    DT$Species[DT$Species == "Western_Hemlock"] <- "Hw"
    DT$Species[DT$Species == "Subalpine_Fir"] <- "Bl"
    DT$Species[DT$Species == "Hybrid_spruce"] <- "Sx"
    DT$Species[DT$Species == "Paper_Birch"] <- "Ep"
    DT$Species[DT$Species == "Black_Cottonwood"] <- "Ac"
    DT$Species[DT$Species == "Amabalis_Fir"] <- "Ba"
    DT$Species[DT$Species == "Trembling_Aspen"] <- "At"
    DT$Species[DT$Species == "Lodgepole_Pine" ] <- "Pl"
    
    #1. Calculate carbon for live trees
    #need to break it to apply the correct function
    DT_L <- DT[Class<3 & !is.na(DBH)]
    Carbon <- vector()
    for(i in 1:nrow(DT_L)){
      Carbon[i] <- TreeCarbonFN(Species = DT_L[i, Species], DBH = DT_L[i, DBH], 
                                HT = DT_L[i, Height], Tree_class = DT_L[i, Class])
    }
    # Translate to Mg C/ tree
    DT_L[, ':='(C_tree = Carbon/1000)]
    
    #2. Calculate carbon for snags
    DT_S <- DT[Class==3] #don't think it can be a dead seedling
    
    #Option 1: can get height when the snag dies during a run and height is assigned:
    #DT_S_ <- DT_S[!is.na(Height)]
    #DT_S[is.na(Height), Height := DT_S_[.SD, Height, on=c("X", "Y")]]
    
    #Option 2: can get height from last timestep before (adult-Alive before became snag-alive):
    
    #Option 3: calculate height from allometry if height hasn't been assigned
    Carbon <- vector()
    for(i in 1:nrow(DT_S)){
      if(is.na(DT_S[i,Height])){
        DT_S[i, Height:= DiamHgtFN(Species = DT_S[i,Species], DBH = DT_S[i,DBH], BECzone = "ICH")]
      }
      
      Carbon[i] <- TreeCarbonFN(Species = DT_S[i, Species], DBH = DT_S[i, DBH], 
                                HT = DT_S[i, Height], Tree_class = DT_S[i, Class])
    }
    # Translate to Mg C/ tree
    DT_S[, ':='(C_tree = Carbon/1000)]
    
    #3. Could add one for seedlings
    
    DT_LS <- rbind(DT_L,DT_S)
    fwrite(DT_LS, CfileName)
  }else{
    #read in calculated carbon
    DT_LS <- fread(CfileName)
  }

return(DT_LS)
}


#Coarse woody debris






######################### Calculate dead tree carbon ################################
#SBS---------------------------------------------------------------------------
out_path <- "./Inputs/SORTIEruns/SummitLake/Outputs/" 

#files_2_ext <- list.files(out_path, pattern = "det", full.names = FALSE)

#do the comparison of parsed in R vs. sortie gui extract for SBS runs

out_files <- list.files(path = out_path, pattern = "ds_part_det_",
                        full.names = TRUE)

dt_table <- c()
for(i in 1:length(out_files)){
  dt <- fread(out_files[i], sep="\t", header=T, na.strings = "--", skip=1)
  plotID <- as.numeric(str_split(str_split(basename(out_files[i]), 
                                           "summit", simplify = T)[,2],
                      "-ds",simplify=TRUE)[,1])
  yr <- as.numeric(str_split(basename(out_files[i]), "det_", simplify = T)[,2])
  dt[, ':='(timestep = yr,unit=plotID)]
  dt_table <- rbind(dt_table,dt)
  print(paste("reading plot",plotID,"year",yr))
}

fwrite(dt_table, paste0(out_path,"summit_trees.csv"))




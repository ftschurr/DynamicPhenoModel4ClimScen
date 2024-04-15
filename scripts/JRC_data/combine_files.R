# base_path <- "O:/Projects/KP0030_ftschurr/data/JRC/Germany/raw"
# 
# 
# 
# files <- list.files(base_path, full.names = T)
# 
# files <- files[grep(".csv",files)]
# all_files <- list()
# for(f in files){
#   all_files[[f]] <- read.csv(f, sep=";")
#   
# }
# 
# 
# combined <- do.call("rbind", all_files)
# 
# write.csv(combined,paste(base_path,"germany_global_radiation_1979010120221231.csv",sep="/"))


  base_path <- "O:/Projects/KP0030_ftschurr/data/JRC/Germany"
  variables <- c("tasmin","tasmax","tas","pr","VP")
  variables <- c("tasmax","tas","pr","VP")
  variables <- c("tasmin")
for (variable in variables) {
  print(variable)
  path_files <- file.path(base_path,variable)
  
  
  files <- list.files(path_files, full.names = T)
  files <- files[grep(".csv",files)]
  
  all_files <- list()
  for(f in files){
    all_files[[f]] <- read.csv(f, sep=";")
    
  }
  
  
  combined <- do.call("rbind", all_files)
  
  write.csv(combined,paste(path_files,paste0("germany_",variable,"_1979010120221231.csv"),sep="/"))
  
  
}
  
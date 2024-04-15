x <- list.files("O:/Projects/KP00xx_DynaPhen/2020_tschurr_flavian_phd_KP00xx/data/CH2018_data/QMstations/rsds")
chain_names <- data.frame(names_col=x)
library(tidyr)
x_new <- separate(chain_names,names_col,sep="_",into=c("CH2018","variable","first","second","third","sceni","QM","years","format"))
x_new <- separate(chain_names,names_col,sep="_",into=c("CH2018","variable","RCM","GCM","resolution","rcp","QM","years","format"))
x_new <- x_new[,-c(1,2,7,8,9)]
x_new$chain_name <- paste(x_new$RCM,x_new$GCM,x_new$resolution,x_new$rcp,sep="_")

write.csv(x_new, "O:/Projects/KP00xx_DynaPhen/2020_tschurr_flavian_phd_KP00xx/climate_suitability/meta_files/modelchains/modelchains_CH2018_climate_suitability_rsds.csv",row.names = FALSE)

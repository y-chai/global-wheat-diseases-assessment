
rm(list=ls())
# Calculate FAO stat for wheat by region
library(data.table)


# Read in FAO data
dat.FAO <- fread("./FAO data/wheat_prod_2019_FAOSTAT_data_7-28-2021.csv")
dat.FAO[, ISO3 := `Area Code (ISO3)`]

##
#Add in Zone info

ISO_Zone_sim <- fread("./Zone table/ISO_Zone_sim_FAO2019.csv")

FAO_Zone <- merge(dat.FAO, ISO_Zone_sim, by="ISO3", all.x=TRUE)

#Aggregate by Zone
Zone_Wheat <- FAO_Zone[, .(whea_tot = sum(Value, na.rm=T)) , by=Zone]

write.csv(Zone_Wheat, paste0("./Final Data/Zone_Wheat_tot.csv"))

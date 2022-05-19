rm(list=ls())

library(raster)
library(data.table)
library(magrittr)

#############
####SPAM 2010#####
#############

# Harvested Acres
SPAM_whea_H_a <- fread("E:/My Backups/SPAM/spam2010V2r0_global_harv_area.csv/spam2010V2r0_global_H_TA.csv")[, .(cell5m, iso3, whea_a)]
SPAM_whea_H_i <- fread("E:/My Backups/SPAM/spam2010V2r0_global_harv_area.csv/spam2010V2r0_global_H_TI.csv")[, .(cell5m, iso3, whea_i)]
SPAM_whea_H_h <- fread("E:/My Backups/SPAM/spam2010V2r0_global_harv_area.csv/spam2010V2r0_global_H_TH.csv")[, .(cell5m, iso3, whea_h)]
SPAM_whea_H_l <- fread("E:/My Backups/SPAM/spam2010V2r0_global_harv_area.csv/spam2010V2r0_global_H_TL.csv")[, .(cell5m, iso3, whea_l)]
SPAM_whea_H_s <- fread("E:/My Backups/SPAM/spam2010V2r0_global_harv_area.csv/spam2010V2r0_global_H_TS.csv")[, .(cell5m, iso3, whea_s)]

SPAM_whea_H <- SPAM_whea_H_a %>% merge(SPAM_whea_H_i) %>% merge(SPAM_whea_H_h) %>% 
                                 merge(SPAM_whea_H_l) %>% merge(SPAM_whea_H_s)
rm(SPAM_whea_H_a, SPAM_whea_H_i, SPAM_whea_H_h, SPAM_whea_H_l, SPAM_whea_H_s)
SPAM_whea_H <- SPAM_whea_H[order(cell5m)]


# Productions
SPAM_whea_P_a <- fread("E:/My Backups/SPAM/spam2010V2r0_global_prod.csv/spam2010V2r0_global_P_TA.csv")[, .(cell5m, iso3, whea_a)]
SPAM_whea_P_i <- fread("E:/My Backups/SPAM/spam2010V2r0_global_prod.csv/spam2010V2r0_global_P_TI.csv")[, .(cell5m, iso3, whea_i)]
SPAM_whea_P_h <- fread("E:/My Backups/SPAM/spam2010V2r0_global_prod.csv/spam2010V2r0_global_P_TH.csv")[, .(cell5m, iso3, whea_h)]
SPAM_whea_P_l <- fread("E:/My Backups/SPAM/spam2010V2r0_global_prod.csv/spam2010V2r0_global_P_TL.csv")[, .(cell5m, iso3, whea_l)]
SPAM_whea_P_s <- fread("E:/My Backups/SPAM/spam2010V2r0_global_prod.csv/spam2010V2r0_global_P_TS.csv")[, .(cell5m, iso3, whea_s)]

SPAM_whea_P <- SPAM_whea_P_a %>% merge(SPAM_whea_P_i) %>% merge(SPAM_whea_P_h) %>% 
                                 merge(SPAM_whea_P_l) %>% merge(SPAM_whea_P_s)
rm(SPAM_whea_P_a, SPAM_whea_P_i, SPAM_whea_P_h, SPAM_whea_P_l, SPAM_whea_P_s)
SPAM_whea_P <- SPAM_whea_P[order(cell5m)]


# #####################################
# #Read in wheat SPAM data 2010 from Mason Hurley processed wheat sPAM#
# #####################################
# SPAM_whea_a <- fread("./Wheat SPAM 2010/MH Data/wheat-2010-HA-all.csv", col.names = c("y", "x", "whea_a"))
# SPAM_whea_i <- fread("./Wheat SPAM 2010/MH Data/wheat-2010-HA-irrigated.csv", col.names = c("y", "x", "whea_i"))
# SPAM_whea_h <- fread("./Wheat SPAM 2010/MH Data/wheat-2010-HA-rainfed_high.csv", col.names = c("y", "x", "whea_h"))
# SPAM_whea_l <- fread("./Wheat SPAM 2010/MH Data/wheat-2010-HA-rainfed_low.csv", col.names = c("y", "x", "whea_l"))
# SPAM_whea_s <- fread("./Wheat SPAM 2010/MH Data/wheat-2010-HA-rainfed_subsistence.csv", col.names = c("y", "x", "whea_s"))
# SPAM_whea_H <- SPAM_whea_a %>% merge(SPAM_whea_i) %>% merge(SPAM_whea_h) %>% merge(SPAM_whea_l) %>% merge(SPAM_whea_s)
# rm(SPAM_whea_a, SPAM_whea_i, SPAM_whea_h, SPAM_whea_l, SPAM_whea_s)
# str(SPAM_whea_H)
# SPAM_whea_H <- SPAM_whea_H[order(-y, x)][, cell5m := 0:(.N-1)]
# setkey(SPAM_whea_H, cell5m)  #Set the HC cell5m as the key for data.table


#########################
#A 5' raster for wheat data#
#########################

#Create a 5' blank raster#
r5 <- raster(ext=extent(-180, 180, -90, 90), res=1/12)
r5_df <- data.table(cell5m = seq(1:ncell(r5))-1)
SPAM_whea_H <- merge(r5_df, SPAM_whea_H, by="cell5m", all.x=T)
SPAM_whea_P <- merge(r5_df, SPAM_whea_P, by="cell5m", all.x=T)


#Add in SPAM data
r5_whea_H <- r5
r5_whea_H$whea   <- SPAM_whea_H$whea_a
r5_whea_H$whea_h <- SPAM_whea_H$whea_h                        #High input wheat
r5_whea_H$whea_i <- SPAM_whea_H$whea_i                        #Irrigated wheat
r5_whea_H$whea_l <- SPAM_whea_H$whea_l+SPAM_whea_H$whea_s     #Low input wheat (include low and subsistence)

r5_whea_H$whea[is.na(r5_whea_H$whea)] <- 0

plot(r5_whea_H)

r5_whea_P <- r5
r5_whea_P$whea   <- SPAM_whea_P$whea_a
r5_whea_P$whea_h <- SPAM_whea_P$whea_h                        #High input wheat
r5_whea_P$whea_i <- SPAM_whea_P$whea_i                        #Irrigated wheat
r5_whea_P$whea_l <- SPAM_whea_P$whea_l+SPAM_whea_P$whea_s     #Low input wheat (include low and subsistence)

plot(r5_whea_P)

#Save the raster files at 5'
writeRaster(r5_whea_H, filename="./Intermediate Data/SPAM_r5_whea_H.grd", bandorder='BIL', overwrite=TRUE)
writeRaster(r5_whea_P, filename="./Intermediate Data/SPAM_r5_whea_P.grd", bandorder='BIL', overwrite=TRUE)

#########################
#Aggregate to 10' raster#
#########################
r10_whea_H <- aggregate(r5_whea_H, fact=2, fun=sum, na.rm=TRUE)
r10_whea_P <- aggregate(r5_whea_P, fact=2, fun=sum, na.rm=TRUE)

#Check to see 10' aggregate to the same whea
colSums(as.data.frame(r10_whea_H$whea), na.rm=TRUE) - sum(SPAM_whea_H$whea_a, na.rm=T)
colSums(as.data.frame(r10_whea_H$whea_h), na.rm=TRUE) - sum(SPAM_whea_H$whea_h, na.rm = T)
colSums(as.data.frame(r10_whea_H$whea_i), na.rm=TRUE) - sum(SPAM_whea_H$whea_i, na.rm=T)
colSums(as.data.frame(r10_whea_H$whea_l), na.rm=TRUE) - sum(SPAM_whea_H$whea_l+SPAM_whea_H$whea_s, na.rm = T)

plot(r10_whea_H)

#Save the raster files at 10'
writeRaster(r10_whea_H, filename="./Intermediate Data/SPAM_r10_whea_H.grd", bandorder='BIL', overwrite=TRUE)
writeRaster(r10_whea_P, filename="./Intermediate Data/SPAM_r10_whea_P.grd", bandorder='BIL', overwrite=TRUE)



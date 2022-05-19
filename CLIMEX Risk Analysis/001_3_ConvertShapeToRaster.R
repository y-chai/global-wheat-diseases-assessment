rm(list=ls())

library(maptools)
library(raster)
library(data.table)

#World raster
r5  <- raster(ext=extent(-180, 180, -90, 90), res=1/12)
values(r5)  <- seq(1:ncell(r5))-1

r10 <- raster(ext=extent(-180, 180, -90, 90), res=1/6)
values(r10) <- seq(1:ncell(r10))-1

#world polygon
data(wrld_simpl, package="maptools")
#wrld_simpl <- getData('countries')
wrld_simpl$RANK <- rank(wrld_simpl$ISO3)
#plot(wrld_simpl)

#Save the polygons as data.frame
wrld_simpl_df <- as.data.table(as.data.frame(wrld_simpl))[, .(RANK, ISO3, NAME)]

#Change world polygon to raster

###########
#5' raster#
###########
r5_cntr    <- rasterize(wrld_simpl,  r5, field="RANK")
r5_cntr    <- stack(r5, r5_cntr)
names(r5_cntr)  <- c("cell5m", "RANK")

# Convert to data.table
r5_cntr_df <- as.data.table(as.data.frame(r5_cntr))

# Add in RANK, ISO3 and NAME
r5_cntr_df <- merge(r5_cntr_df, wrld_simpl_df, by="RANK", all.x=TRUE)
setkey(r5_cntr_df, cell5m)
r5_cntr_df

# SPAM cell5m
SPAM_2010_H <- fread("S:/01Yuan Data/SPAM/spam2010V1r0_global_harv_area.csv/spam2010V1r0_global_H_TA.csv")
SPAM_2010_H_ISO <- SPAM_2010_H[, .(iso3, cell5m, whea_a)]

# Merge
r5_cntr_df <- merge(r5_cntr_df, SPAM_2010_H_ISO, by="cell5m", all.x=T)
r5_cntr_df[ is.na(ISO3) & !is.na(iso3), ISO3 := iso3]

r5_cntr_df$RANK <- NULL
r5_cntr_df$NAME <- NULL

r5_cntr_df <- merge(r5_cntr_df, wrld_simpl_df, by="ISO3", all.x=TRUE)

save(r5_cntr_df, file="./Global Country Raster/r5_cntr_clean.RData")

############
#10' raster#
############
r10_cntr    <- rasterize(wrld_simpl,  r10, field="RANK", fun="min", background=NA, mask=FALSE, na.rm=TRUE)
r10_cntr    <- stack(r10, r10_cntr)
names(r10_cntr)  <- c("cell10m", "RANK")

r10_cntr_df <- as.data.table(as.data.frame(r10_cntr))
r10_cntr_df <- merge(r10_cntr_df, wrld_simpl_df, by="RANK", all.x=TRUE)
setkey(r10_cntr_df, cell10m)
r10_cntr_df

save(r10_cntr_df, file="./Global Country Raster/r10_cntr_clean.RData")



# # #####
# # # 10' raster
# load("./Global Country Raster/r5_cntr_clean.RData")
# r10 <- raster(ext=extent(-180, 180, -90, 90), res=1/6)
# r10_cntr <- resample(r5_cntr, r10, method="ngb")
# r10_cntr_df <- as.data.table(as.data.frame(r10_cntr, xy=TRUE)) [, cell10m:=seq(1:ncell(r10))-1]
# 
# r10_cntr_df <- merge(r10_cntr_df, r5_cntr_df, by="cell5m", all.x=TRUE)
# 
# #r10_cntr_df <- merge(r10_cntr_df, wrld_simpl_df, by="RANK", all.x=TRUE)
# setkey(r10_cntr_df, cell10m)
# 
# save(r10_cntr_df, file="./Global Country Raster/r10_cntr_clean.RData")


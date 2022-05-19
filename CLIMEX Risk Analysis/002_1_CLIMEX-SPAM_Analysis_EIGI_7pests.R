rm(list=ls())

library(raster)
library(data.table)
library(ggplot2)

####
#### Choose EI or GI, Harvested Area (H) or Production (P) analysis ####
EG.sel <- "GI"
HP.sel <- "H"

#############
####SPAM#####
#############

###Start with 10' SPAM raster data

if (HP.sel == "H") {
  r10 <- brick("./Intermediate Data/SPAM_r10_whea_H.grd")
} else if(HP.sel == "P") {
  r10 <- brick("./Intermediate Data/SPAM_r10_whea_P.grd")
}

##############
####CLIMEX####
##############

# Read in processed CLIMEX data from above
# For GI/EI analysis, read in GI/EI files
CX_Pest <- fread(paste0("./Intermediate Data/CX_", EG.sel, ".csv"))

#####
#####Aligh SPAM with CLIMEX
#####
# Aggregated 10' SPAM data
# Note that cell10m starting from 0
SPAM_r10 <- as.data.table(as.data.frame(r10, xy=TRUE))[, cell10m := 0:(ncell(r10)-1)]
setkey(SPAM_r10, cell10m)

# Number of whea pixels
sum(SPAM_r10$whea >= 0)
sum(SPAM_r10$whea > 0)

#CLIMEX coordinate 10' raster cell number
# Note that cell10m starting from 0
CX_Pest$cell10m <- cellFromXY(r10, SpatialPoints(CX_Pest[, .(Longitude, Latitude)]))-1
setkey(CX_Pest, cell10m)

#Merge SPAM and CLIMEX by cell10m
SPAM_CX <- merge(SPAM_r10, CX_Pest, all=TRUE)
rm(list=c("SPAM_r10", "CX_Pest"))

#Add in Zone Info
#Read in SPAM data
# SPAM_whea_H <- fread("./Wheat SPAM 2005 v2/SPAM_whea_H_AB.csv")  #Read in as data.table
# sum(SPAM_CX$whea, na.rm=TRUE) - sum(SPAM_whea_H$whea, na.rm=TRUE)
# sum(SPAM_CX$whea_h, na.rm=TRUE) - sum(SPAM_whea_H$whea_h, na.rm=TRUE)
# sum(SPAM_CX$whea_i, na.rm=TRUE) - sum(SPAM_whea_H$whea_i, na.rm=TRUE)

# #Set region factor levels: Need to check the factors are matching
# SPAM_CX$region <- as.factor(SPAM_CX$region)
# levels(SPAM_CX$region) <- levels(as.factor(SPAM_whea_H$region))

# Add in country 
# load("./Global Country Raster/r10_cntr_clean.RData")
load("./Zone table/cc.rd")
#fix china country code
cc[cc$cntr_name=="China",]$FAOCODE=351


SPAM_CX <- merge(SPAM_CX, cc, all.x=T, by="cell10m")


## End Code from Jason ##


#Set country factor levels: Need to check the factors are matching
#Notice the after aggregate to 10', country "147 SGP" is missing from factor levels
# SPAM_CX$cntr <- as.factor(SPAM_CX$cntr)
# levels(SPAM_CX$cntr)[1:146] <- levels(as.factor(SPAM_whea_H$iso3))[1:146]
# levels(SPAM_CX$cntr)[147:182] <- levels(as.factor(SPAM_whea_H$iso3))[148:183]


####
#Create Irrigation Dummy
SPAM_CX[, dw := (whea>0)]
SPAM_CX[, di := (whea_i>0)]
SPAM_CX[, dh := (whea_h>0)]


#Mesh Natural and Irrigation for pests, under whea areas
SPAM_CX[,dcs := ((1-di)*dcs_NR+di*dcs_IG)*dw
        ][,dfg := ((1-di)*dfg_NR+di*dfg_IG)*dw
          ][,dmg := ((1-di)*dmg_NR+di*dmg_IG)*dw
            ][,dpg := ((1-di)*dpg_NR+di*dpg_IG)*dw
              ][,dps := ((1-di)*dps_NR+di*dps_IG)*dw
                ][,dpt := ((1-di)*dpt_NR+di*dpt_IG)*dw
                  ][,dst := ((1-di)*dst_NR+di*dst_IG)*dw]

#Mesh Natural and Irrigation for pests, under whea areas, for GI
SPAM_CX[,GI_cs := ((1-di)*GI_cs_NR+di*GI_cs_IG)*dw
        ][,GI_fg := ((1-di)*GI_fg_NR+di*GI_fg_IG)*dw
          ][,GI_mg := ((1-di)*GI_mg_NR+di*GI_mg_IG)*dw
            ][,GI_pg := ((1-di)*GI_pg_NR+di*GI_pg_IG)*dw
              ][,GI_ps := ((1-di)*GI_ps_NR+di*GI_ps_IG)*dw
                ][,GI_pt := ((1-di)*GI_pt_NR+di*GI_pt_IG)*dw
                  ][,GI_st := ((1-di)*GI_st_NR+di*GI_st_IG)*dw]


#Quick Check to make sure no overlaying categories
#summary( SPAM_CX[, .(Rust=dS+dY+dL+dSY+dYL+dLS+dSYL)] )

#Remove GI EI and intermediate dummy variables
SPAM_CX <- SPAM_CX[, which(!grepl(glob2rx("d*_??"), colnames(SPAM_CX))), with=FALSE]
SPAM_CX <- SPAM_CX[, which(!grepl(glob2rx("?I_??_??"), colnames(SPAM_CX))), with=FALSE]

# #Add in RustType Category
# SPAM_CX[, RustType := dS*1L + dY*2L + dL*3L + dSY*4L + dYL*5L + dLS*6L + dSYL*7L ]
# SPAM_CX[dw==1 & RustType==0, RustType:= 8L ]
# SPAM_CX[, RustType := as.factor(RustType)]
# 
# #Add RustType Layer in raster r10, notice that the RustType is shifted from 0~8 to 1~9
# r10$RustType <- as.factor(SPAM_CX$RustType)
# 
# #Export Raster for GIS mapping
# writeRaster(r10$RustType, file=paste0("./Intermediate Data/SPAM_CX_Rust_", EG.sel, ".asc"), overwrite=TRUE)
# 
# #Plot
# library(rworldmap)
# data("countriesLow")
# 
# par(xpd=FALSE)
# rustcolor <- c("white", "black", "yellow", "dark blue", "orange", "blue", "brown", "red", "green")
# plot(r10$RustType, breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), col=rustcolor, legend=FALSE)
# plot(countriesLow, add=TRUE)
# 
# par(xpd=TRUE)
# legend(x=190, y=80, legend=c("No Wheat", "Stem", "Yellow", "Leaf", "SY", "YL", "LS", "SYL", "No Rust"), fill=rustcolor,
#        cex=0.7, inset=0.9)


# Add in Pest Count Category
SPAM_CX[, PestCount := dcs + dfg + dmg + dpg + dps + dpt + dst]

# Add PestCount in raster r10
r10$GI_cs <- SPAM_CX$GI_cs
r10$GI_fg <- SPAM_CX$GI_fg
r10$GI_mg <- SPAM_CX$GI_mg
r10$GI_pg <- SPAM_CX$GI_pg
r10$GI_ps <- SPAM_CX$GI_ps
r10$GI_pt <- SPAM_CX$GI_pt
r10$GI_st <- SPAM_CX$GI_st
r10$PestCount <- SPAM_CX$PestCount


#Export Raster for GIS mapping
writeRaster(r10$PestCount, file=paste0("./Intermediate Data/SPAM_CX_7Pest_", EG.sel, ".asc"), overwrite=TRUE)
writeRaster(r10, file=paste0("./Intermediate Data/SPAM_CX_7Pest_", EG.sel, ".grd"), bandorder='BIL', overwrite=TRUE)

#Plot
library(rworldmap)
library(RColorBrewer)
data("countriesLow")

par(xpd=FALSE)
pal <- colorRampPalette(c("white", "orange", "red"))
plot(r10$PestCount, breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), col=pal(8), legend=FALSE)
plot(countriesLow, add=TRUE)

par(xpd=TRUE)
legend(x=190, y=80, legend=c("0", "1", "2", "3", "4", "5", "6", "7"), fill=pal(8),
       cex=0.7, inset=0.9)

#####
#####SPAM-CLIMEX aggregation table
#####


#Summary Statistics


##
#Add in Zone info
setkey (SPAM_CX, ISO3)

ISO_Zone_sim <- fread("./Zone table/ISO_Zone_sim_savary.csv")
setkey (ISO_Zone_sim, ISO3)

SPAM_CX_Zone <- merge(SPAM_CX, ISO_Zone_sim, by="ISO3", all.x=TRUE)

# Save data for further analysis
save(SPAM_CX_Zone, file=paste0("./Final Data/SPAM_CX_", HP.sel, "_", EG.sel, ".RData"))

#Aggregate by Zone

#High input + Irrigation areas
Zone_Pest_hi <- SPAM_CX_Zone[, TotWhea_h:= whea_h+whea_i
                       ][, .(TotWhea=sum(whea, na.rm=TRUE), TotWhea_h=sum(TotWhea_h, na.rm=TRUE),
                             cs=sum(dcs*TotWhea_h, na.rm=TRUE),
                             fg=sum(dfg*TotWhea_h, na.rm=TRUE),
                             mg=sum(dmg*TotWhea_h, na.rm=TRUE),
                             pg=sum(dpg*TotWhea_h, na.rm=TRUE),
                             ps=sum(dps*TotWhea_h, na.rm=TRUE),
                             pt=sum(dpt*TotWhea_h, na.rm=TRUE),
                             st=sum(dst*TotWhea_h, na.rm=TRUE)), by=Zone]
write.csv(Zone_Pest_hi, paste0("./Final Data/Zone_Pest_hi_", HP.sel, "_", EG.sel, ".csv"))

#All wheat areas
Zone_Pest_all <- SPAM_CX_Zone[, .(TotWhea=sum(whea, na.rm=TRUE),
                                  cs=sum(dcs*whea, na.rm=TRUE),
                                  fg=sum(dfg*whea, na.rm=TRUE),
                                  mg=sum(dmg*whea, na.rm=TRUE),
                                  pg=sum(dpg*whea, na.rm=TRUE),
                                  ps=sum(dps*whea, na.rm=TRUE),
                                  pt=sum(dpt*whea, na.rm=TRUE),
                                  st=sum(dst*whea, na.rm=TRUE)), by=Zone]
write.csv(Zone_Pest_all, paste0("./Final Data/Zone_Pest_all_", HP.sel, "_", EG.sel, ".csv"))



# Plots
# Heatmap for zone suitability
Zone_Pest_all_long <- melt(Zone_Pest_all, id.vars = "Zone", measure.vars = c("cs", "fg", "mg", "pg", "ps", "pt", "st"), 
                           variable.name = "Disease", value.name = paste0(EG.sel, "_", HP.sel))
Zone_Pest_all_long <- Zone_Pest_all_long[!is.na(Zone) & Zone != "Pacific"]
Zone_Pest_all_long <- merge(Zone_Pest_all_long, Zone_Pest_all[, .(Zone, TotWhea)], by="Zone")

ggplot(Zone_Pest_all_long) +
  geom_tile(aes(x=Disease, y=Zone, fill=GI_H/TotWhea)) +
  scale_fill_distiller(palette = "YlOrRd", trans = 'reverse')

# Summary of pest-co-suitability

#All wheat areas
Zone_PestCount_all <- SPAM_CX_Zone[, .(TotWhea=sum(whea, na.rm=TRUE)), by=.(Zone, PestCount)]
Zone_PestCount_all_wide <- dcast(Zone_PestCount_all, Zone ~ PestCount)
write.csv(Zone_PestCount_all_wide, paste0("./Final Data/Zone_PestCount_all_", HP.sel, "_", EG.sel, ".csv"))

#Aggregate by Region

#High input + Irrigation areas
Region_Pest_hi <- SPAM_CX_Zone[, TotWhea_h:= whea_h+whea_i
                             ][, .(TotWhea=sum(whea, na.rm=TRUE), TotWhea_h=sum(TotWhea_h, na.rm=TRUE),
                                   cs=sum(dcs*TotWhea_h, na.rm=TRUE),
                                   fg=sum(dfg*TotWhea_h, na.rm=TRUE),
                                   mg=sum(dmg*TotWhea_h, na.rm=TRUE),
                                   pg=sum(dpg*TotWhea_h, na.rm=TRUE),
                                   ps=sum(dps*TotWhea_h, na.rm=TRUE),
                                   pt=sum(dpt*TotWhea_h, na.rm=TRUE),
                                   st=sum(dst*TotWhea_h, na.rm=TRUE)), by=.(region_savary)]
write.csv(Region_Pest_hi, paste0("./Final Data/002_1_Region_Pest_hi_", HP.sel, "_", EG.sel, ".csv"))

#All wheat areas
Region_Pest_all <- SPAM_CX_Zone[, .(TotWhea=sum(whea, na.rm=TRUE),
                                  cs=sum(dcs*whea, na.rm=TRUE),
                                  fg=sum(dfg*whea, na.rm=TRUE),
                                  mg=sum(dmg*whea, na.rm=TRUE),
                                  pg=sum(dpg*whea, na.rm=TRUE),
                                  ps=sum(dps*whea, na.rm=TRUE),
                                  pt=sum(dpt*whea, na.rm=TRUE),
                                  st=sum(dst*whea, na.rm=TRUE)), by=.(region_savary)]
write.csv(Region_Pest_all, paste0("./Final Data/002_1_Region_Pest_all_", HP.sel, "_", EG.sel, ".csv"))

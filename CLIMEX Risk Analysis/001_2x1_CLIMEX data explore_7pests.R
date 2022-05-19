###These codes are used to
### Aggregate Different CLIMEX scenarios

rm(list=ls())

library(data.table)
library(ggplot2)
library(raster)


##############
####CLIMEX####
##############

# Read in CLIMEX files for 7 wheat diseases

# Natural rainfall
CX_cs_NR <- fread("K:/Shared drives/CLIMEX/CX_CM10_1975_world/Cochliobolus_sativus_Rainfall.csv")[, .(Location, Longitude, Latitude, EI_cs_NR=EI, GI_cs_NR=GI)]
CX_fg_NR <- fread("K:/Shared drives/CLIMEX/CX_CM10_1975_world/Fusarium_graminearum_Rainfall.csv")[, .(Location, Longitude, Latitude, EI_fg_NR=EI, GI_fg_NR=GI)]
CX_mg_NR <- fread("K:/Shared drives/CLIMEX/CX_CM10_1975_world/Magnoporthe_grisea_Rainfall.csv")[, .(Location, Longitude, Latitude, EI_mg_NR=EI, GI_mg_NR=GI)]
CX_pg_NR <- fread("K:/Shared drives/CLIMEX/CX_CM10_1975_world/Puccinia_graminis_Rainfall.csv")[, .(Location, Longitude, Latitude, EI_pg_NR=EI, GI_pg_NR=GI)]
CX_ps_NR <- fread("K:/Shared drives/CLIMEX/CX_CM10_1975_world/PUccinia_striiformis_Rainfall.csv")[, .(Location, Longitude, Latitude, EI_ps_NR=EI, GI_ps_NR=GI)]
CX_pt_NR <- fread("K:/Shared drives/CLIMEX/CX_CM10_1975_world/Puccinia_triticina_Rainfall.csv")[, .(Location, Longitude, Latitude, EI_pt_NR=EI, GI_pt_NR=GI)]
CX_st_NR <- fread("K:/Shared drives/CLIMEX/CX_CM10_1975_world/Septoria_tritici_Rainfall.csv")[, .(Location, Longitude, Latitude, EI_st_NR=EI, GI_st_NR=GI)]

# Irrigation
CX_cs_IG <- fread("K:/Shared drives/CLIMEX/CX_CM10_1975_world/Cochliobolus_sativus_Irrigation.csv")[, .(Location, Longitude, Latitude, EI_cs_IG=EI, GI_cs_IG=GI)]
CX_fg_IG <- fread("K:/Shared drives/CLIMEX/CX_CM10_1975_world/Fusarium_graminearum_Irrigation.csv")[, .(Location, Longitude, Latitude, EI_fg_IG=EI, GI_fg_IG=GI)]
CX_mg_IG <- fread("K:/Shared drives/CLIMEX/CX_CM10_1975_world/Magnoporthe_grisea_Irrigation.csv")[, .(Location, Longitude, Latitude, EI_mg_IG=EI, GI_mg_IG=GI)]
CX_pg_IG <- fread("K:/Shared drives/CLIMEX/CX_CM10_1975_world/Puccinia_graminis_Irrigation.csv")[, .(Location, Longitude, Latitude, EI_pg_IG=EI, GI_pg_IG=GI)]
CX_ps_IG <- fread("K:/Shared drives/CLIMEX/CX_CM10_1975_world/PUccinia_striiformis_Irrigation.csv")[, .(Location, Longitude, Latitude, EI_ps_IG=EI, GI_ps_IG=GI)]
CX_pt_IG <- fread("K:/Shared drives/CLIMEX/CX_CM10_1975_world/Puccinia_triticina_Irrigation.csv")[, .(Location, Longitude, Latitude, EI_pt_IG=EI, GI_pt_IG=GI)]
CX_st_IG <- fread("K:/Shared drives/CLIMEX/CX_CM10_1975_world/Septoria_tritici_Irrigation.csv")[, .(Location, Longitude, Latitude, EI_st_IG=EI, GI_st_IG=GI)]


# 
# #Check EI/GI match
# sum.cs <- CX_cs_NR[, .(EI_all=sum(EI>0), GI_all=sum(GI>5), EIGI=sum(EI>0 & GI>5), GI_only=sum(EI==0 & GI>5), CX="cs")]
# sum.fg <- CX_fg_NR[, .(EI_all=sum(EI>0), GI_all=sum(GI>5), EIGI=sum(EI>0 & GI>5), GI_only=sum(EI==0 & GI>5), CX="fg")]
# sum.mg <- CX_mg_NR[, .(EI_all=sum(EI>0), GI_all=sum(GI>5), EIGI=sum(EI>0 & GI>5), GI_only=sum(EI==0 & GI>5), CX="mg")]
# sum.pg <- CX_pg_NR[, .(EI_all=sum(EI>0), GI_all=sum(GI>5), EIGI=sum(EI>0 & GI>5), GI_only=sum(EI==0 & GI>5), CX="pg")]
# sum.ps <- CX_ps_NR[, .(EI_all=sum(EI>0), GI_all=sum(GI>5), EIGI=sum(EI>0 & GI>5), GI_only=sum(EI==0 & GI>5), CX="ps")]
# sum.pt <- CX_pt_NR[, .(EI_all=sum(EI>0), GI_all=sum(GI>5), EIGI=sum(EI>0 & GI>5), GI_only=sum(EI==0 & GI>5), CX="pt")]
# sum.st <- CX_st_NR[, .(EI_all=sum(EI>0), GI_all=sum(GI>5), EIGI=sum(EI>0 & GI>5), GI_only=sum(EI==0 & GI>5), CX="st")]
# sum.CX <- rbind(sum.cs, sum.fg, sum.mg, sum.pg, sum.ps, sum.pt, sum.st)
# 
# ggplot(sum.CX) +
#   geom_col(aes(x=CX, y=EIGI/GI_all, fill="EI/GI")) 

setkey(CX_cs_NR, Location, Longitude, Latitude)
setkey(CX_fg_NR, Location, Longitude, Latitude)
setkey(CX_mg_NR, Location, Longitude, Latitude)
setkey(CX_pg_NR, Location, Longitude, Latitude)
setkey(CX_ps_NR, Location, Longitude, Latitude)
setkey(CX_pt_NR, Location, Longitude, Latitude)
setkey(CX_st_NR, Location, Longitude, Latitude)

setkey(CX_cs_IG, Location, Longitude, Latitude)
setkey(CX_fg_IG, Location, Longitude, Latitude)
setkey(CX_mg_IG, Location, Longitude, Latitude)
setkey(CX_pg_IG, Location, Longitude, Latitude)
setkey(CX_ps_IG, Location, Longitude, Latitude)
setkey(CX_pt_IG, Location, Longitude, Latitude)
setkey(CX_st_IG, Location, Longitude, Latitude)

#Merge Natural Rainfall scenario
CX_NR <- merge(CX_cs_NR, CX_fg_NR)
CX_NR <- merge(CX_NR, CX_mg_NR)
CX_NR <- merge(CX_NR, CX_pg_NR)
CX_NR <- merge(CX_NR, CX_ps_NR)
CX_NR <- merge(CX_NR, CX_pt_NR)
CX_NR <- merge(CX_NR, CX_st_NR)

rm(list=ls(pattern="CX_[a-z]*_NR"))


#Merge Irrigation scenario
CX_IG <- merge(CX_cs_IG, CX_fg_IG)
CX_IG <- merge(CX_IG, CX_mg_IG)
CX_IG <- merge(CX_IG, CX_pg_IG)
CX_IG <- merge(CX_IG, CX_ps_IG)
CX_IG <- merge(CX_IG, CX_pt_IG)
CX_IG <- merge(CX_IG, CX_st_IG)

rm(list=ls(pattern="CX_[a-z]*_IG"))

####
####  Disease Categories
####

#Create dummies to represent each sub categories

#GI
CX_GI_NR <- copy(CX_NR) #Create a new copy, not reference to original data
CX_GI_NR[, dcs_NR:= (GI_cs_NR>=5)
         ][, dfg_NR:= (GI_fg_NR>=5)
           ][, dmg_NR:= (GI_mg_NR>=5)
             ][, dpg_NR:= (GI_pg_NR>=5)
               ][,dps_NR:= (GI_ps_NR>=5)
                 ][, dpt_NR:=(GI_pt_NR>=5)
                   ][, dst_NR:=(GI_st_NR>=5)]

CX_GI_IG <- copy(CX_IG) #Create a new copy, not reference to original data
CX_GI_IG[, dcs_IG:= (GI_cs_IG>=5)
         ][, dfg_IG:= (GI_fg_IG>=5)
           ][, dmg_IG:= (GI_mg_IG>=5)
             ][, dpg_IG:= (GI_pg_IG>=5)
               ][,dps_IG:= (GI_ps_IG>=5)
                 ][, dpt_IG:=(GI_pt_IG>=5)
                   ][, dst_IG:=(GI_st_IG>=5)]


#EI
CX_EI_NR <- copy(CX_NR) #Create a new copy, not reference to original data
CX_EI_NR[, dcs_NR:= (EI_cs_NR>0)
         ][, dfg_NR:= (EI_fg_NR>0)
           ][, dmg_NR:= (EI_mg_NR>0)
             ][, dpg_NR:= (EI_pg_NR>0)
               ][,dps_NR:= (EI_ps_NR>0)
                 ][, dpt_NR:=(EI_pt_NR>0)
                   ][, dst_NR:=(EI_st_NR>0)]

CX_EI_IG <- copy(CX_IG) #Create a new copy, not reference to original data
CX_EI_IG[, dcs_IG:= (EI_cs_IG>0)
         ][, dfg_IG:= (EI_fg_IG>0)
           ][, dmg_IG:= (EI_mg_IG>0)
             ][, dpg_IG:= (EI_pg_IG>0)
               ][,dps_IG:= (EI_ps_IG>0)
                 ][, dpt_IG:=(EI_pt_IG>0)
                   ][, dst_IG:=(EI_st_IG>0)]



#Merge NR and IG
setkey(CX_GI_NR, Location, Longitude, Latitude)
setkey(CX_GI_IG, Location, Longitude, Latitude)
CX_GI <- merge(CX_GI_NR, CX_GI_IG)

setkey(CX_EI_NR, Location, Longitude, Latitude)
setkey(CX_EI_IG, Location, Longitude, Latitude)
CX_EI <- merge(CX_EI_NR, CX_EI_IG)

rm(list=c("CX_GI_NR", "CX_GI_IG", "CX_EI_NR", "CX_EI_IG"))

#Check
sum((CX_GI$dpg_NR - CX_EI$dpg_NR)^2)
sum((CX_GI$dps_IG - CX_EI$dps_IG)^2)

#Save CLIMEX data
write.table(CX_GI, file="./Intermediate Data/CX_GI.csv", row.names=FALSE)
write.table(CX_EI, file="./Intermediate Data/CX_EI.csv", row.names=FALSE)


# #############
# ####SPAM#####
# #############
# 
# ###Start with 10' SPAM raster data
# 
# r10 <- brick("./Intermediate Data/SPAM_r10_whea_H.grd")
# 
# ##############
# ####CLIMEX####
# ##############
# 
# #####
# #####Aligh SPAM with CLIMEX
# #####
# # Aggregated 10' SPAM data
# # Note that cell10m starting from 0
# SPAM_r10 <- as.data.table(as.data.frame(r10, xy=TRUE))[, cell10m := 0:(ncell(r10)-1)]
# setkey(SPAM_r10, cell10m)
# 
# #CLIMEX coordinate 10' raster cell number
# 
# CX_Pest <- CX_NR
# rm(CX_NR)
# 
# # Note that cell10m starting from 0
# CX_Pest$cell10m <- cellFromXY(r10, SpatialPoints(CX_Pest[, .(Longitude, Latitude)]))-1
# setkey(CX_Pest, cell10m)
# 
# #Merge SPAM and CLIMEX by cell10m
# SPAM_CX <- merge(SPAM_r10, CX_Pest, all=TRUE)
# SPAM_CX[, d_whea := (whea >0)]
# rm(SPAM_r10)
# 
# # Summary
# 
# d.cutoff.GI <- 5
# 
# # Pixels
# sum.CX.all.pixel <- rbind(SPAM_CX[, .( cs = sum(EI_cs_NR >0 & GI_cs_NR>d.cutoff.GI, na.rm=T),
#                                  fg = sum(EI_fg_NR >0 & GI_fg_NR>d.cutoff.GI, na.rm=T),
#                                  mg = sum(EI_mg_NR >0 & GI_mg_NR>d.cutoff.GI, na.rm=T),
#                                  pg = sum(EI_pg_NR >0 & GI_pg_NR>d.cutoff.GI, na.rm=T),
#                                  ps = sum(EI_ps_NR >0 & GI_ps_NR>d.cutoff.GI, na.rm=T),
#                                  pt = sum(EI_pt_NR >0 & GI_pt_NR>d.cutoff.GI, na.rm=T),
#                                  st = sum(EI_st_NR >0 & GI_st_NR>d.cutoff.GI, na.rm=T),
#                                  CX="EI")],
#                           SPAM_CX[, .( cs = sum(EI_cs_NR ==0 & GI_cs_NR>d.cutoff.GI, na.rm=T),
#                                        fg = sum(EI_fg_NR ==0 & GI_fg_NR>d.cutoff.GI, na.rm=T),
#                                        mg = sum(EI_mg_NR ==0 & GI_mg_NR>d.cutoff.GI, na.rm=T),
#                                        pg = sum(EI_pg_NR ==0 & GI_pg_NR>d.cutoff.GI, na.rm=T),
#                                        ps = sum(EI_ps_NR ==0 & GI_ps_NR>d.cutoff.GI, na.rm=T),
#                                        pt = sum(EI_pt_NR ==0 & GI_pt_NR>d.cutoff.GI, na.rm=T),
#                                        st = sum(EI_st_NR ==0 & GI_st_NR>d.cutoff.GI, na.rm=T),
#                                        CX="GI")])
# names(sum.CX.all.pixel) <- c("Spot Blotch", "Fusarium Head Blight", "Wheat Blast", "Stem Rust", "Stripe Rust", "Leaf Rust", "Tritici Blotch", "CX")
# sum.CX.all.pixel <- melt(sum.CX.all.pixel, id.vars = "CX", variable.name = "Pest", value.name = "Pixels")
# 
# ggplot(sum.CX.all.pixel) +
#   geom_col(aes(x=Pest, y=Pixels, fill=CX)) +
#   ylab("EI/GI Pixels") +
#   coord_flip() +
#   theme_bw()
# 
# # Wheat Area
# sum.CX.whea.area <- rbind(SPAM_CX[, .( cs = sum(whea*(EI_cs_NR >0 & GI_cs_NR>d.cutoff.GI), na.rm=T),
#                                  fg = sum(whea*(EI_fg_NR >0 & GI_fg_NR>d.cutoff.GI), na.rm=T),
#                                  mg = sum(whea*(EI_mg_NR >0 & GI_mg_NR>d.cutoff.GI), na.rm=T),
#                                  pg = sum(whea*(EI_pg_NR >0 & GI_pg_NR>d.cutoff.GI), na.rm=T),
#                                  ps = sum(whea*(EI_ps_NR >0 & GI_ps_NR>d.cutoff.GI), na.rm=T),
#                                  pt = sum(whea*(EI_pt_NR >0 & GI_pt_NR>d.cutoff.GI), na.rm=T),
#                                  st = sum(whea*(EI_st_NR >0 & GI_st_NR>d.cutoff.GI), na.rm=T),
#                                  CX="EI")],
#                           SPAM_CX[, .( cs = sum(whea*(EI_cs_NR ==0 & GI_cs_NR>d.cutoff.GI), na.rm=T),
#                                        fg = sum(whea*(EI_fg_NR ==0 & GI_fg_NR>d.cutoff.GI), na.rm=T),
#                                        mg = sum(whea*(EI_mg_NR ==0 & GI_mg_NR>d.cutoff.GI), na.rm=T),
#                                        pg = sum(whea*(EI_pg_NR ==0 & GI_pg_NR>d.cutoff.GI), na.rm=T),
#                                        ps = sum(whea*(EI_ps_NR ==0 & GI_ps_NR>d.cutoff.GI), na.rm=T),
#                                        pt = sum(whea*(EI_pt_NR ==0 & GI_pt_NR>d.cutoff.GI), na.rm=T),
#                                        st = sum(whea*(EI_st_NR ==0 & GI_st_NR>d.cutoff.GI), na.rm=T),
#                                        CX="GI")],
#                           SPAM_CX[, .( cs = sum(whea*( GI_cs_NR<=d.cutoff.GI), na.rm=T),
#                                        fg = sum(whea*( GI_fg_NR<=d.cutoff.GI), na.rm=T),
#                                        mg = sum(whea*( GI_mg_NR<=d.cutoff.GI), na.rm=T),
#                                        pg = sum(whea*( GI_pg_NR<=d.cutoff.GI), na.rm=T),
#                                        ps = sum(whea*( GI_ps_NR<=d.cutoff.GI), na.rm=T),
#                                        pt = sum(whea*( GI_pt_NR<=d.cutoff.GI), na.rm=T),
#                                        st = sum(whea*( GI_st_NR<=d.cutoff.GI), na.rm=T),
#                                        CX="N/S")])
# names(sum.CX.whea.area) <- c("Spot Blotch", "Fusarium Head Blight", "Wheat Blast", "Stem Rust", "Stripe Rust", "Leaf Rust", "Tritici Blotch", "CX")
# sum.CX.whea.area <- melt(sum.CX.whea.area, id.vars = "CX", variable.name = "Pest", value.name = "Area")
# 
# ggplot(sum.CX.whea.area) +
#   geom_col(aes(x=Pest, y=Area, fill=CX)) +
#   ylab("EI/GI Area") +
#   scale_fill_manual(values=c("#d7191c", "#2c7bb6", "#a6d96a")) +
#   coord_flip() +
#   theme_bw()
# 
# # Pixel Shares
# sum.CX.all.share <- SPAM_CX[, .( r_EIGI_cs = sum(EI_cs_NR >0 & GI_cs_NR>d.cutoff.GI, na.rm=T) / sum(GI_cs_NR>d.cutoff.GI, na.rm=T),
#                            r_EIGI_fg = sum(EI_fg_NR >0 & GI_fg_NR>d.cutoff.GI, na.rm=T) / sum(GI_fg_NR>d.cutoff.GI, na.rm=T),
#                            r_EIGI_mg = sum(EI_mg_NR >0 & GI_mg_NR>d.cutoff.GI, na.rm=T) / sum(GI_mg_NR>d.cutoff.GI, na.rm=T),
#                            r_EIGI_pg = sum(EI_pg_NR >0 & GI_pg_NR>d.cutoff.GI, na.rm=T) / sum(GI_pg_NR>d.cutoff.GI, na.rm=T),
#                            r_EIGI_ps = sum(EI_ps_NR >0 & GI_ps_NR>d.cutoff.GI, na.rm=T) / sum(GI_ps_NR>d.cutoff.GI, na.rm=T),
#                            r_EIGI_pt = sum(EI_pt_NR >0 & GI_pt_NR>d.cutoff.GI, na.rm=T) / sum(GI_pt_NR>d.cutoff.GI, na.rm=T),
#                            r_EIGI_st = sum(EI_st_NR >0 & GI_st_NR>d.cutoff.GI, na.rm=T) / sum(GI_st_NR>d.cutoff.GI, na.rm=T),
#                            SPAM="all pixel")]
# # Wheat Area
# sum.CX.whea.share <- SPAM_CX[, .( r_EIGI_cs = sum(whea*(EI_cs_NR >0 & GI_cs_NR>d.cutoff.GI), na.rm=T) / sum(whea*(GI_cs_NR>d.cutoff.GI), na.rm=T),
#                             r_EIGI_fg = sum(whea*(EI_fg_NR >0 & GI_fg_NR>d.cutoff.GI), na.rm=T) / sum(whea*(GI_fg_NR>d.cutoff.GI), na.rm=T),
#                             r_EIGI_mg = sum(whea*(EI_mg_NR >0 & GI_mg_NR>d.cutoff.GI), na.rm=T) / sum(whea*(GI_mg_NR>d.cutoff.GI), na.rm=T),
#                             r_EIGI_pg = sum(whea*(EI_pg_NR >0 & GI_pg_NR>d.cutoff.GI), na.rm=T) / sum(whea*(GI_pg_NR>d.cutoff.GI), na.rm=T),
#                             r_EIGI_ps = sum(whea*(EI_ps_NR >0 & GI_ps_NR>d.cutoff.GI), na.rm=T) / sum(whea*(GI_ps_NR>d.cutoff.GI), na.rm=T),
#                             r_EIGI_pt = sum(whea*(EI_pt_NR >0 & GI_pt_NR>d.cutoff.GI), na.rm=T) / sum(whea*(GI_pt_NR>d.cutoff.GI), na.rm=T),
#                             r_EIGI_st = sum(whea*(EI_st_NR >0 & GI_st_NR>d.cutoff.GI), na.rm=T) / sum(whea*(GI_st_NR>d.cutoff.GI), na.rm=T),
#                             SPAM="wheat area")]
# 
# sum.CX.share <- rbind(sum.CX.all.share, sum.CX.whea.share)
# 
# names(sum.CX.share) <- c("Spot Blotch", "Fusarium Head Blight", "Wheat Blast", "Stem Rust", "Stripe Rust", "Leaf Rust", "Tritici Blotch", "SPAM")
# sum.CX.share.plot <- melt(sum.CX.share, id.vars = "SPAM", variable.name = "Pest", value.name = "r_EI_GI")
# 
# ggplot(sum.CX.share.plot) +
#   geom_col(aes(x=Pest, y=r_EI_GI, fill=SPAM), position = "dodge") +
#   ylab("EI/GI share") +
#   coord_flip() +
#   theme_bw()
# 
# # Suitable Wheat Area
# sum.CX.GI.whea <- SPAM_CX[, .( r_GI_cs = sum(whea*(GI_cs_NR>d.cutoff.GI), na.rm=T) / sum(whea, na.rm=T),
#                                r_GI_fg = sum(whea*(GI_fg_NR>d.cutoff.GI), na.rm=T) / sum(whea, na.rm=T),
#                                r_GI_mg = sum(whea*(GI_mg_NR>d.cutoff.GI), na.rm=T) / sum(whea, na.rm=T),
#                                r_GI_pg = sum(whea*(GI_pg_NR>d.cutoff.GI), na.rm=T) / sum(whea, na.rm=T),
#                                r_GI_ps = sum(whea*(GI_ps_NR>d.cutoff.GI), na.rm=T) / sum(whea, na.rm=T),
#                                r_GI_pt = sum(whea*(GI_pt_NR>d.cutoff.GI), na.rm=T) / sum(whea, na.rm=T),
#                                r_GI_st = sum(whea*(GI_st_NR>d.cutoff.GI), na.rm=T) / sum(whea, na.rm=T),
#                                SPAM="wheat area")]
# names(sum.CX.GI.whea) <- c("Spot Blotch", "Fusarium Head Blight", "Wheat Blast", "Stem Rust", "Stripe Rust", "Leaf Rust", "Tritici Blotch", "SPAM")
# sum.CX.GI.whea <- melt(sum.CX.GI.whea, id.vars = "SPAM", variable.name = "Pest", value.name = "r_GI")
# 
# ggplot(sum.CX.GI.whea) +
#   geom_col(aes(x=Pest, y=r_GI)) +
#   ylab("Wheat GI Area Share") +
#   coord_flip() +
#   theme_bw()
# 
# 
# # Suitable Wheat Area
# sum.CX.GI.whea <- SPAM_CX[, .( r_GI_cs = sum(whea*(GI_cs_NR>d.cutoff.GI), na.rm=T),
#                                r_GI_fg = sum(whea*(GI_fg_NR>d.cutoff.GI), na.rm=T),
#                                r_GI_mg = sum(whea*(GI_mg_NR>d.cutoff.GI), na.rm=T),
#                                r_GI_pg = sum(whea*(GI_pg_NR>d.cutoff.GI), na.rm=T),
#                                r_GI_ps = sum(whea*(GI_ps_NR>d.cutoff.GI), na.rm=T),
#                                r_GI_pt = sum(whea*(GI_pt_NR>d.cutoff.GI), na.rm=T),
#                                r_GI_st = sum(whea*(GI_st_NR>d.cutoff.GI), na.rm=T),
#                                SPAM="wheat area")]
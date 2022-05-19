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

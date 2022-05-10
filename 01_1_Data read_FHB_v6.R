# Read in US FHB Loss data
# Author: Yuan Chai
# Email: chaix026@umn.edu

# Clear workspace
rm(list=ls())

# Load library
library(data.table)

# ---- Read in data ----
fhb.raw <- fread(file="./Ori data/FHB data/FHB_US.csv")
# code NA as 0
fhb.raw[ is.na(Losses_000bu), Losses_000bu := 0 ]


# ---- aggregate by state by year (across Winter, Spring and Durum wheat types) ----

fhb.all <- fhb.raw[, .(Prod1000Bu = sum(Prod_bu) /1000,
                       FHB1000Bu = sum(Losses_000bu)),
                   by=.(State, Year)]

fhb.all[, `:=`(FHBPer = (FHB1000Bu / (Prod1000Bu + FHB1000Bu)))]

# Add state info
US.state.name <- data.frame(state=state.name, state_abb=state.abb)
fhb.all <- merge(fhb.all, US.state.name, by.x="State", by.y="state_abb", all.x=TRUE)


# Final data
fhb.state <- fhb.all[, .(state=state, state_abb=State, year=Year, FHBPer, FHB1000Bu, Prod1000Bu)]

# Save
save(fhb.state, file="./Mid data/fhb.state.Rdata")
write.csv(fhb.state, file="./Mid data/fhb.state.csv", row.names = F)

# State loss data histogram
hist(fhb.state$FHBPer)

fhb.breaks <- seq(0, max(fhb.state$FHBPer)+0.01, by=0.01)
fhb.bin.num <- length(fhb.breaks) - 1
fhb.bins <- cut(fhb.state$FHBPer, breaks = fhb.breaks, include.lowest = T, right = F)
fhb.bins.sum <- data.frame(lower.include = fhb.breaks[1:fhb.bin.num], upper.exclude= fhb.breaks[2:(fhb.bin.num+1)], count=summary(fhb.bins))


write.csv(fhb.bins.sum, file="./Mid data/bins.sum.fhb.csv", row.names = F)

# --- aggregate by year (all US) ----
fhb.US <- fhb.all[, .(Prod1000Bu = sum(Prod1000Bu),
                      FHB1000Bu = sum(FHB1000Bu)), 
                    by=.(Year)]
fhb.US <- fhb.US[order(Year)]

fhb.US[, `:=`(FHBPer = (FHB1000Bu / (Prod1000Bu + FHB1000Bu)))]

# Save
save(fhb.US, file="./Mid data/fhb.US.Rdata")
write.csv(fhb.US, file="./Fin data/fhb.US.csv", row.names = F)



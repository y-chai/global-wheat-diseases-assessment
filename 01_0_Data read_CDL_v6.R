# Read in CDL Rust Loss data #
# Author: Yuan Chai 
# Email: chaix026@umn.edu

# Clear workspace
rm(list=ls())

# Load library
library(data.table)

# ---- Read in data ----
rust.raw <- fread(file="./Ori data/CDL data/WheatRustCDL20210219.csv")

# ---- aggregate by state by year (across Winter, Spring and Durum wheat types) ----
rust.raw[, `:=`(StemBu = Prod1000Bu*1000/(100-StemPer)*StemPer,
                StripeBu = Prod1000Bu*1000/(100-StripePer)*StripePer,
                LeafBu = Prod1000Bu*1000/(100-LeafPer)*LeafPer)]

rust.all <- rust.raw[, .(Prod1000Bu = sum(Prod1000Bu),
                             Acre1000 = sum(Acre1000),
                             Stem1000Bu = sum(StemBu/1000),
                             Stripe1000Bu = sum(StripeBu/1000),
                             Leaf1000Bu = sum(LeafBu/1000)), 
                         by=.(State, Year)]
# rust.all[, Prod1000Bu_RustFree := Prod1000Bu + Stem1000Bu + Stripe1000Bu + Leaf1000Bu]

rust.all[, `:=`(StemPer = (Stem1000Bu / (Prod1000Bu + Stem1000Bu)),
                    StripePer = (Stripe1000Bu / (Prod1000Bu + Stripe1000Bu)),
                    LeafPer = (Leaf1000Bu / (Prod1000Bu + Leaf1000Bu)))]

# Add state info
US.state.name <- data.frame(state=state.name, state_abb=state.abb)
rust.all <- merge(rust.all, US.state.name, by.x="State", by.y="state", all.x=TRUE)


# Final data
rust.state <- rust.all[, .(state=State, state_abb, year=Year, StemPer, StripePer, LeafPer)]

# Save
save(rust.state, file="./Mid data/rust.state.Rdata")


# --- aggregate by year (all US) ----
rust.US <- rust.all[, .(Prod1000Bu = sum(Prod1000Bu),
                        Acre1000 = sum(Acre1000),
                        Stem1000Bu = sum(Stem1000Bu),
                        Stripe1000Bu = sum(Stripe1000Bu),
                        Leaf1000Bu = sum(Leaf1000Bu)), 
                    by=.(Year)]
rust.US <- rust.US[order(Year)]

# rust.US[, Prod1000Bu_RustFree := Prod1000Bu + Stem1000Bu + Stripe1000Bu + Leaf1000Bu]
rust.US[, `:=`(StemPer = (Stem1000Bu / (Prod1000Bu + Stem1000Bu)),
               StripePer = (Stripe1000Bu / (Prod1000Bu + Stripe1000Bu)),
               LeafPer = (Leaf1000Bu / (Prod1000Bu + Leaf1000Bu)))]

# Save
save(rust.US, file="./Mid data/rust.US.Rdata")
write.csv(rust.US, file="./Fin data/rust.US.csv", row.names = F)

# Histgram
hist(rust.US$StemPer)

fx.bins <- function(loss.per, bin.width=0.01) {
  loss.breaks <- seq(0, max(loss.per)+0.01, by=0.01)
  loss.bin.num <- length(loss.breaks) - 1
  loss.bins <- cut(loss.per, breaks = loss.breaks, include.lowest = T, right = F)
  loss.bins.sum <- data.frame(lower.include = loss.breaks[1:loss.bin.num], 
                              upper.exclude= loss.breaks[2:(loss.bin.num+1)], 
                              count=summary(loss.bins))
}

bins.sum.stem <- fx.bins(rust.US$StemPer)
bins.sum.stripe <- fx.bins(rust.US$StripePer)
bins.sum.leaf <- fx.bins(rust.US$LeafPer)

write.csv(bins.sum.stem, file="./Mid data/bins.sum.stem.csv", row.names = F)
write.csv(bins.sum.stripe, file="./Mid data/bins.sum.stripe.csv", row.names = F)
write.csv(bins.sum.leaf, file="./Mid data/bins.sum.leaf.csv", row.names = F)


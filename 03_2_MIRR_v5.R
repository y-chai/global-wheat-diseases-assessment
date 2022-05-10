rm(list = ls())

library(data.table)
library(ggplot2)

# Load in results
load("./Fin Data/03_0_loss.sim_v5_US.high.RData")
load("./Fin Data/03_0_loss.sim_v5_US.low.RData")
loss.sim.A <- loss.sim.US.high
# loss.sim.A <- loss.sim.US.low

# load("./Fin Data/03_0_loss.sim_v3_savary.RData")
# loss.sim.A <- loss.sim.savary

# Read in price info
price <- fread("./Ori Data/FAO data/wheat_price_b2018.csv")
price.use <- as.vector(price[yr %in% 1961:2050, .(price=r.P.Ton.b2018)]$price)
price.sim <- matrix(price.use, nrow=length(price.use), ncol=ncol(loss.sim.A$T), byrow = F)

# Add in price
# Value of loss
loss.value.A <- loss.sim.A$T * price.sim


fx.summary <- function(x) {
  df.quantil <- quantile(x, probs = c(0.1, 0.50, 0.8, 0.95))
  df.mean <- mean(x)
  df.sum  <- c(df.quantil, mean = df.mean)
  
  return(df.sum)
}

sum.sel <- 60:90
yr.avg.A <- colMeans(loss.value.A[sum.sel, ])

my.statistics <- c("90%", "50%", "20%", "5%", "Mean")
value.report.A  <- data.table(scenario = "A", my.statistics,  Value = fx.summary(yr.avg.A))

write.csv (value.report.A,
           file="./Fin Data/03_2_Table_Value Summary_v5_High.csv", row.names = F)

# Interests rates
rc <- 0.1
rr <- 0.03
rm <- 0.1

# Future value of benefits
rr.yr <- 2020:2050
rr.vec <- (1+rr)^(2050-rr.yr)

fx.FVB <- function(loss.value) {
  as.vector( rr.vec %*% tail(loss.value, length(rr.yr) ) )
}

FVB.A <- fx.FVB(loss.value.A)
  
# Present value of costs
fx.PVC <- function(cost, start.yr=2010, end.yr=2050) {
  PVC <- sum(cost / ((1+rc)^(start.yr:end.yr - start.yr)))
  return(PVC)
}

# MIRR
fx.MIRR <- function(cost, FVB, start.yr=2010, end.yr=2050) {
  PVC <- fx.PVC(cost, start.yr, end.yr)
  MIRR <- (FVB/PVC)^(1/(end.yr-start.yr)) - 1
  return(MIRR)
}

# Objective
fx.Obj <- function(x, FVB) {
  MIRR.cutoff <- as.numeric(quantile(fx.MIRR(cost=x, FVB), 0.05))
  Obj <- abs(MIRR.cutoff-rm)
  return(Obj)
}

# Economically justified investment
Opt.Invest.A <- optimize(fx.Obj, c(1e+8, 1e+10), FVB=FVB.A)
Opt.res.A <- Opt.Invest.A$minimum
Opt.res.A


write.csv(data.frame(scenario = "A", 
                     investment = Opt.res.A), 
          file="./Fin Data/03_2_Table_MIRR_v5_High.csv",
          row.names = F)



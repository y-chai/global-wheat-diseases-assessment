# Loss simulation based on conditional beta distributions
# Author: Yuan Chai
# Email: chaix026@umn.edu

# clear workspace
rm(list = ls())

# load libraries
library(data.table)
library(ggplot2)

####################################

# Read in FAO data for prod and area
prod <- fread("./Ori Data/FAO data/wheat_prod.csv")
area <- fread("./Ori Data/FAO data/wheat_area.csv")

# Change data to long format
prod.L <- melt(prod, id.vars="yr", variable.name = "zone", variable.factor =F, value.name = "prod")
area.L <- melt(area, id.vars="yr", variable.name = "zone", variable.factor =F, value.name = "area")
yield.L <- merge(prod.L, area.L, by=c("yr", "zone")) [, yield := prod/area]

# Save data
save(prod.L, file="./Mid Data/03_0_prod.L.RData")

rm(prod, prod.L, area, area.L)


# Read in CLIMEX zone GIEI data
Pest.GI <- fread("./Ori data/CLIMEX output/Zone_Pest_hi_H_GI.csv")[, V1 := NULL ]
Pest.GI[, `:=` (area.hi.shr = TotWhea_h / TotWhea,
                cs.shr = cs/TotWhea_h,
                fg.shr = fg/TotWhea_h,
                mg.shr = mg/TotWhea_h,
                pg.shr = pg/TotWhea_h,
                ps.shr = ps/TotWhea_h,
                pt.shr = pt/TotWhea_h,
                st.shr = st/TotWhea_h)]

# Merge data
dat.atrisk <- merge(yield.L, Pest.GI[, .(zone = Zone, area.hi.shr, cs.shr, fg.shr, mg.shr, pg.shr,
                                         ps.shr, pt.shr, st.shr)], by="zone", all.x=TRUE) 
dat.atrisk.ori <- as.data.table(dat.atrisk)
rm(yield.L)


#####################################

# Simulation

# Pest Losses

# load("./Mid Data/02_1_dat.beta.pest_v2.RData")
dat.beta.pest <- fread("./Ori Data/gmm/beta_gmm_v2.csv")


param.pest.US.high <- list(a_cs=dat.beta.pest$a.WLD[dat.beta.pest$pest == "cs"], b_cs=dat.beta.pest$b.WLD[dat.beta.pest$pest == "cs"], 
                      a_fg=dat.beta.pest$a.US.high[dat.beta.pest$pest == "fg"], b_fg=dat.beta.pest$b.US.high[dat.beta.pest$pest == "fg"],
                      m_mg = 0,
                      a_mg_SBA=dat.beta.pest$a.WLD[dat.beta.pest$pest == "mg_SBA"], b_mg_SBA=dat.beta.pest$b.WLD[dat.beta.pest$pest == "mg_SBA"],
                      a_mg_IGP=dat.beta.pest$a.WLD[dat.beta.pest$pest == "mg_IGP"], b_mg_IGP=dat.beta.pest$b.WLD[dat.beta.pest$pest == "mg_IGP"],
                      a_pg=dat.beta.pest$a.US.high[dat.beta.pest$pest == "pg"], b_pg=dat.beta.pest$b.US.high[dat.beta.pest$pest == "pg"],
                      a_ps=dat.beta.pest$a.US.high[dat.beta.pest$pest == "ps"], b_ps=dat.beta.pest$b.US.high[dat.beta.pest$pest == "ps"],
                      a_pt=dat.beta.pest$a.US[dat.beta.pest$pest == "pt"], b_pt=dat.beta.pest$b.US[dat.beta.pest$pest == "pt"],
                      a_st=dat.beta.pest$a.WLD[dat.beta.pest$pest == "st"], b_st=dat.beta.pest$b.WLD[dat.beta.pest$pest == "st"])

param.pest.US.low <- list(a_cs=dat.beta.pest$a.WLD[dat.beta.pest$pest == "cs"], b_cs=dat.beta.pest$b.WLD[dat.beta.pest$pest == "cs"], 
                           a_fg=dat.beta.pest$a.US.low[dat.beta.pest$pest == "fg"], b_fg=dat.beta.pest$b.US.low[dat.beta.pest$pest == "fg"],
                           m_mg = 0,
                           a_mg_SBA=dat.beta.pest$a.WLD[dat.beta.pest$pest == "mg_SBA"], b_mg_SBA=dat.beta.pest$b.WLD[dat.beta.pest$pest == "mg_SBA"],
                           a_mg_IGP=dat.beta.pest$a.WLD[dat.beta.pest$pest == "mg_IGP"], b_mg_IGP=dat.beta.pest$b.WLD[dat.beta.pest$pest == "mg_IGP"],
                           a_pg=dat.beta.pest$a.US.low[dat.beta.pest$pest == "pg"], b_pg=dat.beta.pest$b.US.low[dat.beta.pest$pest == "pg"],
                           a_ps=dat.beta.pest$a.US.low[dat.beta.pest$pest == "ps"], b_ps=dat.beta.pest$b.US.low[dat.beta.pest$pest == "ps"],
                           a_pt=dat.beta.pest$a.US[dat.beta.pest$pest == "pt"], b_pt=dat.beta.pest$b.US[dat.beta.pest$pest == "pt"],
                           a_st=dat.beta.pest$a.WLD[dat.beta.pest$pest == "st"], b_st=dat.beta.pest$b.WLD[dat.beta.pest$pest == "st"])

param.pest.Savary <- list(cs=0.0167,
                       fg=0.0285,
                       mg = 0, 
                       mg_SBA=0.0352, 
                       mg_IGP=0.0002, 
                       pg=0.009, 
                       ps=0.0208, 
                       pt=0.0325, 
                       st=0.0244)


# Simulation Setup
set.seed(1234)
rep.N <- 5000
obs.N <- nrow(dat.atrisk)
yr.N <- length(unique(dat.atrisk$yr))

fx.loss.sim <- function(N, distri, param) {
  
  # Empty list to store results
  loss.sim.df <- matrix(NA, nrow = yr.N, ncol = rep.N)
  rownames(loss.sim.df) <- unique(dat.atrisk$yr)
  colnames(loss.sim.df) <- paste0("rep", 1:rep.N)
  loss.sim.ls <-  list(T=loss.sim.df, cs=loss.sim.df, fg=loss.sim.df, mg=loss.sim.df,
                       pg=loss.sim.df, ps=loss.sim.df, pt=loss.sim.df, st=loss.sim.df)

  # Simulations iterations
  for (i in 1:rep.N) {
    
    # For beta distribution
    
    if ( distri == "beta") {
      # Simulate losses, accounting for the suitabile area share
      loss.rep <- dat.atrisk[, `:=` (cs.l= rbeta(N, param$a_cs, param$b_cs) * cs.shr,
                                     fg.l= rbeta(N, param$a_fg, param$b_fg) * fg.shr,
                                     mg.l= param$m_mg * mg.shr,
                                     pg.l= rbeta(N, param$a_pg, param$b_pg) * pg.shr,
                                     ps.l= rbeta(N, param$a_ps, param$b_ps) * ps.shr,
                                     pt.l= rbeta(N, param$a_pt, param$b_pt) * pt.shr,
                                     st.l= rbeta(N, param$a_st, param$b_st) * st.shr)]
      SBA_N <- nrow(loss.rep[zone == "ESouthAm"])
      loss.rep[zone == "ESouthAm", mg.l := rbeta(SBA_N, param$a_mg_SBA, param$b_mg_SBA) * mg.shr]
      
      IGP_N <- nrow(loss.rep[zone == "SouthWestAsia"])
      loss.rep[zone == "SouthWestAsia", mg.l := rbeta(IGP_N, param$a_mg_IGP, param$b_mg_IGP)* mg.shr]
      
    } else if ( distri == "average") {
      # Simulate losses, accounting for the suitabile area share
      loss.rep <- dat.atrisk[, `:=` (cs.l= param$cs * cs.shr,
                                     fg.l= param$fg * fg.shr,
                                     mg.l= param$mg * mg.shr,
                                     pg.l= param$pg * pg.shr,
                                     ps.l= param$ps * ps.shr,
                                     pt.l= param$pt * pt.shr,
                                     st.l= param$st * st.shr)]
      SBA_N <- nrow(loss.rep[zone == "ESouthAm"])
      loss.rep[zone == "ESouthAm", mg.l := param$mg_SBA * mg.shr]
      
      IGP_N <- nrow(loss.rep[zone == "SouthWestAsia"])
      loss.rep[zone == "SouthWestAsia", mg.l := param$mg_IGP* mg.shr]
    }
    
    # Calcuate losses, using the multiplicative rule to account for the multi-pest accounting
    loss.rep[, `:=` (loss.add = cs.l + fg.l + mg.l + pg.l + ps.l + pt.l + st.l,
                     loss.shr = 1- (1-cs.l)*(1-fg.l)*(1-mg.l)*(1-pg.l)*(1-ps.l)*(1-pt.l)*(1-st.l) )
             ][, rust.free.prod := yield*(1/(1-area.hi.shr*loss.shr))*area]
    
    # Distribute losses using the simulated loss
    loss.rep <- loss.rep[, `:=`(T.loss = rust.free.prod - prod)
                         ][, `:=` (cs.loss = T.loss*cs.l/loss.add,
                                   fg.loss = T.loss*fg.l/loss.add,
                                   mg.loss = T.loss*mg.l/loss.add,
                                   pg.loss = T.loss*pg.l/loss.add,
                                   ps.loss = T.loss*ps.l/loss.add,
                                   pt.loss = T.loss*pt.l/loss.add,
                                   st.loss = T.loss*st.l/loss.add)
                           ][, .(T.loss.tot = sum(T.loss, na.rm=T),
                                 cs.loss.tot = sum(cs.loss, na.rm=T),
                                 fg.loss.tot = sum(fg.loss, na.rm=T),
                                 mg.loss.tot = sum(mg.loss, na.rm=T),
                                 pg.loss.tot = sum(pg.loss, na.rm=T),
                                 ps.loss.tot = sum(ps.loss, na.rm=T),
                                 pt.loss.tot = sum(pt.loss, na.rm=T),
                                 st.loss.tot = sum(st.loss, na.rm=T)), by=.(yr)]
    loss.sim.ls[["T"]][, i] <- loss.rep$T.loss.tot
    loss.sim.ls[["cs"]][, i] <- loss.rep$cs.loss.tot
    loss.sim.ls[["fg"]][, i] <- loss.rep$fg.loss.tot
    loss.sim.ls[["mg"]][, i] <- loss.rep$mg.loss.tot
    loss.sim.ls[["pg"]][, i] <- loss.rep$pg.loss.tot
    loss.sim.ls[["ps"]][, i] <- loss.rep$ps.loss.tot
    loss.sim.ls[["pt"]][, i] <- loss.rep$pt.loss.tot
    loss.sim.ls[["st"]][, i] <- loss.rep$st.loss.tot
  }
  
  return(loss.sim.ls)
  
}

# Simulation results


# Savary
loss.sim.savary <- fx.loss.sim(obs.N, "average", param.pest.Savary)
save(loss.sim.savary, file="./Fin Data/03_0_loss.sim_v5_savary.RData")

# All time average loss using US beta-distribution, High-loss scenario
loss.sim.US.high <- fx.loss.sim(obs.N, "beta", param.pest.US.high)
save(loss.sim.US.high, file="./Fin Data/03_0_loss.sim_v5_US.high.RData")

# All time average loss using US beta-distribution, Low-loss scenario
loss.sim.US.low <- fx.loss.sim(obs.N, "beta", param.pest.US.low)
save(loss.sim.US.low, file="./Fin Data/03_0_loss.sim_v5_US.low.RData")
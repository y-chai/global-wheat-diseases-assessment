rm(list=ls())
library(fitdistrplus)


# Load data
# Rust loss
load("./Mid data/rust.US.Rdata")
# FHB loss
load("./Mid data/fhb.US.Rdata")
load("./Mid data/fhb.state.Rdata")

# Function to calcuate mean of beta
fx.mean_beta <- function(res.fitdist, distr = "beta") {
  if (distr == "beta") {
    res.mean = res.fitdist$estimate[1] / (res.fitdist$estimate[1] + res.fitdist$estimate[2])
    names(res.mean) <- NULL
  } else {
    print("Only calcuate beta fitdist class")
    res.mean = NA
  }
  
  return(res.mean)
} 

# stem rust
# High loss: pre-1960; Low loss: post-1960
# beta.pg.H <- fitdist(rust.US$StemPer[rust.US$Year <= 1960], "beta", method="mme")
# beta.pg.L <- fitdist(rust.US$StemPer[rust.US$Year > 1960], "beta", method="mme")
# beta.pg.H.US <- fitdist(rust.US$StemPer[rust.US$StemPer >= 0.02], "beta", method="mme")
# beta.pg.L.US <- fitdist(rust.US$StemPer[rust.US$StemPer < 0.02], "beta", method="mme")
beta.pg.A.US <- fitdist(rust.US$StemPer, "beta", method="mme")

# fx.mean_beta(beta.pg.H.US)
# fx.mean_beta(beta.pg.L.US)
pg.A.US.avg <- fx.mean_beta(beta.pg.A.US)

# Adjust to Savary et al's mean Global YL
beta.pg.A <- fitdist(rust.US$StemPer/pg.A.US.avg*0.0090, "beta", method="mme")

# denscomp(beta.pg.H, xlim=c(0, 0.3), ylim=c(0, 40), main="Stem Rust, High")
# denscomp(beta.pg.L, xlim=c(0, 0.3), ylim=c(0, 40), main="Stem Rust, Low")
denscomp(beta.pg.A, xlim=c(0, 0.3), ylim=c(0, 40), main="Stem Rust, All")

# stripe rust
# High loss: post-1960; low loss: pre-1960
# beta.ps.H <- fitdist(rust.US$StripePer[rust.US$Year > 1960], "beta", method="mme")
# beta.ps.L <- fitdist(rust.US$StripePer[rust.US$Year <= 1960], "beta", method="mme")
# beta.ps.H.US <- fitdist(rust.US$StripePer[rust.US$StripePer >= 0.02], "beta", method="mme")
# beta.ps.L.US <- fitdist(rust.US$StripePer[rust.US$StripePer < 0.02], "beta", method="mme")
beta.ps.A.US <- fitdist(rust.US$StripePer, "beta", method="mme")

# fx.mean_beta(beta.ps.H.US)
# fx.mean_beta(beta.ps.L.US)
ps.A.US.avg <- fx.mean_beta(beta.ps.A.US)
ps.A.US.avg

# Adjust to Savary et al's mean Global YL
beta.ps.A <- fitdist(rust.US$StripePer/ps.A.US.avg*0.0208, "beta", method="mme")

# denscomp(beta.ps.H, xlim=c(0, 0.3), ylim=c(0, 40), main="Stripe Rust, High")
# denscomp(beta.ps.L, xlim=c(0, 0.3), ylim=c(0, 40), main="Stripe Rust, Low")
denscomp(beta.ps.A, xlim=c(0, 0.3), ylim=c(0, 40), main="Stripe Rust, All")

# leaf rust
# High loss: loss > 2%; low loss: overall
# beta.pt.H <- fitdist(rust.US$LeafPer[rust.US$LeafPer >= 0.02 ], "beta", method="mme")
# beta.pt.L <- fitdist(rust.US$LeafPer[rust.US$LeafPer < 0.02 ], "beta", method="mme")
beta.pt.A.US <- fitdist(rust.US$LeafPer, "beta", method="mme")

# fx.mean_beta(beta.pt.H)
# fx.mean_beta(beta.pt.L)
pt.A.US.avg <- fx.mean_beta(beta.pt.A.US)

# Adjust to Savary et al's mean Global YL
beta.pt.A <- fitdist(rust.US$LeafPer/pt.A.US.avg*0.0325, "beta", method="mme")


# denscomp(beta.pt.H, xlim=c(0, 0.3), ylim=c(0, 40), main="Leaf Rust, High")
# denscomp(beta.pt.L, xlim=c(0, 0.3), ylim=c(0, 40), main="Leaf Rust, Low")
denscomp(beta.pt.A, xlim=c(0, 0.3), ylim=c(0, 40), main="Leaf Rust, All")

# FHB
# High loss: loss > 5%; low loss: overall
# beta.fg.H <- fitdist(fhb.US$FHBPer[fhb.US$FHBPer >= 0.05 ], "beta", method="mme")
# beta.fg.L <- fitdist(fhb.US$FHBPer[fhb.US$FHBPer < 0.05 ], "beta", method="mme")
# beta.fg.A <- fitdist(fhb.US$FHBPer, "beta", method="mme")

# beta.fg.H.US <- fitdist(fhb.state$FHBPer[fhb.state$FHBPer >= 0.02 ], "beta", method="mme")
# beta.fg.L.US <- fitdist(fhb.state$FHBPer[fhb.state$FHBPer < 0.02 ], "beta", method="mme")
beta.fg.A.US <- fitdist(fhb.state$FHBPer, "beta", method="mme")


# fx.mean_beta(beta.fg.H)
# fx.mean_beta(beta.fg.L)
fg.A.US.avg <- fx.mean_beta(beta.fg.A.US)
fg.A.US.avg

# Adjust to Savary et al's mean Global YL
beta.fg.A <- fitdist(fhb.state$FHBPer/fg.A.US.avg*0.0285, "beta", method="mme")
fx.mean_beta(beta.fg.A)

# denscomp(beta.fg.H, xlim=c(0, 0.3), ylim=c(0, 40), main="FHB, High")
# denscomp(beta.fg.L, xlim=c(0, 0.3), ylim=c(0, 40), main="FHB, Low")
denscomp(beta.fg.A, xlim=c(0, 0.3), ylim=c(0, 40), main="FHB, All")

# Spot blotch
beta.cs.A <- fitdist(fhb.state$FHBPer/fg.A.US.avg*0.0167, "beta", method="mme")
fx.mean_beta(beta.cs.A)
denscomp(beta.cs.A, xlim=c(0, 0.3), ylim=c(0, 40), main="Spot blotch, All")


# Wheat blast
beta.mg.A_SBA <- fitdist(fhb.state$FHBPer/fg.A.US.avg*0.0352, "beta", method="mme")
beta.mg.A_IGP <- fitdist(fhb.state$FHBPer/fg.A.US.avg*0.0002, "beta", method="mme")

fx.mean_beta(beta.mg.A_SBA)
fx.mean_beta(beta.mg.A_IGP)

denscomp(beta.mg.A_SBA, xlim=c(0, 0.3), ylim=c(0, 40), main="Wheat blast, All, SBA")
denscomp(beta.mg.A_IGP, ylim=c(0, 500), main="Wheat blast, All, IGP")


# Tritici blotch
beta.st.A <- fitdist(rust.US$StripePer/ps.A.US.avg*0.0244, "beta", method="mme")
fx.mean_beta(beta.st.A)
denscomp(beta.st.A, xlim=c(0, 0.3), ylim=c(0, 40), main="Tritici blotch, All")

# Save beta distribution parameters:
temp.pest = c("cs","fg", "mg_SBA", "mg_IGP", "pg", "ps", "pt", "st")
# temp.H = matrix(c(beta.fg.H$estimate, beta.pg.H$estimate, beta.ps.H$estimate, beta.pt.H$estimate), ncol=2, byrow = T)
# temp.L = matrix(c(beta.fg.L$estimate, beta.pg.L$estimate, beta.ps.L$estimate, beta.pt.L$estimate), ncol=2, byrow = T)
temp.A = matrix(c(beta.cs.A$estimate, beta.fg.A$estimate, beta.mg.A_SBA$estimate, beta.mg.A_IGP$estimate,
                  beta.pg.A$estimate, beta.ps.A$estimate, beta.pt.A$estimate, beta.st.A$estimate), 
                ncol=2, byrow = T)

# dat.beta.pest <- data.frame(pest = temp.pest, a.H = temp.H[, 1], b.H = temp.H[, 2],
#                             a.L = temp.L[, 1], b.L = temp.L[, 2],
#                             a.A = temp.A[, 1], b.A = temp.A[, 2])

dat.beta.pest <- data.frame(pest = temp.pest, 
                            a.A = temp.A[, 1], b.A = temp.A[, 2])

save(dat.beta.pest, file="./Mid Data/02_1_dat.beta.pest_v2.RData")
write.csv(dat.beta.pest, file="./Fin Data/02_1_dat.beta.pest_v2.csv")

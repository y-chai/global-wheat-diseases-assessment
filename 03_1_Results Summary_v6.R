# Results Summary
# Author: Yuan Chai 

rm(list=ls())

library(data.table)
library(ggplot2)

# Load in results
load("./Fin Data/03_0_loss.sim_v5_savary.RData")
load("./Fin Data/03_0_loss.sim_v5_US.high.RData")
load("./Fin Data/03_0_loss.sim_v5_US.low.RData")

load("./Mid Data/03_0_prod.L.RData")


# 2020-2050

# Average Annual Losses
sum.sel <- 60:90   # For 2020-2050

sum.US.high  <- data.table(sapply(loss.sim.US.high, function(x) colMeans(x[sum.sel,]))) 
sum.US.low  <- data.table(sapply(loss.sim.US.low, function(x) colMeans(x[sum.sel,]))) 

prod.sum <- prod.L[yr >= 2020, .(prod = sum(prod, na.rm = T)), by=.(yr)]
prod.yr.avg <- mean(prod.sum$prod)
prod.yr.avg


sum.US.high.long <- melt(sum.US.high, variable.name = "Disease", value.name="Loss" )
sum.US.low.long <- melt(sum.US.low, variable.name = "Disease", value.name="Loss" )


sum.US.high.long$Scenario = "High-loss regime"
sum.US.low.long$Scenario = "Low-loss regime"

sum.savary = data.table(Disease = factor(c("pt", "fg", "st", "ps", "cs", "pg", "mg"), 
                                         levels = c("pt", "fg", "st", "ps", "cs", "pg", "mg"),
                                         labels = c("Leaf Rust", "FHB", "Tritici Blotch", "Stripe Rust", "Spot Blotch", "Stem Rust", "Wheat Blast")),  
                        LossShr = c(3.25, 2.85, 2.44, 2.08, 1.67, 0.90, 0.07))

sum.A.long <- rbind(sum.US.high.long, sum.US.low.long)
sum.A.long$Disease <- factor(sum.A.long$Disease, levels = c("T", "fg", "pg", "st", "pt", "cs", "ps", "mg"),
                             labels = c("Total", "FHB", "Stem Rust", "Tritici Blotch", "Leaf Rust", "Spot Blotch", "Stripe Rust", "Wheat Blast"))

# Box plots
ggplot(data=sum.A.long[Disease != "Total"]) +
  geom_boxplot(aes(x=Disease, y=Loss/(Loss+prod.yr.avg)*100, fill=Scenario), outlier.shape = "a")+
  facet_wrap(~Scenario, nrow = 2) +
  ylab("Yearly Wheat Production Loss Share (%) \n") +
  xlab("") +
  theme_bw() +
  theme(text = element_text(size=14, color="grey20", face="bold"),
        axis.text.x = element_text(angle=90),
        axis.title = element_text(size=12),
        legend.position = "none")

# Summary statistics
fx.summary <- function(x) {
  df.quantil <- quantile(x, probs = c(0.1, 0.50, 0.8, 0.95))
  df.mean <- mean(x)
  df.sum  <- c(df.quantil, mean = df.mean)
  
  return(df.sum)
}

my.statistics <- c("90%", "50%", "20%", "5%", "Mean")

loss.report.A.prod  <- data.table(scenario = "A", my.statistics, apply(sum.US.high, 2, fx.summary))
loss.report.A.share <- data.table(scenario = "A", my.statistics, apply(sum.US.high, 2, fx.summary)/prod.yr.avg)
write.csv (loss.report.A.prod,
           file="./Fin Data/03_1_Table_Loss Summary_High_prod_v5.csv", row.names = F)
write.csv (loss.report.A.share,
           file="./Fin Data/03_1_Table_Loss Summary_High_share_v5.csv", row.names = F)

loss.report.A.prod  <- data.table(scenario = "A", my.statistics, apply(sum.US.low, 2, fx.summary))
loss.report.A.share <- data.table(scenario = "A", my.statistics, apply(sum.US.low, 2, fx.summary)/prod.yr.avg)
write.csv (loss.report.A.prod,
           file="./Fin Data/03_1_Table_Loss Summary_Low_prod_v5.csv", row.names = F)
write.csv (loss.report.A.share,
           file="./Fin Data/03_1_Table_Loss Summary_Low_share_v5.csv", row.names = F)


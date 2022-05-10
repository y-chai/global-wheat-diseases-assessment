# Yuan Chai 
# 202105
# 

rm(list=ls())

library(data.table)
library(ggplot2)

# Load in results
load("./Fin Data/03_0_loss.sim_v5_savary.RData")
load("./Fin Data/03_0_loss.sim_v5_US.high.RData")
load("./Fin Data/03_0_loss.sim_v5_US.low.RData")

load("./Mid Data/03_0_prod.L.RData")

# loss.sim.WLD <- loss.sim.savary

# loss.sim.A <- loss.sim.US

# 2020-2050

# Average Annual Losses
sum.sel <- 60:90   # For 2020-2050

sum.US.high  <- data.table(sapply(loss.sim.US.high, function(x) colMeans(x[sum.sel,]))) 
sum.US.low  <- data.table(sapply(loss.sim.US.low, function(x) colMeans(x[sum.sel,]))) 

prod.sum <- prod.L[yr >= 2020, .(prod = sum(prod, na.rm = T)), by=.(yr)]
prod.yr.avg <- mean(prod.sum$prod)
prod.yr.avg

# # Savary et al. data
# sum.WLD <- data.frame(cs = rep(prod.yr.avg * 1.67/100, 200), fg = rep(prod.yr.avg * 2.85 / 100, 200),
#                       mg = rep(prod.yr.avg * 0.07/100, 200), pg = rep(prod.yr.avg * 0.09 / 100, 200),
#                       ps = rep(prod.yr.avg * 2.08/100, 200), pt = rep(prod.yr.avg * 3.25 / 100, 200),
#                       st = rep(prod.yr.avg * 2.44/100, 200))

# # Density Plots
# ggplot() +
#   geom_density(data=sum.A, aes(T/1e+6), fill="#d7191c", color="#d7191c", alpha=0.2) +
#   theme_minimal()

ggplot() +
  geom_density(data=sum.US.high, aes(T/1e+6), fill="#d7191c", color="#d7191c", alpha=0.2) +
  theme_minimal()

# Violin plots
# sum.A.long <- melt(sum.A, variable.name = "Disease", value.name="Loss" )
# sum.A.long$Disease <- factor(sum.A.long$Disease, levels = c("T", "fg", "pt", "pg", "cs", "ps", "st", "mg"), 
#                              labels = c("Total", "FHB", "Leaf Rust", "Stem Rust", "Spot Blotch", "Stripe Rust", "Tritici Blotch", "Wheat Blast")) 
# 
# 
# ggplot(data=sum.A.long[Disease != "Total"], aes(x=Disease, y=Loss/prod.yr.avg*100)) +
#   geom_violin(aes(fill=Disease))+
#   stat_summary(fun=mean, geom="point", size=2) +
#   #  scale_x_discrete(limits = rev(c("Leaf Rust", "FHB", "Stripe Rust", "Tritici Blotch", "Spot Blotch", "Stem Rust", "Wheat Blast") )) +
#   ylab("Yearly Wheat Production Loss Share (%) \n") +
#   xlab("") +
#   #  coord_flip() +
#   theme_bw() +
#   theme(text = element_text(size=14, color="grey20", face="bold"),
#         axis.text.x = element_text(angle=90),
#         axis.title = element_text(size=12),
#         legend.position = "none")


sum.US.high.long <- melt(sum.US.high, variable.name = "Disease", value.name="Loss" )
sum.US.low.long <- melt(sum.US.low, variable.name = "Disease", value.name="Loss" )


sum.US.high.long$Scenario = "High-loss regime"
sum.US.low.long$Scenario = "Low-loss regime"

sum.savary = data.table(Disease = factor(c("pt", "fg", "st", "ps", "cs", "pg", "mg"), 
                                         levels = c("pt", "fg", "st", "ps", "cs", "pg", "mg"),
                                         labels = c("Leaf Rust", "FHB", "Tritici Blotch", "Stripe Rust", "Spot Blotch", "Stem Rust", "Wheat Blast")),  
                        LossShr = c(3.25, 2.85, 2.44, 2.08, 1.67, 0.90, 0.07))

sum.A.long <- rbind(sum.US.high.long, sum.US.low.long)
# sum.A.long$Disease <- factor(sum.A.long$Disease, levels = c("T", "pt", "fg", "st", "ps", "cs", "pg", "mg"),
#                              labels = c("Total", "Leaf Rust", "FHB", "Tritici Blotch", "Stripe Rust", "Spot Blotch", "Stem Rust", "Wheat Blast"))

sum.A.long$Disease <- factor(sum.A.long$Disease, levels = c("T", "fg", "pg", "st", "pt", "cs", "ps", "mg"),
                             labels = c("Total", "FHB", "Stem Rust", "Tritici Blotch", "Leaf Rust", "Spot Blotch", "Stripe Rust", "Wheat Blast"))


ggplot(data=sum.A.long[Disease != "Total"], aes(x=Disease, y=Loss/(Loss+prod.yr.avg)*100)) +
  geom_violin(aes(color=Scenario, fill=Scenario), position = position_dodge(0.2), width=1, alpha=0.8)+
#  geom_point(data=sum.savary, aes(x=Disease, y=LossShr, color="Savary et al. (2019)", fill="Savary et al. (2019)"), size=3) +
#  stat_summary(fun.y=mean, geom="point", size=2) +
#  scale_x_discrete(limits = rev(c("Leaf Rust", "FHB", "Stripe Rust", "Tritici Blotch", "Spot Blotch", "Stem Rust", "Wheat Blast") )) +
  scale_color_manual(values=c("#d7191c", "#2b83ba", "#404040")) +
  scale_fill_manual(values=c("#d7191c", "#2b83ba", "#404040")) +
  ylab("Proportional Wheat Production Loss (%) \n") +
  xlab("") +
#  coord_flip() +
  facet_wrap(~Scenario, nrow=2)+
  theme_bw() +
  theme(axis.text.x = element_text(size=14, face="bold", angle=90),
        axis.title = element_text(size=12, face="bold"),
        legend.position = "none")
#  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# Box plots
ggplot(data=sum.A.long[Disease != "Total"]) +
  geom_boxplot(aes(x=Disease, y=Loss/(Loss+prod.yr.avg)*100, fill=Scenario), outlier.shape = "a")+
#  stat_summary(fun.y=mean, geom="point", size=2, color="red") +
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


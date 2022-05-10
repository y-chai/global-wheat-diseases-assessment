# 20210526 YC
# Needs to update loss data and parameters

rm(list=ls())

library(data.table)
library(ggplot2)
library(gridExtra)

# Read in data
dat.CPN <- fread("./Ori Data/CPN data/US Wheat Losses CPN_2018-2020.csv")
dat.CDL <- fread("./Fin Data/rust.US.csv")
dat.STB <- fread("./Ori Data/CPN data/Wheat Losses_CPN_Septoria_2018-2020.csv")
dat.FHB.low <- fread("./Ori Data/CPN data/Wheat Losses_CPN_FHB_2018-2020.csv")
dat.FHB.high <- fread("./Ori Data/CPN data/fhb.state.csv")

dat.FHB.combo <- data.table(Year = c(dat.FHB.high$year, dat.FHB.low$Year),
                            State = c(dat.FHB.high$state_abb, dat.FHB.low$State),
                            FHBPer = c(dat.FHB.high$FHBPer, dat.FHB.low$Loss/100))

dat.beta.pest <- fread("./Ori Data/gmm/beta_gmm_v2.csv")

# Plot

dat.CDL$Period <- "Post-1960"
dat.CDL[Year <= 1960, Period := "Pre-1960"]

my.col.pal <- c("#2c7bb6", "#d7191c")
my.col.pal2 <- c("#d7191c", "#2c7bb6")

# Stem Rust
p.pg.yr <- ggplot(dat.CDL) +
  geom_col(aes(x=Year, y=StemPer, fill=(Year <= 1960))) +
  scale_fill_manual(values = c("#67a9cf", "#ef8a62")) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.8), legend.background = element_rect(fill=NA))
p.pg.yr

p.pg.den <- ggplot() +
  geom_histogram(data=dat.CDL, mapping = aes(x = StemPer, y=..density.., fill=Period),  color="white", position="dodge") +
  scale_fill_manual(values = my.col.pal, guide="none") +
  stat_function(fun = dbeta, 
                args = list(shape1=dat.beta.pest$a.US.high[dat.beta.pest$pest=="pg"], 
                            shape2=dat.beta.pest$b.US.high[dat.beta.pest$pest=="pg"]), 
                aes(color="High-loss"), size=1) +
  stat_function(fun = dbeta, 
                args = list(shape1=dat.beta.pest$a.US.low[dat.beta.pest$pest=="pg"], 
                            shape2=dat.beta.pest$b.US.low[dat.beta.pest$pest=="pg"]), 
                aes(color="Low-loss"), size=1) +
  xlab("") +
  xlim(c(0, 0.4)) +
  ylim(c(0, 60)) +
  ggtitle("Stem Rust") +
  scale_color_manual(name= "Estimated beta-distributions", values = my.col.pal2) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.5), legend.background = element_rect(fill=NA))
p.pg.den

grid.arrange(p.pg.yr, p.pg.den, ncol=2)

# Stripe Rust
p.ps.yr <- ggplot(dat.CDL) +
  geom_col(aes(x=Year, y=StripePer, fill=(Year <= 1960))) +
  scale_fill_manual(values = c("#67a9cf", "#ef8a62")) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.8), legend.background = element_rect(fill=NA))
# p.ps.yr

p.ps.den <- ggplot() +
  geom_histogram(data=dat.CDL, mapping = aes(x = StripePer, y=..density.., fill=Period),  color="white", position="dodge") +
  scale_fill_manual(values = my.col.pal2, guide="none") +
  stat_function(fun = dbeta, 
                args = list(shape1=dat.beta.pest$a.US.low[dat.beta.pest$pest=="ps"], 
                            shape2=dat.beta.pest$b.US.low[dat.beta.pest$pest=="ps"]), 
                aes(color="Low-loss"), size=1) +
  stat_function(fun = dbeta, 
                args = list(shape1=dat.beta.pest$a.US.high[dat.beta.pest$pest=="ps"], 
                            shape2=dat.beta.pest$b.US.high[dat.beta.pest$pest=="ps"]), 
                aes(color="High-loss"), size=1) +
  xlab("") +
  xlim(c(0, 0.4)) +
  ylim(c(0, 60)) +
  ggtitle("Stripe Rust") +
  scale_color_manual(name= "Estimated beta-distributions", values = my.col.pal2) +
  #  scale_fill_manual(name="", values="grey") +
  theme_bw() +
  theme(legend.position = c(0.7, 0.5), legend.background = element_rect(fill=NA))
p.ps.den

grid.arrange(p.ps.yr, p.ps.den, ncol=2)

# Leaf Rust
p.pt.yr <- ggplot(dat.CDL) +
  geom_col(aes(x=Year, y=LeafPer), fill="#008837") +
  theme_bw()
# p.pt.yr

p.pt.den <- ggplot() +
  geom_histogram(data=dat.CDL, mapping = aes(x = LeafPer, y=..density..), fill="grey", color="white", breaks = seq(0, 0.15, by=0.01)) +
#  scale_fill_manual(values = c("#67a9cf", "#ef8a62")) +
  stat_function(fun = dbeta, 
                args = list(shape1=dat.beta.pest$a.US[dat.beta.pest$pest=="pt"], 
                            shape2=dat.beta.pest$b.US[dat.beta.pest$pest=="pt"]), 
                aes(color=""), size=1) +
  xlab("") +
  xlim(c(0, 0.4)) +
  ylim(c(0, 60)) +
  ggtitle("Leaf Rust") +
  scale_color_manual(name= "Estimated beta-distributions", values = c("black")) +
  #  scale_fill_manual(name="", values="grey") +
  theme_bw() +
  theme(legend.position = c(0.7, 0.5), legend.background = element_rect(fill=NA))
p.pt.den

grid.arrange(p.pt.yr, p.pt.den, ncol=2)

# # Leaf Rust
# p.pt <- ggplot(dat.CDL) +
#   geom_histogram(mapping = aes(x = LeafPer, y=..density..), fill="grey", color="white", breaks = seq(0, 0.15, by=0.01)) +
#   stat_function(fun = dbeta, args = list(shape1=0.819, shape2=50.828), aes(color="Leaf Rust"), size=1) +
#   xlab("") +
#   ggtitle("Leaf Rust") +
#   scale_fill_manual(name="", values="grey") +
#   scale_color_manual(name= "", values = c("#abdda4")) +
#   theme_bw() +
#   theme(legend.position = c(0.7, 0.7), legend.background = element_rect(fill=NA))


# Tritici blotch
p.st.yr <- ggplot(dat.STB) +
  geom_col(aes(x=State, y=Loss/100, fill=as.factor(Year)), position = "dodge") +
  ylab("Yield Loss") +
  theme_bw() +
  theme(legend.position = c(0.5, 0.8), legend.background = element_rect(fill=NA))
p.st.yr

p.st.den <- ggplot(dat.STB) +
  geom_histogram(mapping = aes(x = Loss/100, y=..density..), fill="grey", color="white") +
  stat_function(fun = dbeta, args = list(shape1=dat.beta.pest$a.US[dat.beta.pest$pest=="st"], 
                                         shape2=dat.beta.pest$b.US[dat.beta.pest$pest=="st"]), 
                aes(color=""), 
                size=1) +
  xlab("") +
  xlim(c(0, 0.4)) +
  ylim(c(0, 60)) +
  ggtitle("Tritici Blotch") +
  scale_color_manual(name= "Estimated beta-distributions", values = c( "black"))+
#  scale_fill_manual(name="", values="grey") +
  theme_bw() +
  theme(legend.position = c(0.7, 0.7), legend.background = element_rect(fill=NA))
p.st.den

grid.arrange(p.st.yr, p.st.den, ncol=2)


# FHB
# p.fhb.high <- ggplot() +
#   geom_col(data=dat.FHB.high, aes(x=state_abb, y=FHBPer, fill=as.factor(year)), position = "dodge") +
#   ylab("Yield Loss") +
#   xlab("State") +
#   theme_bw() +
#   theme(legend.position = c(0.7, 0.8), legend.background = element_rect(fill=NA)) +
#   guides(fill = guide_legend(ncol = 3, byrow = TRUE))
# 
# p.fhb.low <- ggplot() +
#   geom_col(data=dat.FHB.low, aes(x=State, y=Loss/100, fill=as.factor(Year)), position = "dodge") +
#   ylab("Yield Loss") +
#   ylim(c(0, 0.4)) +
#   theme_bw() +
#   theme(legend.position = c(0.7, 0.8), legend.background = element_rect(fill=NA)) +
#   guides(fill = guide_legend(ncol = 3, byrow = TRUE))

p.fhb.combo <- ggplot() +
  geom_col(data=dat.FHB.combo, aes(x=State, y=FHBPer, fill=as.factor(Year)), position = "dodge") +
  scale_fill_manual(values = c(rep("#ef8a62", 9), rep("#67a9cf", 3)))+
  ylab("Yield Loss") +
  theme_bw() +
  theme(legend.position = c(0.7, 0.8), legend.background = element_rect(fill=NA)) +
  guides(fill = guide_legend(ncol = 3, byrow = TRUE))
p.fhb.combo


# FHB
p.FHB.den <- ggplot() +
  geom_histogram(data=dat.FHB.combo, mapping = aes(x = FHBPer, y=..density.., fill=(Year <= 2001)),  color="white", position = "dodge") +
  stat_function(fun = dbeta, args = list(shape1=0.615904, shape2=32.97552), aes(color="High-loss"), size=1) +
  stat_function(fun = dbeta, args = list(shape1=0.323982, shape2=6.642481), aes(color="Low-loss"), size=1) +
  xlab("Yield Loss") +
  ylim(c(0, 60)) +
  ggtitle("FHB") +
  scale_color_manual(name= "Estimated beta-distributions", values = my.col.pal2) +
  scale_fill_manual(name="Period", values=my.col.pal, labels = c("2018-2020", "1993-2001"), guide="none") +
  theme_bw() +
  theme(legend.position = c(0.7, 0.5), legend.background = element_rect(fill=NA))

p.FHB.den

# FHB, Spot Blotch, Wheat Blast
p.FHB2.den <- ggplot() +
  geom_histogram(data=dat.FHB.combo[Year >= 2018], mapping = aes(x = FHBPer, y=..density..),  color="white", position = "dodge") +
  # stat_function(fun = dbeta, args = list(shape1=0.615904, shape2=32.97552), aes(color="FHB 2018-2020"), size=1) +
  # stat_function(fun = dbeta, args = list(shape1=0.323982, shape2=6.642481), aes(color="FHB 1993-2001"), size=1) +
  stat_function(fun = dbeta, args = list(shape1=0.615904, shape2=36.264567), aes(color="Spot Blotch"), size=1) +
  stat_function(fun = dbeta, args = list(shape1=0.615904, shape2=16.881365), aes(color="Wheat Blast (East South America)"), size=1) +
  stat_function(fun = dbeta, args = list(shape1=0.615904, shape2=3078.903403), aes(color="Wheat Blast (Southwest Asia)"), size=1) +
  xlab("Yield Loss") +
  xlim(c(0, 0.4)) +
  ylim(c(0, 60)) +
  ggtitle("Spot Blotch and Wheat Blast") +
  scale_color_manual(name= "Estimated beta-distributions", values = c("#ff7f00", "#984ea3", "#4daf4a")) +
  scale_fill_manual(name="FHB", values=c("#67a9cf", "white"), labels = c("2018-2020", ""), guide="none") +
  theme_bw() +
  theme(legend.position = c(0.7, 0.6), legend.background = element_rect(fill=NA)) 

p.FHB2.den

# grid.arrange(p.fhb.combo, p.FHB.den,  ncol=2)


grid.arrange(p.pg.den, p.ps.den, p.pt.den, p.st.den, p.FHB.den, p.FHB2.den, ncol=2)

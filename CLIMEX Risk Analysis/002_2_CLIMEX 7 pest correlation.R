rm(list=ls())

library(raster)
library(data.table)
library(ggplot2)
library(heatmaply)
library(gplots)
library(RColorBrewer)

####
#### Choose EI or GI, Harvested Area (H) or Production (P) analysis ####
EG.sel <- "GI"
HP.sel <- "H"

# Read in processed CLIMEX data from above
# For GI/EI analysis, read in GI/EI files
CX_Pest <- fread(paste0("./Intermediate Data/CX_", EG.sel, ".csv"))

pest.GI.cor <- cor(CX_Pest[, .(GI_cs_NR, GI_fg_NR, GI_mg_NR, GI_pg_NR, GI_ps_NR, GI_pt_NR, GI_st_NR)])
pest.EI.cor <- cor(CX_Pest[, .(EI_cs_NR, EI_fg_NR, EI_mg_NR, EI_pg_NR, EI_ps_NR, EI_pt_NR, EI_st_NR)])

pest.col.names <- c("Spot Blotch", "FHB", "Wheat Blast", "Stem Rust", "Stripe Rust", "Leaf Rust", "Tritici Blotch")
colnames(pest.GI.cor) <- rownames(pest.GI.cor) <- pest.col.names
# Tutorial: https://bio723-class.github.io/Bio723-book/clustering-in-r.html

color.scheme <- brewer.pal(8,"YlOrRd") # generate the color scheme to use
heatmap.2(pest.GI.cor, revC = TRUE, col=color.scheme, trace="none", density.info = "none", margins = c(8, 8))

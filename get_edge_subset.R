rm(list=ls())
setwd("/nfs/nsaph_ci3/users/ci3_kcummiskey/paper3")

library(data.table)
library(geosphere)
library(grid)
library(gridExtra)
library(directlabels)
library(ggplot2)
library(viridis)


source(file = "/nfs/nsaph_ci3/users/ci3_kcummiskey/emissionsNetworks/R/import_edges.R")
source(file = "/nfs/nsaph_ci3/users/ci3_kcummiskey/emissionsNetworks/R/windrosePlots.R")


years <- 2005:2010
receptor.regions <- c("IndustrialMidwest", "Northeast", "Southeast")
PP.regions <- c( "Northeast", "IndustrialMidwest", "Southeast")
# "Northwest","UpperMidwest","Southwest",
#              "SouthernCalifornia")


#--------------------------------load edges-----------------------------------#

edges <- lapply(years, import_edges2, 
                path = "/nfs/nsaph_ci3/users/ci3_kcummiskey/emissionsNetworks/monitor_networks/allyears_byseason",
                by.season = TRUE)
edges <- do.call(rbind, edges)
setkey(edges, Monitor, PP)


#--------------------------------Obtain subset of edges for analysis-----------------------------------#
#this analysis is performed only on PP/monitors continously operating...see emissions_analysis and monitor_analysis
monitor.subset <- fread(file = "data/paper3monitors.csv")$Monitor
PP.subset <- fread(file = "data/paper3powerplants.csv")$PP

setkey(edges, Monitor)
edges <- edges[monitor.subset]
setkey(edges, PP)
edges <- edges[PP.subset]
setkey(edges, Monitor,PP)

#check if each period has same number of power plants and monitors
edges[ , list(n = .N, 
              n.PP = length(unique(PP)),
              n.monitors = length(unique(Monitor))),
      by = c("season","year")]

#add dates for plotting
dates <- data.table(season = c("winter","spring","summer","fall"), date = c("-01-01","-04-01","-07-01","-10-01"))
setkey(dates, season)
setkey(edges,season)
edges <- dates[edges]
edges[ , date := as.Date(paste(year,date, sep = ""))]

write.csv(edges, "data/edge_subset2005_2010.csv")

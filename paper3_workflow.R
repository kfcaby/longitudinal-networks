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

#---------------------------------degree analysis------------------------------#

# monitor degree
monitor.degree <- edges[ , list(degree = sum(edge, na.rm = TRUE),
                                season = unique(season),
                                year = unique(year)), 
                        by = c("date","Monitor","receptor.region")]
season.degree <- monitor.degree[ , list(median.degree = as.double(median(degree, na.rm = TRUE)),
                                        season = unique(season),
                                        year = unique(year)),
                                by = c("date","receptor.region")]

#plot monitor degree by season
season.plots <- lapply(c("winter","spring","summer","fall"), function(x, degree){
  plot <- ggplot(season.degree[season == x], aes(x = year, y = median.degree, linetype = receptor.region)) + 
    geom_line() +
    theme_classic() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(size = 10, hjust = 0.5)) +
    ylim(0,60) + 
    labs(y = "monitor degree", title = x)
  return(plot)
}, degree = season.degree)

mylegend <- g_legend(season.plots[[1]])
season.plots <- lapply(season.plots, function(x) x + theme(legend.position = "none"))
season.plots <- do.call("arrangeGrob", c(season.plots, ncol = 2))
blank <- rectGrob(gp = gpar(col = "white"))

pdf(file = "../paper3_overleaf/figures/monitor_degree.pdf", width = 6.5, height = 4)
grid.arrange(season.plots, mylegend,  heights = c(0.8,0.2), nrow = 2)
dev.off()

# powerplant degree
PP.degree <- edges[ PP.region %in% PP.regions , list(degree = sum(edge, na.rm = TRUE),
                                season = unique(season),
                                year = unique(year)), 
                        by = c("date","PP","PP.region")]
season.degree <- PP.degree[ , list(median.degree = as.double(median(degree, na.rm = TRUE)),
                                        season = unique(season),
                                        year = unique(year)),
                                by = c("date","PP.region")]

#plot powerplant degree by season
season.plots <- lapply(c("winter","spring","summer","fall"), function(x, degree){
  plot <- ggplot(season.degree[season == x], aes(x = year, y = median.degree, linetype = PP.region)) + 
    geom_line() +
    theme_classic() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(size = 10, hjust = 0.5)) +
    ylim(0,60) + 
    labs(y = "pwr plt degree", title = x)
  return(plot)
}, degree = season.degree)

mylegend <- g_legend(season.plots[[1]])
season.plots <- lapply(season.plots, function(x) x + theme(legend.position = "none"))
season.plots <- do.call("arrangeGrob", c(season.plots, ncol = 2))
blank <- rectGrob(gp = gpar(col = "white"))

pdf(file = "../paper3_overleaf/figures/powerplant_degree.pdf", width = 6.5, height = 4)
grid.arrange(season.plots, mylegend,  heights = c(0.8,0.2), nrow = 2)
dev.off()


#-----------windrose plots by region, season, and year-----------------------------------------------#
year.season <- expand.grid( c("winter","spring","summer","fall"),years)
year.season <- data.frame(year.season)
colnames(year.season) <- c("season","year")

lapply(PP.regions, function(y){
  y = "IndustrialMidwest"
  windrose.plots <- lapply(1:nrow(year.season), function(x, year.season, region){
    p <- plotPairCounts(edges[year == year.season[x,2] & season == year.season[x,1] & edge == 1, ], 
                        region = region,
                        title = paste(year.season[x,1],year.season[x,2]),
                        center = "powerplants")
    return(p)
  }, year.season = year.season, region = y)
  
  mylegend <- g_legend(windrose.plots[[1]] + theme(legend.position = "bottom")) 
  windrose.plots <- lapply(windrose.plots, function(x) x + expand_limits(y = c(0,1000)) + theme(plot.title = element_blank()))
  windrose.plots <- do.call("arrangeGrob", c(windrose.plots, ncol = 4) )
  season.labels <- arrangeGrob(
                    textGrob(label = "winter", gp = gpar(fontsize = 12)),
                    textGrob(label = "spring", gp = gpar(fontsize = 12)),
                    textGrob(label = "summer", gp = gpar(fontsize = 12)),
                    textGrob(label = "fall", gp = gpar(fontsize = 12)),
                    ncol = 4)
  year.labels <- do.call("arrangeGrob", c(lapply(years, function(x) textGrob(label = x, gp = gpar(fontsize = 12))), ncol = 1))
  blank <- rectGrob(gp = gpar(col = "white"))
  
  main <- arrangeGrob(blank, season.labels, year.labels, windrose.plots,
               heights = c(0.05,0.95),
               widths = c(0.10,0.90),
               layout_matrix = rbind(c(1,2),c(3,4)),
               ncol = 2)
  mylegend <- arrangeGrob(mylegend, ncol = 5)
  pdf(file = paste("../paper3_overleaf/figures/windrose_",y,".pdf",sep = ""), height = 8, width = 6.5)
    grid.arrange(mylegend)
  dev.off()
})






#plot all pairs ~ note this stays the same year to year for a fixed set of monitors and powerplants
allpairs <- lapply(c("IndustrialMidwest", "Northeast", "Southeast"),
                   function(x, monitors, powerplants, path){
                     edges <- import_edges2(2003,path)
                     edges <- edges[Monitor %in% monitors & PP %in% powerplants,]
                     return(plotPairCounts(edges, regions = x, center = "powerplants", title = x))
                   }, monitors = monitor.subset, powerplants = PP.subset, path = path)
#pdf(file = "resultsPaper3/allpairs.pdf", height = 3, width = 6.5)
do.call("grid.arrange", c(allpairs, ncol = 3))
#dev.off()

#all edges
plots <- lapply(years, function(x, monitors = NULL, powerplants = NULL, path) {
  edges <- import_edges2(x, path)
  if(!is.null(monitors)){
    edges <- edges[Monitor %in% monitors,]
  }
  if(!is.null(powerplants)){
    edges <- edges[PP %in% powerplants]
  }
  plotPairCounts(edges[edge == 1,], regions = "Northeast", center = "powerplants", title = x) 
}, monitors =monitor.subset, powerplant = PP.subset, path = path)
do.call("grid.arrange", c(plots, ncol = 4))

#edge probs
plots <- lapply(years, function(x, monitors = NULL, powerplants = NULL, path, region) {
  edges <- import_edges2(x, path)
  if(!is.null(monitors)){
    edges <- edges[Monitor %in% monitors,]
  }
  if(!is.null(powerplants)){
    edges <- edges[PP %in% powerplants]
  }
  year <- textGrob(label = x, gp = gpar(fontsize = 12))
  p1 <- plotEdgeProbs(edges[distance < 250,], regions = region, center = "powerplants", title = "") 
  p2 <- plotEdgeProbs(edges[distance > 250 & distance < 500,], regions = region, center = "powerplants", title = "") 
  p3 <- plotEdgeProbs(edges[distance > 500 & distance < 750,], regions = region, center = "powerplants", title = "") 
  p4 <- plotEdgeProbs(edges[distance > 750 & distance < 1000,], regions = region, center = "powerplants", title = "") 
  return(list(year,p1,p2,p3,p4))
}, monitors = monitor.subset, powerplant = PP.subset, path = path, region = "Northeast")

distances <- list(textGrob(label = "", gp = gpar(fontsize = 12)),
                  textGrob(label = "0-250km", gp = gpar(fontsize = 12)),
                  textGrob(label = "250-500km", gp = gpar(fontsize = 12)),
                  textGrob(label = "500-750km", gp = gpar(fontsize = 12)),
                  textGrob(label = "750-1000km", gp = gpar(fontsize = 12)))
plots2 <- c(distances, unlist(plots, recursive = FALSE))
pdf(file = "resultsPaper3/NE.pdf", height = 9, width = 6.5)
do.call("grid.arrange", c(plots2,  ncol = 5))
dev.off()


edges <- lapply(2003:2004, function(x, path) {
  import_edges2(x, path)[ , .(Monitor, PP, edge, distance, bearing, PP.region)]
}, path = path)
edges <- do.call("rbind",edges)
edges <- edges[Monitor %in% monitor.subset & PP %in% PP.subset, ]

p = list()
region = "IndustrialMidwest"
p[[1]] <- textGrob(label = "Industrial\nMidwest", gp = gpar(fontsize = 12))
p[[2]] <- plotEdgeProbs(edges[distance < 250,], regions = region, center = "powerplants", title = "") 
p[[3]] <- plotEdgeProbs(edges[distance > 250 & distance < 500,], regions = region, center = "powerplants", title = "") 
p[[4]] <- plotEdgeProbs(edges[distance > 500 & distance < 750,], regions = region, center = "powerplants", title = "") 
p[[5]] <- plotEdgeProbs(edges[distance > 750 & distance < 1000,], regions = region, center = "powerplants", title = "")
region = "Northeast"
p[[6]] <- textGrob(label = "Northeast", gp = gpar(fontsize = 12))
p[[7]] <- plotEdgeProbs(edges[distance < 250,], regions = region, center = "powerplants", title = "") 
p[[8]] <- plotEdgeProbs(edges[distance > 250 & distance < 500,], regions = region, center = "powerplants", title = "") 
p[[9]] <- plotEdgeProbs(edges[distance > 500 & distance < 750,], regions = region, center = "powerplants", title = "") 
p[[10]] <- plotEdgeProbs(edges[distance > 750 & distance < 1000,], regions = region, center = "powerplants", title = "")
region = "Southeast"
p[[11]] <- textGrob(label = "Southeast", gp = gpar(fontsize = 12))
p[[12]] <- plotEdgeProbs(edges[distance < 250,], regions = region, center = "powerplants", title = "") 
p[[13]] <- plotEdgeProbs(edges[distance > 250 & distance < 500,], regions = region, center = "powerplants", title = "") 
p[[14]] <- plotEdgeProbs(edges[distance > 500 & distance < 750,], regions = region, center = "powerplants", title = "") 
p[[15]] <- plotEdgeProbs(edges[distance > 750 & distance < 1000,], regions = region, center = "powerplants", title = "")



pdf(file = "resultsPaper3/allyears.pdf", height = 4, width = 6.5)
do.call("grid.arrange", c(p,  ncol = 5))
dev.off()








# Other visualizations of extreme PM days
#number of monitor each year with at least one extreme day of PM
extreme.year.region <- PM_annual[ monitor.subset , 
                                  list(n.monitor.extreme = sum(ifelse(n.days.extreme > 0,1,0),na.rm = TRUE)),
                                  by = c("receptor.region","year")]
setkey(extreme.year.region, year,receptor.region)


region.summary <- degree.by.region[extreme.year.region]
setkey(region.summary,year,receptor.region)
region.summary_long <- melt(region.summary, id.vars = c("receptor.region","year"))

p1 <- ggplot(region.summary_long[variable == "avg.degree",], 
             aes(x = year, y = value, linetype = receptor.region, color = receptor.region)) + 
  geom_line() + labs(y = "Average monitor degree")
p2 <- ggplot(region.summary_long[variable == "n.monitor.extreme",], 
             aes(x = year, y = value, linetype = receptor.region)) + 
  geom_line() + labs(y = "Number of extreme monitors")
#pdf(file = "resultsPaper3/highPMdays.pdf", height = 10, width = 11)
grid.arrange(p1,p2, ncol = 1)
#dev.off()





#-----------Look at concordance of edges between power plants that--------------#

#NOTE: In this section, analysis is limited to the subset of monitors and power plants identified above

#overall
concordance <- getEdgeConcordance(monitor.subset, PP.subset, years, path)

#by region
concordance.region <- lapply(c("IndustrialMidwest","Northeast", "Southeast"),
                             function(x, monitor.subset, PP.subset, years, path) {
                               region.monitors <-unique(PM[Monitor %in% monitor.subset & receptor.region == x,]$Monitor)
                               results <- getEdgeConcordance(region.monitors, PP.subset, years, path)
                               results <- cbind(rep(x,nrow(results)), results)
                               names(results)[1] <- "receptor.region"
                               return(results)
                             }, monitor.subset = monitor.subset, PP.subset = PP.subset,
                             years = years, path = path)
concordance.region <- do.call("rbind",concordance.region)

#pdf(file = "resultsPaper3/degree_corr.pdf", height = 3, width = 6.5)
ggplot(concordance.region, aes(x = year2, y = degree.rank.corr, linetype = receptor.region)) +
  geom_line() + labs(y = "monitor degree correlation", x = "year") + 
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
#dev.off()

conc_long <- data.table(concordance.region)
conc_long <- melt(conc_long[ ,.(receptor.region,year2,concordance,pos.perc,neg.perc)], 
                  id.vars = c("receptor.region","year2"))
#pdf(file = "resultsPaper3/concordance.pdf", height = 3, width = 6.5)
ggplot(conc_long, aes(x = year2, y = value, linetype = receptor.region, color = variable)) +
  geom_line() + labs(y = "", x = "year") + ylim(0,1) +
  scale_color_viridis(discrete = TRUE, option = "magma", end = 0.75,
                      guide = guide_legend(title = "concordance", title.position = "top"),
                      labels = c("overall", "edges", "nonedges"))+
  scale_linetype(guide = guide_legend(title = element_blank())) +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal") 
#dev.off()
#-----------Look at annual edge percent---------------------------------------#

edge.perc <- t(getEdgePercents(monitor.subset, PP.subset, years, path))
edge.perc <- cbind(years, edge.perc)



#FIX list structure

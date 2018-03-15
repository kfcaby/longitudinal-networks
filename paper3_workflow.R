rm(list=ls())

library(data.table)
library(geosphere)
library(grid)
library(gridExtra)
library(directlabels)
library(ggplot2)
library(viridis)

source(file = "paper3_functions.R")
source(file = "../emissionsNetworks/R/windrosePlots.R")
regions <- c("IndustrialMidwest", "Northeast", "Southeast")
years <- 2005:2010

#---------------------------------load edges------------------------------#

edges <- fread(file = "data/edge_subset2005_2010.csv")[ , V1:= NULL]
setkey(edges, Monitor, PP)

#---------------------------------degree analysis------------------------------#



#MONITORS DEGREE

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
    labs(y = "degree", title = x)
  return(plot)
}, degree = season.degree)

mylegend <- g_legend(season.plots[[1]])
season.plots <- lapply(season.plots, function(x) x + theme(legend.position = "none"))
season.plots <- do.call("arrangeGrob", c(season.plots, ncol = 2))
blank <- rectGrob(gp = gpar(col = "white"))

pdf(file = "../paper3_overleaf/figures/monitor_degree.pdf", width = 6.5, height = 3.5)
grid.arrange(season.plots, mylegend,  heights = c(0.9,0.1), nrow = 2)
dev.off()

# monitor degree year-to-year correlation by season
years.corr <- cbind(2005:2009,2006:2010)
params <- cbind(rep(c("winter", "spring", "summer", "fall"),3), unlist(lapply(regions, rep, 4)))
setkey(monitor.degree, Monitor)
monitor.degree.cor <- 
  lapply(1:12, function(y, parameters)
    lapply(1:5, function(x, degree, years, season.p, region){
      correlation = cor(degree[year == years[x,1] & receptor.region == region & season == season.p,]$degree,
                        degree[year == years[x,2] & receptor.region == region & season == season.p,]$degree,
                        method = "spearman", use = "pairwise.complete.obs")
      correlation = round(correlation,3)
      return(data.table(year = years[x,2], season = season.p, receptor.region = region, correlation = correlation))
    }, degree = monitor.degree, years = years.corr, season.p = params[y,1], region = params[y,2]), parameters = params)
monitor.degree.cor <- unlist(monitor.degree.cor, recursive = FALSE)
monitor.degree.cor <- do.call("rbind", monitor.degree.cor)

#plot monitor degree correlation by season
season.plots <- lapply(c("winter","spring","summer","fall"), function(x, monitors){
  plot <- ggplot(monitors[season == x,], aes(x = year, y = correlation, linetype = receptor.region)) + 
    geom_line() +
    theme_classic() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(size = 10, hjust = 0.5)) +
    ylim(-1,1) + 
    labs(y = "correlation", title = x)
  return(plot)
}, monitors = monitor.degree.cor)

mylegend <- g_legend(season.plots[[1]])
season.plots <- lapply(season.plots, function(x) x + theme(legend.position = "none"))
season.plots <- do.call("arrangeGrob", c(season.plots, ncol = 2))
blank <- rectGrob(gp = gpar(col = "white"))

pdf(file = "../paper3_overleaf/figures/monitor_degree_correlation.pdf", width = 6.5, height = 3.5)
grid.arrange(season.plots, mylegend,  heights = c(0.9,0.1), nrow = 2)
dev.off()

#monitor degree season to season correlation
setkey(monitor.degree, date)
dates <- unique(monitor.degree$date)
dates <- cbind(dates[-length(dates)], dates[-1])

monitor.degree.cor2 <- lapply(regions, function(y)
  lapply(1:nrow(dates), function(x, dates, degree, region.p){
    correlation <- cor(degree[date == dates[x,1] & receptor.region == region.p, ]$degree,
                       degree[date == dates[x,2] & receptor.region == region.p, ]$degree,
                       method = "spearman",
                       use = "pairwise.complete.obs")
    return(data.table(date = dates[x,2], region = region.p, correlation = correlation))
  }, dates = dates, degree = monitor.degree, region.p = y)
)
monitor.degree.cor2 <- unlist(monitor.degree.cor2, recursive = FALSE)
monitor.degree.cor2 <- do.call("rbind", monitor.degree.cor2)

cor.plot <- ggplot(monitor.degree.cor2, aes(x = as.Date(date), y = correlation, linetype = region)) + 
  geom_line() +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10),
    plot.title = element_blank()) +
  ylim(-1,1) + 
  labs(y = "correlation")

pdf(file = "../paper3_overleaf/figures/monitor_degree_correlation2.pdf", width = 6.5, height = 2)
cor.plot
dev.off()



# POWERPLANT DEGREE

PP.degree <- edges[ PP.region %in% regions , list(degree = sum(edge, na.rm = TRUE),
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
    labs(y = "degree", title = x)
  return(plot)
}, degree = season.degree)

mylegend <- g_legend(season.plots[[1]])
season.plots <- lapply(season.plots, function(x) x + theme(legend.position = "none"))
season.plots <- do.call("arrangeGrob", c(season.plots, ncol = 2))
blank <- rectGrob(gp = gpar(col = "white"))

pdf(file = "../paper3_overleaf/figures/powerplant_degree.pdf", width = 6.5, height = 3.5)
grid.arrange(season.plots, mylegend,  heights = c(0.9,0.1), nrow = 2)
dev.off()


#-----------windrose plots by region, season, and year-----------------------------------------------#

#edge counts
year.season <- expand.grid( c("winter","spring","summer","fall"),years)
year.season <- data.frame(year.season)
colnames(year.season) <- c("season","year")

lapply(regions, function(y){
  windrose.plots <- lapply(1:nrow(year.season), function(x, year.season, region){
    p <- plotPairCounts(edges[year == year.season[x,2] & season == year.season[x,1] & edge == 1, ], 
                        region = region,
                        title = paste(year.season[x,1],year.season[x,2]),
                        center = "powerplants")
    return(p)
  }, year.season = year.season, region = y)
  
  mylegend <- g_legend(windrose.plots[[1]] + theme(legend.position = "bottom")) 
  lim2 <- ifelse(y == "IndustrialMidwest",1000, ifelse(y == "Northeast", 750, 750))
  windrose.plots <- lapply(windrose.plots, function(x) x + expand_limits(y = c(0,lim2)) + theme(plot.title = element_blank()))
  windrose.plots <- do.call("arrangeGrob", c(windrose.plots, ncol = 4) )
  season.labels <- arrangeGrob(
                    textGrob(label = "winter", gp = gpar(fontsize = 10)),
                    textGrob(label = "spring", gp = gpar(fontsize = 10)),
                    textGrob(label = "summer", gp = gpar(fontsize = 10)),
                    textGrob(label = "fall", gp = gpar(fontsize = 10)),
                    ncol = 4)
  year.labels <- do.call("arrangeGrob", c(lapply(years, function(x) textGrob(label = x, gp = gpar(fontsize = 10))), ncol = 1))
  blank <- rectGrob(gp = gpar(col = "white"))
  
  main <- arrangeGrob(blank, season.labels, year.labels, windrose.plots,
               heights = c(0.05,0.95),
               widths = c(0.10,0.90),
               layout_matrix = rbind(c(1,2),c(3,4)),
               ncol = 2)
  allpairs <- plotPairCounts(edges[year == 2005 & season == "winter",],
                             region = y,
                             center = "powerplants")
  allpairs <- arrangeGrob(textGrob(label = "All pairs", gp = gpar(fontsize = 10)), allpairs, ncol = 1, heights = c(0.05,0.95))
  
  pdf(file = paste("../paper3_overleaf/figures/windrose_",y,".pdf",sep = ""), height = 8, width = 6.5)
    grid.arrange(allpairs,main, mylegend, ncol = 1, heights = c(0.2,0.75, 0.05))
  dev.off()
})

#edge probs
lapply(regions, function(y){
  windrose.plots <- lapply(1:nrow(year.season), function(x, year.season, region){
    p <- plotEdgeProbs_noDistance(edges[year == year.season[x,2] & season == year.season[x,1] & distance < 1000, ], 
                        region = region,
                        title = paste(year.season[x,1],year.season[x,2]),
                        center = "powerplants")
    return(p)
  }, year.season = year.season, region = y)
  
  #lim2 <- ifelse(y == "IndustrialMidwest",1000, ifelse(y == "Northeast", 750, 750))
  windrose.plots <- lapply(windrose.plots, function(x) x + expand_limits(y = c(0,0.5)) + theme(plot.title = element_blank()))
  windrose.plots <- do.call("arrangeGrob", c(windrose.plots, ncol = 4) )
  season.labels <- arrangeGrob(
    textGrob(label = "winter", gp = gpar(fontsize = 10)),
    textGrob(label = "spring", gp = gpar(fontsize = 10)),
    textGrob(label = "summer", gp = gpar(fontsize = 10)),
    textGrob(label = "fall", gp = gpar(fontsize = 10)),
    ncol = 4)
  year.labels <- do.call("arrangeGrob", c(lapply(years, function(x) textGrob(label = x, gp = gpar(fontsize = 10))), ncol = 1))
  blank <- rectGrob(gp = gpar(col = "white"))
  
  pdf(file = paste("../paper3_overleaf/figures/windrose_prob_",y,".pdf",sep = ""), height = 8, width = 6.5)
  grid.arrange(blank, season.labels, year.labels, windrose.plots,
              heights = c(0.05,0.95),
              widths = c(0.10,0.90),
              layout_matrix = rbind(c(1,2),c(3,4)),
              ncol = 2)
  dev.off()
})



#EXTREME DAYS


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
setkey(edges, Monitor, PP)
years.corr <- cbind(2005:2009,2006:2010)
params <- cbind(rep(c("winter", "spring", "summer", "fall"),3), unlist(lapply(regions, rep, 4)))

concordance <- lapply(1:nrow(params), function(y, parameters) 
  lapply(1:5, function(x, edges.p, years.p, season.p, region.p) {
    edge1 <- edges.p[receptor.region == region.p & season == season.p & year == years.p[x,1] & distance < 1000,]$edge
    edge2 <- edges.p[receptor.region == region.p & season == season.p & year == years.p[x,2] & distance < 1000,]$edge
    n.agree <- sum(edge1 == edge2, na.rm = TRUE)
    n.disagree <- sum(edge1 != edge2, na.rm = TRUE)
    n.total <- n.agree + n.disagree
    pos.agree <- sum(edge1 == 1 & edge2 == 1, na.rm = TRUE)
    neg.agree <- sum(edge1 == 0 & edge2 == 0, na.rm = TRUE)
    
    data.table(receptor.region = region.p,
                      season = season.p,
                      year = years.p[x,2],
                concordance = n.agree/n.total, 
                pos.concordance = pos.agree/sum(edge1 == 1 & !is.na(edge2)),
                neg.concordance = neg.agree/sum(edge1 == 0 & !is.na(edge2)),
                edge_percent = sum(edge2, na.rm = TRUE)/sum(!is.na(edge2)))
  }, edges.p = edges, years.p = years.corr, season.p = parameters[y,1], region.p = parameters[y,2]), parameters = params)

concordance <- do.call("rbind", unlist(concordance, recursive = FALSE))


#plot concordance by season
season.plots <- lapply(c("winter","spring","summer","fall"), function(x, concordance.p){
  conc_long <- melt(concordance.p[season == x], id.vars = c("receptor.region", "season", "year"))
  conc_long <- conc_long[variable != "edge_percent",]
  plot <- ggplot(conc_long, aes(x = year, y = value, linetype = receptor.region, color = variable)) + 
    geom_line() +
    theme_classic() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 11),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(size = 12, hjust = 0.5)) +
    ylim(0,1) + 
    scale_color_viridis(discrete = TRUE, option = "magma", end = 0.75,
                        guide = guide_legend(title = element_blank()),
                        labels = c("overall", "edges", "nonedges"))+
    scale_linetype(guide = guide_legend(title = element_blank())) +
    labs(y = "percent", title = x)
  return(plot)
}, concordance.p = concordance)

mylegend <- g_legend(season.plots[[1]])
season.plots <- lapply(season.plots, function(x) x + theme(legend.position = "none"))
season.plots <- do.call("arrangeGrob", c(season.plots, ncol = 2))
blank <- rectGrob(gp = gpar(col = "white"))

pdf(file = "../paper3_overleaf/figures/concordance.pdf", width = 6.5, height = 4)
grid.arrange(season.plots, mylegend,  heights = c(0.9,0.1), nrow = 2)
dev.off()


#difference between positive concordance and edge percent
concordance[ , pos.diff.perc := (pos.concordance - edge_percent)/edge_percent]
season.plots <- lapply(c("winter","spring","summer","fall"), function(x, concordance.p){
  plot <- ggplot(concordance.p[season == x], aes(x = year, y = pos.diff.perc, linetype = receptor.region)) +
    geom_line() +
    theme_classic() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 11),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(size = 12, hjust = 0.5)) +
    ylim(-1,1) +
    labs(y = "percent", title = x)
  return(plot)
}, concordance.p = concordance)

mylegend <- g_legend(season.plots[[1]])
season.plots <- lapply(season.plots, function(x) x + theme(legend.position = "none"))
season.plots <- do.call("arrangeGrob", c(season.plots, ncol = 2))
blank <- rectGrob(gp = gpar(col = "white"))

pdf(file = "../paper3_overleaf/figures/positive_diff.pdf", width = 6.5, height = 4)
grid.arrange(season.plots, mylegend,  heights = c(0.9,0.1), nrow = 2)
dev.off()


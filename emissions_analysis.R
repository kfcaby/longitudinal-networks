#Data analysis for paper 3
# determines subset of powerplants to perfrom analysis on.
rm(list=ls())
setwd("/nfs/nsaph_ci3/users/ci3_kcummiskey/paper3")
source("paper3_functions.R")

library(data.table)
library(xtable)
library(ggplot2)
library(gridExtra)
library(grid)

regions <- c("IndustrialMidwest", "Northeast", "Southeast")

years <- 2005:2010

#-----------------------------read in emissions data-----------------------#
emissions <- fread("/nfs/nsaph_ci3/users/ci3_kcummiskey/emissionsNetworks/data/daily.emissions.csv")
emissions[is.na(emissions)] <- 0
emissions <- melt(emissions, variable.name = "date", value.name = "SO2")
names(emissions)[1] <- "PP"
emissions[ , date := as.Date(date)]
setkey(emissions,PP)
emissions[ , year := year(date)]
emissions[ , month := month(date)]

#add other info
PP.locations <- fread("/nfs/nsaph_ci3/users/ci3_kcummiskey/emissionsNetworks/data/powerplant.locations.csv")[ , V1 := NULL]
setkey(PP.locations,ID)
emissions <- emissions[PP.locations]

#add season
seasons <- data.table(cbind(1:12),c(rep("winter",2),rep("spring",3), rep("summer",3), rep("fall", 3), "winter"))
names(seasons) <- c("month","season")
setkey(seasons, month)
setkey(emissions, month)
emissions <- emissions[seasons]

#December should be with the previous year
emissions[ , year.new := ifelse(month == 12, year + 1, year)]


#find power plants that were operating over entire period
operating_annual <- emissions[ , list(days.operating = sum(SO2 > 0,na.rm = TRUE),
                               PP.region = unique(PP.region)), by = c("season","year.new","PP")]                       
operating <- operating_annual[year.new %in% years  , list(period.operating = sum(days.operating > 0, na.rm = TRUE),
                                      PP.region = unique(PP.region)), by = c("PP") ]
#Find operating all seasons
PP.subset <- operating[period.operating == 4*(years[length(years)] - years[1] + 1), .(PP)]

setkey(emissions,PP)
emissions.subset <- emissions[PP.subset]
emissions.subset <- emissions.subset[PP.region %in% regions,]
emissions.subset <- emissions.subset[year %in% years]

#write.csv(PP.subset, file = "data/paper3powerplants.csv")


#-----------------------------plots for paper----------------------------------------------------#



PP.summary <- emissions.subset[ , list(days.operating = sum(SO2 > 0, na.rm = TRUE),
                                       totalSO2 = sum(SO2, na.rm = TRUE),
                                       PP.region = unique(PP.region)),
                                       by = c("year","PP")]
#avgSO2 on days operating
PP.summary[ , avgSO2.per.operating.day := round(totalSO2/days.operating,1)]

hist(PP.summary$days.operating)
hist(PP.summary$avgSO2.per.operating.day)


annual.summary <- PP.summary[ , list(median.operating.days = as.double(median(days.operating)),
                                     median.SO2.per.operating.day = median(avgSO2.per.operating.day),
                                     PP.region = unique(PP.region)),
                             by = c("year","PP.region")]

p1 <- ggplot(annual.summary, aes(x = year, y = median.operating.days, linetype = PP.region)) +
  geom_line() + 
  theme_classic() +
  labs(y = "days per power plant") +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  expand_limits(y = 0)

p2 <-  ggplot(annual.summary, aes(x = year, y = median.SO2.per.operating.day, linetype = PP.region)) +
  geom_line() + 
  theme_classic() +
  labs(y = "SO2 (tons)/day") +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  guides(linetype = guide_legend(title = "region")) +
  expand_limits(y = 0)

mylegend <- g_legend(p2)
blank <- rectGrob(gp = gpar(col = "white"))

pdf(file = "../paper3_overleaf/figures/powerplant_summary.pdf", width = 6.5, height = 5)
grid.arrange(p1,
             p2,
             blank,
             layout_matrix = rbind(c(3,1),c(3,2)),
             heights = c(0.45,0.55),
             widths = c(0.035,0.965))
dev.off()


region_summary <- PP.summary[ year ==2005 , list(n = .N), by = "PP.region"]

sink(file = "../paper3_overleaf/tables/powerplants.tex")
print(xtable(region_summary, caption = "Number of coal power plants by region"), 
      include.rownames = FALSE, 
      include.colnames = FALSE,
      hline.after = NULL)
sink()

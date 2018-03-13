#Data analysis for paper 3
# determines subset of powerplants to perfrom analysis on.
rm(list=ls())
setwd("/nfs/nsaph_ci3/users/ci3_kcummiskey/longitudinalNetworks")
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

write.csv(emissions.subset, file = "data/emissions_subset_2005-2010.csv")

#Data analysis for paper 3
# determines subset of monitors to perfrom analysis on.
rm(list=ls())
setwd("/nfs/nsaph_ci3/users/ci3_kcummiskey/longitudinalNetworks")

library(data.table)

regions <- c("IndustrialMidwest", "Northeast", "Southeast")

years <- 2005:2010

#-----------------------------PM descriptive statistics----------------------------------------------------#

PM <- fread(file = "/nfs/nsaph_ci3/users/ci3_kcummiskey/emissionsNetworks/data/daily.PM.csv")
PM <- melt(PM, variable.name = "date", value.name = "PM25")
names(PM)[1] <- "Monitor"
setkey(PM,Monitor)
PM$date = as.Date(PM$date)
PM[ ,year := year(date)]
PM[ , month := month(date)]

#attach other information
monitor.locations <- fread(file = "/nfs/nsaph_ci3/users/ci3_kcummiskey/emissionsNetworks/data/monitor.locations.csv")[ , V1 := NULL]
setkey(monitor.locations, ID)

PM <- PM[monitor.locations]

#only include regions specified above
PM <- PM[receptor.region %in% regions,]

#only include years specified above
PM <- PM[year %in% years,]

#-----------------------------Monitor subset----------------------------------------------------#


#include only monitors that measured at least 100 days in each year
monitor.summary <- PM[ , list(days.NA = sum(is.na(PM25)),
                              days.measured = sum(!is.na(PM25)),
                              avg.PM = mean(PM25, na.rm = TRUE),
                              sd.PM = sd(PM25, na.rm = TRUE),
                              receptor.region = unique(receptor.region)),
                      by = c("Monitor","year")]

#Histogram of the number of days of measured PM in 2010
pdf(file = "../paper3_overleaf/figures/PMmeasured_days_hist.pdf", height = 3, width = 5)
ggplot(monitor.summary[ year == 2010,], aes(x = days.measured)) + 
  geom_histogram() +
  theme_classic() +
  labs(x = "days", y = "number of monitors")
dev.off()

# determine subset of monitors for analysis
operating <- monitor.summary[ , list(years.operating = sum(days.measured >= 100)),
                             by = "Monitor"]
monitor.subset <- operating[ years.operating == years[length(years)]- years[1] + 1, .(Monitor)]


PM.subset <- PM[monitor.subset]
setkey(PM.subset, Monitor)

write.csv(PM.subset, file = "data/PM_subset_2005_2010.csv")

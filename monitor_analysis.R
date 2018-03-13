#Data analysis for paper 3
# determines subset of monitors to perfrom analysis on.
rm(list=ls())
setwd("/nfs/nsaph_ci3/users/ci3_kcummiskey/paper3")

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


#write.csv(monitor.subset, file = "data/paper3monitors.csv")

#----------------------------Daily PM Plot----------------------------------------------------#

#daily PM at these monitors
PM_daily_region <- PM.subset[ , list(dailyPM = mean(PM25, na.rm = TRUE),
                                     year = unique(year)),
                      by = c("date", "receptor.region")]


pdf(file = "../paper3_overleaf/figures/dailyPM.pdf", height = 3, width = 6.5)
ggplot(PM_daily_region, aes(x = date, y = dailyPM, color = receptor.region)) +
  geom_line() + scale_color_viridis(discrete = TRUE) +
  theme_classic() +
  theme(legend.position = "bottom",  legend.title = element_blank())
dev.off()


PM_annual_region <-  PM.subset[, list(annualPM = mean(PM25, na.rm = TRUE)),
                        by = c("year", "receptor.region")]
setkey(PM_annual_region, receptor.region, year)


pdf(file = "../paper3_overleaf/figures/annualPM.pdf", height = 3, width = 6.5)
ggplot(PM_annual_region, aes(x = year, y = annualPM, linetype = receptor.region)) +
  geom_line() + 
  theme_classic() +
  ylab("PM2.5") +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.title = element_blank()) +
  expand_limits(y = 0)
dev.off()


region_summary <- monitor.summary[ year == 2010 & Monitor %in% monitor.subset$Monitor, list(n = .N), by = "receptor.region"]

sink(file = "../paper3_overleaf/tables/monitors.tex")
print(xtable(region_summary, caption = "Number of monitors by region"), 
      include.rownames = FALSE, 
      include.colnames = FALSE,
      hline.after = NULL)
sink()

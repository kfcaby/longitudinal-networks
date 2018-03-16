#Data analysis for paper 3
# determines subset of monitors to perfrom analysis on.
rm(list=ls())


library(data.table)
library(grid)
library(gridExtra)
library(ggplot2)
library(viridis)
library(xtable)

regions <- c("IndustrialMidwest", "Northeast", "Southeast")

years <- 2005:2010

#----------------------------get monitor data for analysis----------------------------------------------------#

PM.subset <- fread(file = "data/PM_subset_2005_2010.csv")[ , V1 := NULL]
PM.subset[ , date := as.Date(date)]


#----------------------------Daily PM Plot----------------------------------------------------#

#daily PM at these monitors
PM_daily_region <- PM.subset[ , list(dailyPM = mean(PM25, na.rm = TRUE),
                                     year = unique(year)),
                      by = c("date", "receptor.region")]


pdf(file = "../paper3_overleaf/figures/dailyPM.pdf", height = 3, width = 6.5)
ggplot(PM_daily_region, aes(x = date, y = dailyPM, color = receptor.region)) +
  geom_line() + scale_color_viridis(discrete = TRUE) +
  theme_classic() +
  theme(legend.position = "none",  
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 11)) +
  ylim(0,55) + labs(y = "PM2.5")
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
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 11)) +
  expand_limits(y = 0)
dev.off()



region_summary <- PM.subset[ , length(unique(Monitor)), by = "receptor.region"]

sink(file = "../paper3_overleaf/tables/monitors.tex")
print(xtable(region_summary, 
             caption = "Number of monitors by region",
             label = "tab:monitors"), 
      include.rownames = FALSE, 
      include.colnames = FALSE,
      hline.after = NULL)
sink()


#----------------------------get monitor data for analysis----------------------------------------------------#

PMreplaced.subset <- fread(file = "data/PMreplaced_subset_2005_2010.csv")[ , V1 := NULL]
PMreplaced.subset[ , date := as.Date(date)]


#----------------------------Daily PM Plot----------------------------------------------------#

#daily PM at these monitors
PMreplaced_daily_region <- PMreplaced.subset[ , list(dailyPM = mean(PM25, na.rm = TRUE),
                                     year = unique(year)),
                              by = c("date", "receptor.region")]


pdf(file = "../paper3_overleaf/figures/dailyPMreplaced.pdf", height = 3, width = 6.5)
ggplot(PMreplaced_daily_region, aes(x = date, y = dailyPM, color = receptor.region)) +
  geom_line() + scale_color_viridis(discrete = TRUE) +
  theme_classic() +
  theme(legend.position = "bottom",  
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 11)) +
  ylim(0,55) + labs(y = "PM2.5")
dev.off()




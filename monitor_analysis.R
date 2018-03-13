#Data analysis for paper 3
# determines subset of monitors to perfrom analysis on.
rm(list=ls())
setwd("/nfs/nsaph_ci3/users/ci3_kcummiskey/paper3")

library(data.table)

regions <- c("IndustrialMidwest", "Northeast", "Southeast")

years <- 2005:2010



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

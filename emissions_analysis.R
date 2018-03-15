#Data analysis for paper 3
# determines subset of powerplants to perfrom analysis on.
rm(list=ls())
source("paper3_functions.R")

library(data.table)
library(xtable)
library(ggplot2)
library(gridExtra)
library(grid)

regions <- c("IndustrialMidwest", "Northeast", "Southeast")

years <- 2005:2010

emissions.subset <- fread(file = "data/emissions_subset_2005-2010.csv")[ , V1 := NULL]

#-----------------------------plots for paper----------------------------------------------------#



PP.summary <- emissions.subset[ , list(days.operating = sum(SO2 > 0, na.rm = TRUE),
                                       totalSO2 = sum(SO2, na.rm = TRUE),
                                       PP.region = unique(PP.region)),
                                       by = c("year","PP")]
#avgSO2 on days operating
PP.summary[ , avgSO2.per.operating.day := round(totalSO2/days.operating,1)]


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

pdf(file = "../paper3_overleaf/figures/powerplant_summary.pdf", width = 6.5, height = 4.5)
grid.arrange(p1,
             p2,
             blank,
             layout_matrix = rbind(c(3,1),c(3,2)),
             heights = c(0.45,0.55),
             widths = c(0.035,0.965))
dev.off()


region_summary <- PP.summary[ year ==2005 , list(n = .N), by = "PP.region"]

sink(file = "../paper3_overleaf/tables/powerplants.tex")
print(xtable(region_summary, 
             caption = "Number of coal power plants by region",
             label = "tab:powerplants"), 
      include.rownames = FALSE, 
      include.colnames = FALSE,
      hline.after = NULL)
sink()

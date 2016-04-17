library(Hmisc)
library("latticeExtra")
library(lattice)
library(ggplot2)
library(stats)
library(graphics)

# Department of Finance (DOF)
# https://data.cityofnewyork.us/Housing-Development/Property-Valuation-and-Assessment-Data/rgy2-tti8

setwd('~/Desktop/Columbia/EDAV/Final Project')

finance = mdb.get('AVROLL.mdb')
avroll = finance$avroll
condensed = finance$`Condensed Roll Description`

# Calculate mean building price per zipcode
avg_vals = sapply(split(avroll$FULLVAL, avroll$ZIP), mean) 
avg_vals_df = data.frame(zipcode=names(avg_vals), value=avg_vals, row.names=NULL)
write.csv(avg_vals_df, file='zipcode_average_values.csv')

# Avg building price per zipcode is in avg_vals DF !!!

p <- ggplot(avroll)
pg <- p + geom_point(aes(sample = FULLVAL, colour = factor(ZIP)), stat = "qq", quantiles = ppoints(100)) + facet_grid(~EXT)

print(pg)


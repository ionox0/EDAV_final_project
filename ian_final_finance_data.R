library(Hmisc)
library("latticeExtra")
library(lattice)
library(ggplot2)
library(stats)
library(graphics)

setwd('~/Desktop/Columbia/EDAV/Final Project')

finance = mdb.get('AVROLL.mdb')
avroll = finance$avroll
condensed = finance$`Condensed Roll Description`

p <- ggplot(avroll)

pg <- p + geom_point(aes(sample = FULLVAL, colour = factor(ZIP)), stat = "qq", quantiles = ppoints(100)) + facet_grid(~EXT)

print(pg)


library(plyr)
library(lattice)
library(lubridate)
library(Hmisc)
library("latticeExtra")
library(lattice)
library(ggplot2)
library(stats)
library(graphics)
library(choroplethr)
library(choroplethrMaps)
library(choroplethrZip)

setwd('~/Desktop/Columbia/EDAV/Final Project')
df = read.csv('data/2015.csv')
df_slim = read.csv('slim_data.csv')
zillow = read.csv('Zip_Zhvi_Summary_AllHomes.csv')
azam = read.csv('azam_nyc_data.csv')



######################
#      311 Data      #
######################

# Subset to top 7 complaints
df = df[ which(
  df$Complaint.Type == "HEAT/HOT WATER" |
    df$Complaint.Type == "Blocked Driveway" |
    df$Complaint.Type == "Illegal Parking" |
    df$Complaint.Type == "UNSANITARY CONDITION" |
    df$Complaint.Type == "PAINT/PLASTER" |
    df$Complaint.Type == "PLUMBING" |
    df$Complaint.Type == "Noise - Street/Sidewalk"
), ]

# Remove bad zipcodes (takes a while)
df = df[!is.na(df$Incident.Zip),]
df$Incident.Zip = substr(df$Incident.Zip, 0, 5)

# Column for response time
closed_dates = mdy_hms(df$Closed.Date)
created_dates = mdy_hms(df$Created.Date)
df$response_time = difftime(closed_dates, created_dates, units="mins")
df$response_time = as.numeric(df$response_time, units="mins")

# Avg resp time per zipcode
avg_response_time = sapply(split(df_slim$response_time, df_slim$Incident.Zip), mean) 
avg_resp_per_zipcode = data.frame(zipcode=names(avg_response_time), time=avg_response_time, row.names=NULL)






#####################
#    Zillow Data    #
#####################

# Subset to just NYC
zillow <- zillow[ which(zillow$City=='New York'), ]

# Fix zipcodes with missing beginning 0
indices = nchar(zillow$RegionName) == 4
zillow$RegionName[indices] = paste0('0', zillow$RegionName[indices])

nyc_zips = read.csv('nyc_zipcodes.csv')
nyc_zillow = merge(zillow, nyc_zips, "left", by.x="RegionName", by.y="zip")

# -> Only 73 of 124 NYC zipcodes represented in Zillow data, going to try data from Azam




##############################
#    choroplethrZip Data     #
##############################
api.key.install(key='f57e5e8ebbf927e72dd8c007d24e81821ffcfa40')
choro = get_zip_demographics()






########################
#       Trellis        #
########################

df_merged = merge(df_slim, choro, "left", by.x="Incident.Zip", by.y="region")

df_merged$zip = as.factor(rel_df$zip)

df_merged = aggregate(response_time ~ Incident.Zip + Complaint.Type, df_merged, mean)

# Merge back in the prices and neighborhoods after the aggregation removes them
df_merged = merge(df_merged, choro, by.x="Incident.Zip", by.y="region")

zips_boroughs = unique(df[c("Incident.Zip", "Borough")])

df_merged = merge(df_merged,
                  zips_boroughs,
                  all.x=TRUE,
                  all.y=FALSE,
                  by.x="Incident.Zip",
                  by.y="Incident.Zip")

pg = ggplot(df_merged, aes(per_capita_income, response_time)) + 
  geom_point(shape = 1, aes(color = Borough)) +
  
  scale_y_log10(name="Average Response Time",
                breaks=c(1, 10, 120, 1460, 8760),
                labels=c("1 Hour", "10 Hours", "5 Days", "2 Months", "1 Year")) + 
  
  scale_x_log10(name="Average Income",
                breaks=c(100000, 1000000, 8000000),
                labels=c("100K", "1M", "8M")) + 
  
  facet_wrap(~Complaint.Type,nrow=2) + 
  ggtitle("Average Income vs Average Response Time per Zipcode across Top 7 Complaints")

pg + theme(axis.text=element_text(size=12),
           axis.title=element_text(size=16)
           # plot.margin=unit(c(10,10,10,10),"mm")
)

print(pg)



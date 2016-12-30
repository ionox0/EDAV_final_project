library(plyr)
library(lattice)
library(lubridate)

setwd('~/Desktop/Columbia/EDAV/Final Project')
df = read.csv('data/2015.csv')
zillow = read.csv('Zip_Zhvi_Summary_AllHomes.csv')



f <- function(x, output) {
  if (substr(x[3], 0, 1) == " ") {
    x[3] = gsub(" ", "0", x[3])
  }
}

apply(zillow, 1, f)

nyc_zips = read.csv('nyc_zipcodes.csv')
nyc_zillow = merge(zillow, nyc_zips, "left", by.x="RegionName", by.y="zip")




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

# Column for response time
closed_dates = mdy_hms(df$Closed.Date)
created_dates = mdy_hms(df$Created.Date)
df$response_time = difftime(closed_dates, created_dates, units="mins")
df$response_time = as.numeric(df$response_time, units="mins")

# Avg resp time per zipcode
avg_response_time = sapply(split(df$response_time, df$Incident.Zip), mean) 
avg_resp_per_zipcode = data.frame(zipcode=names(avg_response_time), time=avg_response_time, row.names=NULL)
write.csv(avg_resp_per_zipcode, file='response_time_average_values.csv')

# Remove bad zipcodes (takes a while)
df = df[!is.na(df$Incident.Zip),]
df$Incident.Zip = substr(df$Incident.Zip, 0, 5)





#####################
#    Zillow Data    #
#####################

zillow <- zillow[ which(zillow$City=='New York'), ]









########################
#       Trellis        #
########################

df_merged = merge(df, zillow, "left", by.x="Incident.Zip", by.y="zipcode")

# Aggregating
rel_df = data.frame(zip=df_merged$Incident.Zip,
                    type=df_merged$Complaint.Type,
                    time=df_merged$response_time,
                    borough=df_merged$Borough)
rel_df$zip = as.factor(rel_df$zip)

rel_df_avg = aggregate(time ~ zip + type, rel_df, mean)
# Merge back in the prices and neighborhoods after the aggregation removes them
rel_df_avg_with_prices = merge(rel_df_avg, avg_prices_df, by.x="zip", by.y="zipcode")

zips_boroughs = unique(df[c("Incident.Zip", "Borough")])

rel_df_avg_with_prices_boroughs = merge(rel_df_avg_with_prices,
                                        zips_boroughs,
                                        all.x=TRUE,
                                        all.y=FALSE,
                                        by.x="zip",
                                        by.y="Incident.Zip")

pg = ggplot(rel_df_avg_with_prices_boroughs, aes(value, time)) + 
  geom_point(shape = 1, aes(color = Borough)) + 
  
  scale_y_log10(name="Average Response Time",
                breaks=c(1, 10, 120, 1460, 8760),
                labels=c("1 Hour", "10 Hours", "5 Days", "2 Months", "1 Year")) + 
  
  scale_x_log10(name="Average Building Value",
                breaks=c(1000000, 10000000, 100000000),
                labels=c("1M", "10M", "100M")) + 
  
  facet_wrap(~type,nrow=2) + 
  ggtitle("Average Building Value vs Average Response Time per Zipcode across Top 7 Complaints")

pg + theme(axis.text=element_text(size=12),
           axis.title=element_text(size=16)
           # plot.margin=unit(c(10,10,10,10),"mm")
)

print(pg)



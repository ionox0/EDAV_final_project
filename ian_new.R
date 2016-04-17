library(plyr)
library(lattice)
library(lubridate)

setwd('~/Desktop/Columbia/EDAV/Final Project')
df = read.csv('data/2015.csv')
finance = mdb.get('AVROLL.mdb')
avroll = finance$avroll



###########################
# Complaints data section #
###########################

# Remove bad zipcodes (takes a while)
df = df[!is.na(df$Incident.Zip),]
df$Incident.Zip = substr(df$Incident.Zip, 0, 5)

# Subset to top seven complaint types
df = subset(df, Complaint.Type == "HEAT/HOT WATER" | 
                 Complaint.Type == "Blocked Driveway" | 
                 Complaint.Type == "Illegal Parking" |
                 Complaint.Type == "UNSANITARY CONDITION" |
                 Complaint.Type == "PAINT/PLASTER" |
                 Complaint.Type == "PLUMBING" |
                 Complaint.Type == "Noise - Street/Sidewalk")

# Column for response time
closed_dates = mdy_hms(df$Closed.Date)
created_dates = mdy_hms(df$Created.Date)
df$response_time = difftime(closed_dates, created_dates, units="hours")
df$response_time = as.numeric(df$response_time, units="hours")




################################
# Building prices data section #
################################

# Calculate mean building price per zipcode
avg_prices = sapply(split(avroll$FULLVAL, avroll$ZIP), mean) 
avg_prices_df = data.frame(zipcode=names(avg_prices), value=avg_prices, row.names=NULL)




########################
# Combine into Trellis #
########################
df_merged = merge(df, avg_prices_df, "left", by.x="Incident.Zip", by.y="zipcode")

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

# -> No relationship b/w avg building price and response time. Show lm stats to confirm:
fit = lm(value ~ time, data = rel_df_avg_with_prices)
summary(fit)
plot(value ~ time, data = rel_df_avg_with_prices)
abline(fit)



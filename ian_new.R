library(plyr)
library(lattice)
library(lubridate)
library(Hmisc)
library("latticeExtra")
library(lattice)
library(ggplot2)
library(stats)
library(graphics)
library(randomForest)


setwd('~/Desktop/Columbia/EDAV/Final Project')
# Main 311 Dataset
df = read.csv('data/2015.csv')
# DOF Building Prices Dataset
finance = mdb.get('AVROLL.mdb')
avroll = finance$avroll
# Slimmed-down dataset with Demographics and Weather data
data_slim = read.csv('slimmed_demographics_weather.csv')



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



#####################################
# BULDING VALUE by Resp Time by Zip #
#####################################

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



##############################
# INCOME vs Resp Time by Zip #
##############################

boroughs_zips = unique(df[,c("Incident.Zip", "Borough")])
df_merged = merge(data_slim, boroughs_zips, "left", by.x="incident_zip", by.y="Incident.Zip")

# Aggregating
relevant_df = data.frame(zip=df_merged$incident_zip,
                    type=df_merged$complaint_type,
                    time=df_merged$response_time,
                    borough=df_merged$Borough,
                    income=df_merged$per_capita_income)
relevant_df$zip = as.factor(rel_df$zip)

rel_df_avg = aggregate(time ~ zip + type, relevant_df, mean)
# Merge back in the borough and income after the aggregation removes them
remerged = merge(rel_df_avg, boroughs_zips, "left", by.x="zip", by.y="Incident.Zip")
dedupe = unique(relevant_df[c("zip", "income")])
remerged = merge(remerged, dedupe, by.x="zip", by.y="zip",
                 all.x=FALSE,
                 all.y=FALSE)

pg = ggplot(remerged, aes(income, time)) + 
  geom_point(shape = 1, aes(color = Borough)) + 
  
  scale_y_log10(name="Average Response Time",
                breaks=c(1, 10, 120, 1460, 8760),
                labels=c("1 Hour", "10 Hours", "5 Days", "2 Months", "1 Year")) + 
  
  scale_x_log10(name="Average Income",
                breaks=c(10000, 20000, 50000, 100000, 1000000),
                labels=c("10K", "20K", "50K", "100K", "1M")) + 
  
  facet_wrap(~type,nrow=2) + 
  ggtitle("Average Income vs Average Response Time per Zipcode across Top 7 Complaints")

pg + theme(axis.text=element_text(size=12),
           axis.title=element_text(size=16)
)

print(pg)

# -> No relationship b/w avg building price and response time. Show lm stats to confirm:
fit = lm(value ~ time, data = rel_df_avg_with_prices)
summary(fit)
plot(value ~ time, data = rel_df_avg_with_prices)
abline(fit)



##################################################
#   Resp Time Predictions for Noise complaints   #
##################################################

pred_data = data_slim[complete.cases(data_slim),]
pred_data_noise = subset(pred_data, complaint_type == "Noise - Street/Sidewalk")

# One hot encoding for zipcodes (comment out next two lines to leave out):
pred_data_noise.f = factor(pred_data_noise$incident_zip)
dummies = model.matrix(~pred_data_noise.f)
pred_data_noise = cbind(pred_data_noise, dummies)

n_sample = floor(0.90 * nrow(pred_data_noise))
set.seed(20)

train_ind <- sample(seq_len(nrow(pred_data_noise)), size = n_sample)
train <- pred_data_noise[train_ind,]
test <- pred_data_noise[-train_ind,]

exclude_cols <- c('X',
                  'EST',
                  'created_date',
                  'agency',
                  'complaint_type',
                  'incident_zip',
                  '(Intercept)')

fit <- randomForest(response_time ~ .,
                    data=train[ !names(train) %in% exclude_cols ],
                    importance=TRUE) #, ntree=2000)

round(importance(fit), 2)
preds = (predict(fit, test))
errors = abs(test$response_time - preds)
print(mean(errors))
# Compare to accuracy of simply predicting the mean
mean(test$response_time)


######################################
# Prediction using only top features #
######################################

rf <- randomForest(response_time ~ 
                    Mean.Humidity +
                    Mean.TemperatureF +
                    PrecipitationIn + 
                    total_population + 
                    median_age +
                    per_capita_income +
                    median_rent +
                    percent_black +
                    percent_asian +
                    percent_hispanic +
                    percent_white,
                    data=train, importance=TRUE,
                    proximity=TRUE)

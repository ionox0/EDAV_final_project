

#### DATASET CREATION ####


# Read the weather data
Weather <- read.csv(file="NYC_Weather_2015.csv",header=TRUE,sep=","
                    ,stringsAsFactors=FALSE)

Weather$EST <- as.Date(Weather$EST,format="%Y-%m-%d")

# Replace 'T' for trace precipitation with very small value
dum_precip <- ifelse(Weather$PrecipitationIn=='T',0.00001,
                     Weather$PrecipitationIn)
dum_precip <- as.numeric(dum_precip)
Weather$PrecipitationIn <- dum_precip

# Add variable for weather season
season_function <- function(element){
  element <- as.Date(element,format="%Y-%m-%d")
  if(element < "2015-03-20" | element >= "2015-12-21"){
    return("Winter")
  }
  if(element >= "2015-03-20" & element < "2015-06-21"){
    return("Spring")
  }
  if(element >= "2015-06-21" & element < "2015-09-23"){
    return("Summer")
  }
  else{
    return("Autumn")
  }
}
temp_season <- sapply(Weather$EST,season_function)
Weather$Season <- temp_season


Weather <- Weather[,c("EST","Mean.TemperatureF","Mean.Humidity","PrecipitationIn")]

df_slim <- read.csv('slimmed-with-demographics.csv')

df_slim$Weather.Join.Key <- as.Date(df_slim$created_date,
                                    format="%m/%d/%Y")

df_merge <- merge(df_slim,Weather,by.x="Weather.Join.Key",
                  by.y="EST",all.x=TRUE)

names(df_merge)[1] <- "EST"

write.csv(df_merge, file='slimmed_demographics_weather.csv')







#### EXPLORATORY ANALYSIS ####

df_merge$Year <- format(strptime(df_merge$created_date,
                                 format = "%m/%d/%Y %I:%M:%S %p"),"%Y")
df_merge <- df_merge[df_merge$Year=="2015",]
df_merge$Year <- NULL

one_way.fit <- aov(log(response_time+1) ~ complaint_type, data = df_merge)
layout(matrix(c(1,2,3,4),2,2))
plot(one_way.fit)
summary(one_way.fit)
AIC(one_way.fit)
BIC(one_way.fit)


#### PREDICTIVE MODELING ####

df_heat <- df_merge[which(df_merge$complaint_type=="HEAT/HOT WATER"),c(5:17)]

df_heat <- df_heat[!(is.na(df_heat$response_time)),]



## 20% of the sample size for training, 40% for validation, 40% for testing
n <- nrow(df_heat)
n_sample_train <- floor(0.20 * n)
n_sample_val <- floor(0.40 * n)
n_sample_test <- ceiling(0.40 * n)
set.seed(1)

train_ind <- sample(seq_len(n), size = n_sample_train)
non_train_ind <- seq_len(n)[!(seq_len(n) %in% train_ind)]
val_ind <- sample(non_train_ind, size = n_sample_val)
test_ind <- seq_len(n)[!(seq_len(n) %in% val_ind) & !(seq_len(n) %in% train_ind)]

X_train <- df_heat[train_ind, !(names(df_heat) %in% c("response_time"))]
y_train <- df_heat[train_ind, c("response_time")]

X_val <- df_heat[val_ind, !(names(df_heat) %in% c("response_time"))]
y_val <- df_heat[val_ind, c("response_time")]

X_test <- df_heat[test_ind, !(names(df_heat) %in% c("response_time"))]
y_test <- df_heat[test_ind, c("response_time")]

model <- glm.fit(x = X_train, y = y_train, family = poisson()) 
glm_model.fit <- glm(response_time ~ per_capita_income+total_population+
                   percent_white+percent_black+percent_asian+
                   percent_hispanic+median_rent+median_age+
                   Mean.TemperatureF+Mean.Humidity+PrecipitationIn,
                 data=df_heat[train_ind,],family=poisson())
summary(glm_model.fit)

glm_model.preds <- predict(glm_model.fit,X_val,type ="response")
glm_model.error <- abs(glm_model.preds - y_val)
mean(glm_model.error)
# 39.39592

mean(y_val)
# 80.9045

base_error <- abs(y_val - mean(y_val))
mean(base_error)
# 39.82282

library(randomForest)

#df_heat0 <- subset(df_heat,select=incident_zip)
#df_heat1 <- cbind(model.matrix(~as.factor(incident_zip)-1,df_heat0),
#                  subset(df_heat,select=-incident_zip))
#zip_names <- paste(names(df_heat1)[1:179],collapse=" + ")

train <- df_heat[train_ind,]

rf <- randomForest(response_time ~ 
                     total_population +
#                     percent_white + 
#                     percent_black + 
#                     percent_asian +
#                     percent_hispanic +
                     median_rent + 
                     median_age + 
                     Mean.TemperatureF +
                     Mean.Humidity +
                     PrecipitationIn,
                   data=train, importance=TRUE,
                   proximity=FALSE,ntree=500)

val <- df_heat[val_ind,]

preds <- (predict(rf, val))
model_errors <- abs(val$response_time - preds)
print(mean(model_errors))
# 36.63985

print(median(model_errors))
# 28.93485

base_errors.median <- abs(y_val - median(y_val))
print(median(base_errors.median))
# 25.9325

mean(y_val)
# 80.88809

median(y_val)
# 72

base_errors.mean <- abs(y_val - mean(y_val))
mean(base_error)
# 39.79262


round(importance(rf), 2)




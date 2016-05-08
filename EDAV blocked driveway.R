getwd()
setwd("C:/Users/ahassan/Desktop")
library(leaps)


data_file = read.csv('edited.csv', header = TRUE)

out <- glm(response_time ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8+ X9+ X10
           + X11 + X12 + per_capita_income + total_population + percent_white + percent_black
           + percent_asian + percent_hispanic + median_rent + median_age + Mean.TemperatureF + PrecipitationIn 
           + Mean.Humidity, data = data_file)

summary(out)




leaps=regsubsets(response_time ~ X9 + X10 + X11 + per_capita_income
                 + percent_white + percent_hispanic + median_age + Mean.TemperatureF 
                 + PrecipitationIn, data = data_file, nbest=2)

plot(leaps, scale="adjr2")

out_2 <- glm(response_time ~ X9+ X10 + X11 + per_capita_income+ percent_white + percent_hispanic +  median_age + Mean.TemperatureF + PrecipitationIn 
        , data = data_file)

summary(out_2)

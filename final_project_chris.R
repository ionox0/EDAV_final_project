library(ggplot2)
library(leaflet) 
library(maps)
library(ggmap)

# Read the weather data
Weather <- read.csv(file="NYC_Weather_2015.csv",header=TRUE,sep=","
                      ,stringsAsFactors=FALSE)

# Read 311 case data for 2015
setwd("311Data")

Data_2015 <- read.csv(file="2015.csv",header=TRUE,sep=","
                        ,stringsAsFactors=FALSE)

# Subset to top seven complaint types 
top_complaints = Data_2015[
    Data_2015$Complaint.Type %in% c("HEAT/HOT WATER",
                                    "Blocked Driveway",
                                    "Illegal Parking",
                                    "UNSANITARY CONDITION",
                                    "PAINT/PLASTER",
                                    "PLUMBING",
                                    "Noise - Street/Sidewalk"
                                    )
  ,]


# Create response time variable
top_complaints$Response.Time <- difftime(strptime(top_complaints$Closed.Date, 
                                                  format="%m/%d/%Y %H:%M:%S %p"), 
                                         strptime(top_complaints$Created.Date, 
                                                  format="%m/%d/%Y %H:%M:%S %p"),
                                         units="hours")
# n <- dim(top_complaints)[1]
# for(i in 1:n){
#   temp_create_date <- strptime(top_complaints$Created.Date[i],
#                                format = "%m/%d/%Y %I:%M:%S %p")
#   temp_close_date <- strptime(top_complaints$Closed.Date[i], 
#                               format = "%m/%d/%Y %I:%M:%S %p")
#   if (is.na(temp_close_date)){
#     temp_close_date <- strptime(format(Sys.time(), "%m/%d/%Y %I:%M:%S %p"),
#                                 format = "%m/%d/%Y %I:%M:%S %p")
#   }
#   top_complaints$Response.Time[i] <- difftime(temp_close_date,
#                                                 temp_create_date,
#                                                 units="hours")
# }


top_complaints$Year <- format(strptime(top_complaints$Created.Date,
                                         format = "%m/%d/%Y %I:%M:%S %p"),"%Y")

top_complaints <- top_complaints[top_complaints$Year=="2015",]

# Join weather data to 311 data
Weather$EST <- as.Date(Weather$EST,format="%Y-%m-%d")


top_complaints$Weather.Join.Key <- as.Date(top_complaints$Created.Date,
                                             format="%m/%d/%Y")

Weather_311 <- merge(top_complaints,Weather,by.x="Weather.Join.Key",
                     by.y="EST",all.x=TRUE)

Weather_311$Response.Time <- as.numeric(Weather_311$Response.Time)

# Take small sample of dataset
n <- dim(top_complaints)[1]
SampleSize <- 1000
SampleIndices <- sample(c(1:n),size=SampleSize,replace=F)
Data_2015_Sample <- top_complaints[SampleIndices,]
Data_2015_Sample <- Data_2015_Sample[!is.na(Data_2015_Sample$Latitude),]
SampleSize <- dim(Data_2015_Sample)[1]

Avg_Response_Temp <- aggregate(Weather_311[c("Response.Time")],
          by=list(Mean.TemperatureF=Weather_311$Mean.TemperatureF,
                  Complaint.Type=Weather_311$Complaint.Type
                  ),
          FUN=mean,na.rm=TRUE)

# Lattice plots of case response time per complaint type
ggplot(Avg_Response_Temp,aes(Mean.TemperatureF,Response.Time))+
  geom_point()+facet_wrap(~Complaint.Type,nrow=2) + 
  scale_y_log10(breaks=c(1, 10, 120, 1460, 8760),
                labels=c("1 Hour", "10 Hours", "5 Days", "2 Months", "1 Year"))+
  labs(x="Mean Temperature (deg. F)", y="Response Time",
       title="Response Time per Complaint Type by Average Temperature")+ 
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


Avg_Response_Temp <- aggregate(Weather_311[c("Response.Time")],
                               by=list(Mean.TemperatureF=Weather_311$Mean.TemperatureF,
                                       Complaint.Type=Weather_311$Complaint.Type,
                                       Events=Weather_311$Events
                               ),
                               FUN=mean,na.rm=TRUE)

ggplot(Avg_Response_Temp,aes(Mean.TemperatureF,Response.Time,colour=Events))+
  geom_point()+facet_wrap(~Complaint.Type,nrow=2) + 
  scale_y_log10(breaks=c(1, 10, 120, 1460, 8760),
                labels=c("1 Hour", "10 Hours", "5 Days", "2 Months", "1 Year"))+
  labs(x="Mean Temperature (deg. F)", y="Response Time",
       title="Response Time per Complaint Type by Avg. Temp. & Weather Event")+ 
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))



Avg_Response_Wind <- aggregate(Weather_311[c("Response.Time")],
                               by=list(Mean.Wind.SpeedMPH=Weather_311$Mean.Wind.SpeedMPH,
                                       Complaint.Type=Weather_311$Complaint.Type
                               ),
                               FUN=mean,na.rm=TRUE)

ggplot(Avg_Response_Wind,aes(Mean.Wind.SpeedMPH,Response.Time))+
  geom_point()+facet_wrap(~Complaint.Type,nrow=2) + 
  scale_y_log10(breaks=c(1, 10, 120, 1460, 8760),
                labels=c("1 Hour", "10 Hours", "5 Days", "2 Months", "1 Year"))+
  labs(x="Mean Wind Speed (MPH)", y="Response Time",
       title="Response Time per Complaint Type by Average Wind Speed")+ 
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


Avg_Response_Wind <- aggregate(Weather_311[c("Response.Time")],
                               by=list(Mean.Wind.SpeedMPH=Weather_311$Mean.Wind.SpeedMPH,
                                       Complaint.Type=Weather_311$Complaint.Type,
                                       Events=Weather_311$Events
                               ),
                               FUN=mean,na.rm=TRUE)

ggplot(Avg_Response_Wind,aes(Mean.Wind.SpeedMPH,Response.Time,colour=Events))+
  geom_point()+facet_wrap(~Complaint.Type,nrow=2) + 
  scale_y_log10(breaks=c(1, 10, 120, 1460, 8760),
                labels=c("1 Hour", "10 Hours", "5 Days", "2 Months", "1 Year"))+
  labs(x="Mean Wind Speed (MPH)", y="Response Time",
       title="Response Time per Complaint Type by Avg. Wind & Weather Events")+ 
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


Avg_Response_Vis <- aggregate(Weather_311[c("Response.Time")],
                               by=list(Mean.VisibilityMiles=Weather_311$Mean.VisibilityMiles,
                                       Complaint.Type=Weather_311$Complaint.Type
                               ),
                               FUN=mean,na.rm=TRUE)

ggplot(Avg_Response_Vis,aes(Mean.VisibilityMiles,Response.Time))+
  geom_point()+facet_wrap(~Complaint.Type,nrow=2) + 
  scale_y_log10(breaks=c(1, 10, 120, 1460, 8760),
                labels=c("1 Hour", "10 Hours", "5 Days", "2 Months", "1 Year"))+
  labs(x="Mean Visibility (Miles)", y="Response Time",
       title="Response Time per Complaint Type by Average Visibility")+ 
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


Avg_Response_Vis <- aggregate(Weather_311[c("Response.Time")],
                              by=list(Mean.VisibilityMiles=Weather_311$Mean.VisibilityMiles,
                                      Complaint.Type=Weather_311$Complaint.Type,
                                      Events=Weather_311$Events
                              ),
                              FUN=mean,na.rm=TRUE)

ggplot(Avg_Response_Vis,aes(Mean.VisibilityMiles,Response.Time,colour=Events))+
  geom_point()+facet_wrap(~Complaint.Type,nrow=2) + 
  scale_y_log10(breaks=c(1, 10, 120, 1460, 8760),
                labels=c("1 Hour", "10 Hours", "5 Days", "2 Months", "1 Year"))+
  labs(x="Mean Visibility (Miles)", y="Response Time",
       title="Response Time per Complaint Type by Avg. Visibility & Weather Events")+ 
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))



# Basic exploratory plots of complaint types on maps
map <- get_map(location = "New York City, NY", zoom = 10) 
mapPoints <- ggmap(map) + 
  geom_point(aes(x = Longitude, y = Latitude,color=Agency), 
             data = Data_2015_Sample, alpha = 0.5) +
  labs(title = "311 Complaint Locations by Agency: 2015") +
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
mapPoints

mapPoints <- ggmap(map) + 
  geom_point(aes(x = Longitude, y = Latitude,color=Agency,size=Response.Time), 
             data = Data_2015_Sample, alpha = 0.5) +
  labs(title = "311 Complaint Locations by Agency and Response Time: 2015") +
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
mapPoints

mapPoints <- ggmap(map) + 
  geom_point(aes(x = Longitude, y = Latitude,color=Agency,size=Response.Time), 
             data = subset(Data_2015_Sample,Status=="Open"), alpha = 0.5) +
  labs(title = "Open 311 Complaint Locations by Agency and Response Time: 2015") +
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
mapPoints

mapPoints <- ggmap(map) + 
  geom_point(aes(x = Longitude, y = Latitude,color=Agency,size=Response.Time), 
             data = subset(Data_2015_Sample,Status=="Closed"), alpha = 0.5) +
  labs(title = "Closed 311 Complaint Locations by Agency and Response Time: 2015") +
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
mapPoints


mapPoints <- ggmap(map) + 
  geom_point(aes(x = Longitude, y = Latitude,color=Agency,size=Response.Time), 
             data = subset(Data_2015_Sample,Complaint.Type=="HEAT/HOT WATER"), alpha = 0.5) +
  labs(title = "Heat/Hot Water 311 Complaint Locations by Agency and Response Time: 2015") +
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
mapPoints

mapPoints <- ggmap(map) + 
  geom_point(aes(x = Longitude, y = Latitude,color=Agency,size=Response.Time), 
             data = subset(Data_2015_Sample,Complaint.Type=="HEAT/HOT WATER"), alpha = 0.5) +
  labs(title = "Heat/Hot Water 311 Complaint Locations by Agency and Response Time: 2015") +
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
mapPoints
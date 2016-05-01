library(ggplot2)
library(leaflet) 
library(maps)
library(ggmap)
library(data.table)

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

# Complaints that were opened and closed immediately
bogus_response <- top_complaints[which(top_complaints$Response.Time == 0),]

table(bogus_response$Agency)
# All 6051 are from HPD

table(bogus_response$Agency,bogus_response$Complaint.Type)
# 5089 Heat/Hot Water, 38 Paint/Plaster, 89 Plumbing, 73 Unsanitary Condition

table(bogus_response$Resolution.Description)
# 4187 do not have any complaint resolution description
# 231 are duplicate complaints, 626 + 5 + 826 confirmed that issue was corrected,
# 39 were inspected with no violations issued, 25 were inspected with violations issued,
# 50 were inspected with violations previously issued, 
# 2 + 59 were unable to be inspected and the case was closed
# 1 was unable to be inspected, but violations were issued

# Plot locations of zero response time complaints
map <- get_map(location = "New York City, NY", zoom = 10) 
mapPoints <- ggmap(map) + 
  geom_point(aes(x = Longitude, y = Latitude,color=Complaint.Type), 
             data = bogus_response, alpha = 0.5) +
  labs(title = "Complaints with Zero Response Time: 2015") +
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
mapPoints


top_complaints$Year <- format(strptime(top_complaints$Created.Date,
                                         format = "%m/%d/%Y %I:%M:%S %p"),"%Y")

top_complaints <- top_complaints[top_complaints$Year=="2015" & 
                                 top_complaints$Response.Time != 0,]

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

# Join weather data to 311 data
top_complaints$Weather.Join.Key <- as.Date(top_complaints$Created.Date,
                                             format="%m/%d/%Y")

Weather_311 <- merge(top_complaints,Weather,by.x="Weather.Join.Key",
                     by.y="EST",all.x=TRUE)

Weather_311$Response.Time <- as.numeric(Weather_311$Response.Time)


Avg_Response_Temp <- aggregate(Weather_311[c("Response.Time")],
          by=list(Mean.TemperatureF=Weather_311$Mean.TemperatureF,
                  Complaint.Type=Weather_311$Complaint.Type
                  ),
          FUN=mean,na.rm=TRUE)

# Lattice plots of case response time per complaint type
W3 <- data.table(Weather_311)
Avg_Response_Precip <- W3[is.na(Complaint.Type)==FALSE, 
                          list(Mean.Response.Time=sum(mean(Response.Time)), 
                                 Count=sum(length(Response.Time))), 
                                   by=list(PrecipitationIn,Complaint.Type,Season)]

ggplot(Avg_Response_Precip,aes(PrecipitationIn,Mean.Response.Time,colour=Season,
                               size=Count))+
  geom_point()+facet_wrap(~Complaint.Type,nrow=2) + 
  scale_y_log10(breaks=c(1, 10, 120, 1460, 8760),
                labels=c("1 Hour", "10 Hours", "5 Days", "2 Months", "1 Year"))+
  labs(x="Total Precipitation (in.)", y="Response Time",
       title="Response Time per Complaint Type by Precipitation & Season")+ 
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))



Avg_Response_Humid <- W3[is.na(Complaint.Type)==FALSE, 
                         list(Mean.Response.Time=sum(mean(Response.Time)), 
                                 Count=sum(length(Response.Time))), 
                          by=list(Mean.Humidity,Complaint.Type,Season)]

ggplot(Avg_Response_Humid,aes(Mean.Humidity,Mean.Response.Time,colour=Season,
                               size=Count))+
  geom_point()+facet_wrap(~Complaint.Type,nrow=2) + 
  scale_y_log10(breaks=c(1, 10, 120, 1460, 8760),
                labels=c("1 Hour", "10 Hours", "5 Days", "2 Months", "1 Year"))+
  labs(x="Mean Humidity (%)", y="Response Time",
       title="Response Time per Complaint Type by Humidity & Season")+ 
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

Avg_Response_Temp <- W3[is.na(Complaint.Type)==FALSE, 
                         list(Mean.Response.Time=sum(mean(Response.Time)), 
                              Count=sum(length(Response.Time))), 
                         by=list(Mean.TemperatureF,Complaint.Type,Season)]

ggplot(Avg_Response_Temp,aes(Mean.TemperatureF,Mean.Response.Time,colour=Season,
                              size=Count))+
  geom_point()+facet_wrap(~Complaint.Type,nrow=2) + 
  scale_y_log10(breaks=c(1, 10, 120, 1460, 8760),
                labels=c("1 Hour", "10 Hours", "5 Days", "2 Months", "1 Year"))+
  labs(x="Mean Temperature (deg. F)", y="Response Time",
       title="Response Time per Complaint Type by Avg. Temp. & Season")+ 
  theme(plot.title = element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

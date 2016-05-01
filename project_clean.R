library(lubridate)
library(dplyr)
library(tidyr)


#import data; original obs = 1316806
mydata <- read.csv("2015.csv")

#drop columns
drops <- c("Unique.Key", "Agency.Name", "Incident.Address", 
           "Street.Name", "Address.Type", "Facility.Type", 
           "City", "X.Coordinate..State.Plane.", "Y.Coordinate..State.Plane.", "Location")
mydata <- mydata[ , !(names(mydata) %in% drops)]

#convert date format and calculate difference
mydata$start<- strptime(mydata$Created.Date, format = "%m/%d/%Y %I:%M:%S %p")
mydata$end<- strptime(mydata$Closed.Date, format = "%m/%d/%Y %I:%M:%S %p")
mydata$difftime <- difftime(mydata$end, mydata$start, units = "hours")

#get rid of empty long/lats and negative response
mydata <- mydata[rowSums(is.na(mydata[,c(15,16)]))==0,]

#get rid of extreme value year 2100
complete<- subset(mydata, difftime > 0)


#plot counts for top 7 complaints w/zero difftime time
library(ggplot2)

top_complaints <- subset(complete, Complaint.Type == "HEAT/HOT WATER" | 
                           Complaint.Type == "Blocked Driveway" | 
                           Complaint.Type == "Illegal Parking" |
                           Complaint.Type == "UNSANITARY CONDITION" |
                           Complaint.Type == "PAINT/PLASTER" |
                           Complaint.Type == "PLUMBING" |
                           Complaint.Type == "Noise - Street/Sidewalk")
top_complaints$Complaint.Type <- toupper(top_complaints$Complaint.Type)
new <- count(top_complaints$Complaint.Type)
new$Agency <- c("NYPD","HPD","NYPD","NYPD","HPD","HPD","HPD")
# Generate data
top_graph <- ggplot(new, aes(x = reorder(x, -freq), y = freq, fill = Agency))

# By default, uses stat="bin", which gives the count in each category
top_graph + geom_bar(stat = "identity", width = .5) + labs(x = "Complaint", y = "Count", title = "Count of Complaint by Agency")


#Box Plot
NYPD <- subset(top_complaints, Agency == "NYPD")
x <- ggplot(NYPD, aes(x = Complaint.Type, y = difftime))
x<- x + geom_boxplot(outlier.shape = NA, width = .4) + 
  coord_cartesian(ylim = c(0,15)) + 
  labs(x = "Complaint Type", y = "Time in Hours", title = "Response Time by Complaint Type for NYPD")

HPD <- subset(top_complaints, Agency == "HPD")
y <- ggplot(HPD, aes(x = Complaint.Type, y = difftime))
y<- y + geom_boxplot(outlier.shape = NA, width = .4) + 
  coord_cartesian(ylim = c(0,1600)) +  
  labs(x = "Complaint Type", y = "Time in Hours", title = "Response Time by Complaint Type for HPD")

library(gridExtra)
grid.arrange(x, y)


#take a sample
NYPDsample <- NYPD[sample(1:nrow(NYPD), 10000, replace=FALSE),]
HPDsample  <- HPD[sample(1:nrow(HPD), 2000, replace=FALSE),]

#plot of agency and days to complete
library(ggmap)
mymap <- get_map(location = "New York City", maptype = "roadmap", zoom = 11)

ggmap(mymap)+geom_point(aes(x = Longitude, y = Latitude, colour = NYPDsample$Complaint.Type, size = NYPDsample$difftime), 
                        data = NYPDsample, alpha = .5) + ggtitle('NYPD Response Times')

ggmap(mymap)+geom_point(aes(x = Longitude, y = Latitude, colour = HPDsample$Complaint.Type, size = HPDsample$difftime), 
                        data = HPDsample, alpha = .5) + ggtitle('HPD Response Times')

#restaurant data
rest <- read.csv("rest.csv", sep = "\t")
rest$Type <- "Restaurant"
rest_nypd <- subset(NYPDsample, Complaint.Type == "NOISE - STREET/SIDEWALK")
final <- rest_nypd[, c("Latitude", "Longitude", "Complaint.Type")]
names(final) <- c("lat", "long", "Type")
binded <- rbind(final, rest)
ggmap(mymap) + geom_point(data = binded, aes(x=long, y=lat, colour = Type), alpha = .5)



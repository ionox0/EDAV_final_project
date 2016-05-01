library(plyr)
library(lattice)
library(lubridate)

setwd('~/Desktop/Columbia/EDAV/Final Project')
df = read.csv('data/2015.csv')

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


df_merged = merge(df, avg_vals_df, "left", by.x="Incident.Zip", by.y="zipcode")




# Complaint types (133)
unique(unlist(df$Complaint.Type, use.names = FALSE))


#########################
#    The Lattice plot   #
#########################

# need:
# 1. zipcode
# 2. avg building price
# 3. avg response time
# 4. incident type
# idea to color by neighborhood 

# Avg response time per zipcode is in avg_response_time_df
# Subset to just manhattan zipcodes
manhattan_complaints_subset = subset(complaints_subset, subset = as.character(Incident.Zip) %in% manhattan_zips)

pg = ggplot(df_merged, aes(value, response_time)) + geom_point(shape = 1) +
  facet_grid(~Complaint.Type)

print(pg)

aggdata = aggregate(df_merged, by=list(df_merged$Incident.Zip, df_merged$Incident.Type,
                                       df_merged$value), FUN=mean, na.rm=TRUE)


############################
#       Complaints         #
############################

# Histogram of call types
count(df$Complaint.Type)

# Complaint type counts ordered by freq
counts = count(df$Complaint.Type)
ordered_counts = counts[order(-counts$freq),] 
write.table(ordered_counts, file = "MyData.csv", row.names=FALSE, sep="\t\t\t\t")
cat(capture.output(ordered_counts), file = 'dframe.txt', sep = '\n')

write.csv(unique(unlist(df$Agency.Name, use.names = FALSE)), file = "MyData.csv")

# Subset to just selected complaint types
df = df[ which(
    df$Complaint.Type == "HEAT/HOT WATER" |
    df$Complaint.Type == "Blocked Driveway" |
    df$Complaint.Type == "Illegal Parking" |
    df$Complaint.Type == "UNSANITARY CONDITION" |
    df$Complaint.Type == "PAINT/PLASTER" |
    df$Complaint.Type == "PLUMBING" |
    df$Complaint.Type == "Noise - Street/Sidewalk"
  ), ]
youth = subset(df, Complaint.Type == "Disorderly Youth")

# manhattan zipcodes
zips_data = read.table('manhattan_zips.tsv', sep='\t')
zips_full = data.frame(borough=character(0), neighborhood=character(0), zip=character(0), stringsAsFactors=FALSE)
for (i in 1:42) {
  zips = unlist(strsplit(as.character(zips_data[i,3]), ", "))
  for (j in 1:length(zips)) {
    a = as.character(zips_data[i,1])
    b = as.character(zips_data[i,2])
    c = as.character(zips[j])
    zips_full[i*3+j - 3,] = c(a, b, c)
  }
}

# Join borough and neighborhood data to complaints data (takes forever)
df_merged = merge(df, zips_full, "left", by.x="Incident.Zip", by.y="zip")
write.csv(df_merged, file='data_with_neighborhoods.csv')

# Create list of unique complaint types
types = unique(df$Complaint.Type)

# Use to subset our complaints
complaints_subset = subset(df_merged, subset = Complaint.Type %in% types[20:30])
manhattan_zips = zips_full[zips_full$borough == 'Manhattan',]$zip
manhattan_complaints_subset = subset(complaints_subset, subset = as.character(Incident.Zip) %in% manhattan_zips)

# Trellis hist of first 50 complaint types by zip
histogram(~Complaint.Type | neighborhood, data=manhattan_complaints_subset,
          xlab="X", ylab="Y", 
          main="Title",
          scales=list(x=list(rot=45)))

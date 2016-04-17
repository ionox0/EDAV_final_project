library(plyr)
library(lattice)

df = read.csv('data/2015.csv')

setwd('~/Desktop/Columbia/EDAV/Final Project')

# Column for response time
df$response_time = as.Date(df$Closed.Date, "%m/%d/%Y %H:%M:%S %p") - as.Date(df$Created.Date, "%m/%d/%Y %H:%M:%S %p")

# Complaint types (133)
unique(unlist(df$Complaint.Type, use.names = FALSE))

# Histogram of call types
count(df$Complaint.Type)




# Complaint type counts ordered by freq
counts = count(df$Complaint.Type)
ordered_counts = counts[order(-counts$freq),] 
write.table(ordered_counts, file = "MyData.csv", row.names=FALSE, sep="\t\t\t\t")
cat(capture.output(ordered_counts), file = 'dframe.txt', sep = '\n')


write.csv(unique(unlist(df$Agency.Name, use.names = FALSE)), file = "MyData.csv")


# Subset to just selected complaint types
complaints_subset = df[
    df$Complaint.Type == "HEAT/HOT WATER" ||
    df$Complaint.Type == "Blocked Driveway" ||
    df$Complaint.Type == "Illegal Parking" ||
    df$Complaint.Type == "UNSANITARY CONDITION" ||
    df$Complaint.Type == "PAINT/PLASTER" ||
    df$Complaint.Type == "PLUMBING" ||
    df$Complaint.Type == "Noise - Street/Sidewalk"
  ]
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

# join borough and neighborhood data to complaints data (takes forever)
df_merged = merge(df, zips_full, "left", by.x="Incident.Zip", by.y="zip")

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

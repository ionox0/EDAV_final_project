library(plyr)
library(lattice)

setwd('~/Desktop/Columbia/EDAV/Final Project')

# Complaint types (133)
unique(unlist(df$Complaint.Type, use.names = FALSE))

# Rename columns

# Histogram of call types
count(df$Complaint.Type)

# Map by call type

# Avg response time by zipcode
# Avg response time by call type
# Ave response time by call type by zipcode

# % resolved by call type
# % resolved by zip code
# % resolved by zip code by call type (trellis)

# Avg response time by % resolved

# Complaint type counts ordered by freq
counts = count(df$Complaint.Type)
ordered_counts = counts[order(-counts$freq),] 
write.table(ordered_counts, file = "MyData.csv", row.names=FALSE, sep="\t\t\t\t")
cat(capture.output(ordered_counts), file = 'dframe.txt', sep = '\n')


write.csv(unique(unlist(df$Agency.Name, use.names = FALSE)), file = "MyData.csv")


# Subset to just selected complaint types
complaints_subset = df[
    df$Complaint.Type == "Disorderly Youth" ||
    df$Complaint.Type == "Disorderly Youth" ||
    df$Complaint.Type == "Disorderly Youth" ||
    df$Complaint.Type == "Disorderly Youth"
  ]
youth = subset(df, Complaint.Type == "Disorderly Youth")
# Histogram
histogram(Complaint.Type~Incident.Zip | Complaint.Type, data=youth,
       xlab="X", ylab="Y", 
       main="Title")

hist(youth, x = youth$Incident.Zip)

sum(is.na(df$Incident.Zip))


# Create list of unique complaint types
types = unique(df$Complaint.Type)
# Use to subset our complaints
first_half = subset(df, subset = Complaint.Type %in% types[1:50])
# Trellis hist of first 50 complaint types by zip
histogram(Complaint.Type~Incident.Zip | Complaint.Type, data=first_half,
          xlab="X", ylab="Y", 
          main="Title")

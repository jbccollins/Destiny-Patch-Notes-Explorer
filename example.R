require(dplyr)

# DataSet
df = read.csv('data/exampledata.csv', na.strings=c(""), header=TRUE, stringsAsFactors=FALSE)
df[is.na(df)] <- 'None'
df$COLUMN3 <- strsplit(df$COLUMN3, "/")
dput(df)
myVector <- c("me", "stackoverflow")
idx <- which(sapply(df$COLUMN3, function(x) length(intersect(myVector, x))) > 0)
print(idx)
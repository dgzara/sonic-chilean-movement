folder <- paste0(getwd(),"/code/preprocess-data")
setwd(folder)
source(file='../libraries.R')
source(file='../dbConnect.R')

# We get the tweets
q <- paste('SELECT *  
           FROM hashtags',sep="")
tweets <- dbGetQuery(mydb, q)

# We format timestamp
tweets$datetime <- as.POSIXct(tweets$created_at, format="%a %b %d %H:%M:%S %z %Y") 

# We get the hashtag list
hashtags <- sort(unique(unlist(tweets$hashtag, use.names = FALSE)))

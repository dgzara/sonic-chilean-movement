folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
library(xtable)
source(file='../libraries.R')
source(file='../dbConnect.R')
source(file='../accountsList.R')

# Obtengo la red
q <- paste('SELECT hashtag, source, target, type  
           FROM hashtags_network',sep="")
tweets.network <- dbGetQuery(mydb, q)

# Borramos a los que tenemos identificados
tweets.network <- tweets.network[!(tweets.network$target %in% movs),]
tweets.network <- tweets.network[!(tweets.network$target %in% celebrities),]
tweets.network <- tweets.network[!(tweets.network$target %in% media),]
tweets.network <- tweets.network[!(tweets.network$target %in% leaders),]
tweets.network <- tweets.network[!(tweets.network$target %in% orgs),]

# Vemos los usuarios más mencionados
frequency <- table(tweets.network$target)
most_mentioned <- as.data.frame(sort(frequency,decreasing=TRUE)[1:500])

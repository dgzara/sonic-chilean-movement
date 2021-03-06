folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
library(xtable)
source(file='../libraries.R')
source(file='../dbConnect.R')
source(file='../accountsList.R')

# Configuro UTF-8
dbSendQuery(mydb, "SET NAMES utf8")

# Obtenemos los tweets de los hashtags
q <- paste('SELECT hashtag, lower(user_screen_name) as user, text as tweet, created_at as date  
           FROM hashtags',sep="")
tweets <- dbGetQuery(mydb, q)

# Obtengo la red
q <- paste('SELECT hashtag, lower(source) as source, lower(target) as target, type  
           FROM hashtags_network',sep="")
tweets.network <- dbGetQuery(mydb, q)

# Listado de hashtags
hashtags <- sort(unique(unlist(tweets$hashtag, use.names = FALSE)))

# Solo dejamos a los líderes, organizaciones, y common-people
tweets <- tweets[!(tweets$user %in% movs),]
tweets <- tweets[!(tweets$user %in% celebrities),]
tweets <- tweets[!(tweets$user %in% media),]
tweets.network <- tweets.network[!(tweets.network$source %in% movs),]
tweets.network <- tweets.network[!(tweets.network$source %in% celebrities),]
tweets.network <- tweets.network[!(tweets.network$source %in% media),]
tweets.network <- tweets.network[!(tweets.network$target %in% movs),]
tweets.network <- tweets.network[!(tweets.network$target %in% celebrities),]
tweets.network <- tweets.network[!(tweets.network$target %in% media),]

# Reordenamos
tweets$group <- "people"
tweets.network$group.source <- "people"
tweets.network$group.target <- "people"

# Listado de usuarios
users <- as.data.frame(sort(unique(unlist(tweets$user, use.names = FALSE))))

# Clasificamos
tweets[(tweets$user %in% orgs), ]$group <- "orgs"
tweets[(tweets$user %in% leaders), ]$group <- "leaders"
tweets.network[(tweets.network$source %in% orgs), ]$group.source <- "orgs"
tweets.network[(tweets.network$source %in% leaders), ]$group.source <- "leaders"
tweets.network[(tweets.network$target %in% orgs), ]$group.target <- "orgs"
tweets.network[(tweets.network$target %in% leaders), ]$group.target <- "leaders"

grupos <- c("orgs", "leaders")

# Calculamos las métricas
users.tweets <- c()
for(i in grupos)
{
  tweets.grupo <- tweets[(tweets$group %in% i), ]
  tweets.grupo.source <- tweets.network[(tweets.network$group.source %in% i), ]
  tweets.grupo.target <- tweets.network[(tweets.network$group.target %in% i), ]
  users <- sort(unique(unlist(tweets.grupo$user, use.names = FALSE)))
  
  for(j in users)
  {
    row <- c()
    row$group <- i
    row$user <- j
    row$tweets <- nrow(tweets.grupo[(tweets.grupo$user == j),])
    row$retweets <- nrow(tweets.grupo.source[(tweets.grupo.source$source == j & tweets.grupo.source$type == 'retweet'),])
    row$retweeted <- nrow(tweets.grupo.target[(tweets.grupo.target$target == j & tweets.grupo.target$type == 'retweet'),])
    row$reply <- nrow(tweets.grupo.source[(tweets.grupo.source$source == j & tweets.grupo.source$type == 'reply'),])
    row$replied <- nrow(tweets.grupo.target[(tweets.grupo.target$target == j & tweets.grupo.target$type == 'reply'),])
    row$mention <- nrow(tweets.grupo.source[(tweets.grupo.source$source == j & tweets.grupo.source$type == 'mention'),])
    row$mentioned <- nrow(tweets.grupo.target[(tweets.grupo.target$target == j & tweets.grupo.target$type == 'mention'),])
    row$urls <- length(grep("http", tweets.grupo[(tweets.grupo$user == j), ]$tweet))
    users.tweets <- rbind(users.tweets, as.data.frame(row))
  }
}

# Comparamos ambos grupos
row <- c()
row$type <- i
row$n.users <- nrow(users.tweets)
table <- c()

# Agregamos las variables
for(j in 2:ncol(users.tweets))
{
  row2 <- c()
  row2$md <- round(mean(users.tweets[,j]),2)
  row2$sd <- round(sd(users.tweets[,j]),2)
  row2 <- as.data.frame(row2)
  colnames(row2) <- c(paste("md.",colnames(users.tweets)[j], sep=""), paste("sd.",colnames(users.tweets)[j], sep=""))
  row <- cbind(row, row2)
}
table <- rbind(table, row)

rm(row, row2)
xtable(table)

folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
library(xtable)
source(file='../libraries.R')
source(file='../dbConnect.R')
source(file='../accountsList.R')

# Configuro UTF-8
dbSendQuery(mydb, "SET NAMES utf8")

# Obtenemos los tweets de los hashtags
q <- paste('SELECT hashtag, user_screen_name as user, text as tweet, created_at as date  
           FROM hashtags',sep="")
tweets <- dbGetQuery(mydb, q)

# Obtengo la red
q <- paste('SELECT hashtag, source, target, type  
           FROM hashtags_network',sep="")
tweets.network <- dbGetQuery(mydb, q)

# Listado de hashtags
hashtags <- sort(unique(unlist(tweets$hashtag, use.names = FALSE)))

# Ponemos en minúsculas a todos
tweets$user <- tolower(tweets$user)
tweets.network$source <- tolower(tweets.network$source)
tweets.network$target <- tolower(tweets.network$target)

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

grupos <- c("orgs", "leaders", "people")

# Calculamos las métricas
table <- c()
for(i in grupos)
{
  tweets.grupo <- tweets[(tweets$group %in% i), ]
  tweets.grupo.source <- tweets.network[(tweets.network$group.source %in% i), ]
  tweets.grupo.target <- tweets.network[(tweets.network$group.target %in% i), ]
  users <- as.data.frame(sort(unique(unlist(tweets.grupo$user, use.names = FALSE))))
  
  row <- c()
  row$type <- i
  row$n.users <- nrow(users)
  row$n.tweets <- round(nrow(tweets.grupo)/nrow(users), 3)
  row$n.retweets <- round(nrow(tweets.grupo.source[(tweets.grupo.source$type == 'retweet'),])/nrow(users), 3)
  row$n.retweeted <- round(nrow(tweets.grupo.target[(tweets.grupo.target$type == 'retweet'),])/nrow(users), 3)
  row$n.replies <- round(nrow(tweets.grupo.source[(tweets.grupo.source$type == 'reply'),])/nrow(users), 3)
  row$n.mentions <- round(nrow(tweets.grupo.target[(tweets.grupo.target$type == 'mention'),])/nrow(users), 3)
  row <- as.data.frame(row)
  table <- rbind(table, row)
}
rm(row)
xtable(table)

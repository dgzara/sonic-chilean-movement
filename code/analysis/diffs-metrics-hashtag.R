folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
library(xtable)
library(lsr)
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

# Obtengo los mejores hashtags
q <- paste('SELECT hashtag, COUNT(*) 
           FROM hashtags_network
           GROUP BY hashtag
           ORDER BY COUNT(*) DESC',sep="")
hashtags.ranking <- dbGetQuery(mydb, q)

# Dejamos a los tweets de los 20 hasthags
tweets <- tweets[c(tweets$hashtag %in% hashtags.ranking$hashtag[1:20]),]
tweets.network <- tweets.network[c(tweets.network$hashtag %in% hashtags.ranking$hashtag[1:20]),]

# Listado de hashtags
hashtags <- sort(unique(unlist(tweets$hashtag, use.names = FALSE)))

# Solo dejamos a los líderes, organizaciones, y common-people
# tweets <- tweets[!(tweets$user %in% movs),]
# tweets <- tweets[!(tweets$user %in% celebrities),]
# tweets <- tweets[!(tweets$user %in% media),]
# tweets.network <- tweets.network[!(tweets.network$source %in% movs),]
# tweets.network <- tweets.network[!(tweets.network$source %in% celebrities),]
# tweets.network <- tweets.network[!(tweets.network$source %in% media),]
# tweets.network <- tweets.network[!(tweets.network$target %in% movs),]
# tweets.network <- tweets.network[!(tweets.network$target %in% celebrities),]
# tweets.network <- tweets.network[!(tweets.network$target %in% media),]

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

# Filtramos
users <- unique(as.character(users[(users[,1] %in% orgs | users[,1] %in% leaders),]))

# Calculamos las métricas
users.tweets <- c()
for(j in users)
{
  row <- c()
  row$group <- na.omit(unique(tweets[(tweets$user == j), ]$group))
  row$user <- j
  row$tweets <- nrow(tweets[(tweets$user == j),])
  row$retweets <- nrow(tweets.network[(tweets.network$source == j & tweets.network$type == 'retweet'),])
  row$retweeted <- nrow(tweets.network[(tweets.network$target == j & tweets.network$type == 'retweet'),])
  row$reply <- nrow(tweets.network[(tweets.network$source == j & tweets.network$type == 'reply'),])
  row$replied <- nrow(tweets.network[(tweets.network$target == j & tweets.network$type == 'reply'),])
  row$mention <- nrow(tweets.network[(tweets.network$source == j & tweets.network$type == 'mention'),])
  row$mentioned <- nrow(tweets.network[(tweets.network$target == j & tweets.network$type == 'mention'),])
  row$urls <- length(grep("http", tweets[(tweets$user == j), ]$tweet))
  users.tweets <- rbind(users.tweets, as.data.frame(row))
}

# Dejamos a los con mayor indegree
users.tweets.filtered <- users.tweets[(users.tweets$tweets > 40), ]

# Comparamos ambos grupos
table <- c()

# Agregamos las variables
for(j in 3:ncol(users.tweets))
{
  row <- c()
  row$md.leaders <- round(mean(users.tweets.filtered[(users.tweets.filtered$group == "leaders"),j]),2)
  row$sd.leaders <- round(sd(users.tweets.filtered[(users.tweets.filtered$group == "leaders"),j]),2)
  row$md.orgs <- round(mean(users.tweets.filtered[(users.tweets.filtered$group == "orgs"),j]),2)
  row$sd.orgs <- round(sd(users.tweets.filtered[(users.tweets.filtered$group == "orgs"),j]),2)
  row$p.value <- wilcox.test(users.tweets.filtered[(users.tweets.filtered$group == "leaders"),j], users.tweets.filtered[(users.tweets.filtered$group == "orgs"),j])$p.value
  row$cohen.d <- round(cohensD(users.tweets.filtered[(users.tweets.filtered$group == "leaders"),j], users.tweets.filtered[(users.tweets.filtered$group == "orgs"),j], method="unequal"),2)
  row <- as.data.frame(row)
  rownames(row) <- colnames(users.tweets.filtered)[j]
  table <- rbind(table, row)
}
rm(row)

# Generamos la tabla
final <- matrix(0, nrow = nrow(table), ncol = 2)
final <- as.data.frame(final)
rownames(final) <- rownames(table)
colnames(final) <- c("Leaders", "Organizations")

for(i in 1:nrow(final))
{
  final[i,1] <- paste(round(table[i,1],3)," (",round(table[i,2],2), ")", sep="")
  final[i,2] <- paste(round(table[i,3],3)," (",round(table[i,4],2), ")", sep="")
}
xtable(final, align="|l|r|r|")

folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
source(file='../libraries.R')
source(file='../dbConnect.R')
source(file='../accountsList.R')

# Configuro UTF-8
dbSendQuery(mydb, "SET NAMES utf8")

# Obtenemos los tweets de los hashtags
q <- paste('SELECT hashtag, lower(user_screen_name) as user, text as tweet, created_at as datetime  
           FROM hashtags',sep="")
tweets <- dbGetQuery(mydb, q)
tweets$datetime <- as.POSIXct(tweets$datetime, format="%a %b %d %H:%M:%S %z %Y") 
tweets <- tweets[-(tweets$datetime < "2011-01-01"),]

# Listado de hashtags
hashtags <- sort(unique(unlist(tweets$hashtag, use.names = FALSE)))

# Solo dejamos a los líderes, organizaciones, y common-people
#tweets <- tweets[!(tweets$user %in% movs),]
#tweets <- tweets[!(tweets$user %in% celebrities),]
#tweets <- tweets[!(tweets$user %in% media),]

# Calculamos las métricas
table <- c()
for(i in hashtags)
{
  tweets.hashtag <- tweets[(tweets$hashtag %in% i), ]
  users <- as.data.frame(sort(unique(unlist(tweets.hashtag$user, use.names = FALSE))))
  
  row <- c()
  row$hashtag <- i
  row$n.users <- nrow(users)
  row$n.tweets <- nrow(tweets.hashtag)
  row$date.min <- format(min(tweets.hashtag$datetime), format="%m-%d-%Y")
  row$date.max <- format(max(tweets.hashtag$datetime), format="%m-%d-%Y")
  row <- as.data.frame(row)
  table <- rbind(table, row)
}
rm(row, tweets.hashtag)
xtable(table)

# Tabla resumen
table.summary <- c()
table.summary$number <- nrow(table)
table.summary$users.m <- mean(table$n.users)
table.summary$users.sd <- sd(table$n.users)
table.summary$users.min <- min(table$n.users)
table.summary$users.max <- max(table$n.users)
table.summary$tweets.m <- mean(table$n.tweets)
table.summary$tweets.sd <- sd(table$n.tweets)
table.summary$tweets.min <- min(table$n.tweets)
table.summary$tweets.max <- max(table$n.tweets)
table.summary$total <- nrow(tweets)
table.summary <- as.data.frame(table.summary)
xtable(table.summary)

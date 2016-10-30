folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
library(xtable)
library(lsr)
source(file='../libraries.R')
source(file='../dbConnect.R')
source(file='../accountsList.R')

# Configuro UTF-8
dbSendQuery(mydb, "SET NAMES utf8")

# Obtenemos los tweets dirigidos
q <- paste('SELECT lower(source) as source, lower(target) as target, type, hashtag, datetime 
           FROM hashtags_network',sep="")
tweets <- dbGetQuery(mydb, q)
tweets$datetime <- as.POSIXct(tweets$datetime, format="%a %b %d %H:%M:%S %z %Y") 

# Obtengo los mejores hashtags
q <- paste('SELECT hashtag, COUNT(*) 
           FROM hashtags_network
           GROUP BY hashtag
           ORDER BY COUNT(*) DESC',sep="")
hashtags.ranking <- dbGetQuery(mydb, q)

# Listado de hashtags
hashtags <- sort(unique(unlist(tweets$hashtag, use.names = FALSE)))

# Solo dejamos a los líderes, organizaciones, y common-people
# tweets <- tweets[!(tweets$source %in% movs),]
# tweets <- tweets[!(tweets$source %in% celebrities),]
# tweets <- tweets[!(tweets$source %in% media),]
# tweets <- tweets[!(tweets$target %in% movs),]
# tweets <- tweets[!(tweets$target %in% celebrities),]
# tweets <- tweets[!(tweets$target %in% media),]

# Revisamos por año
leaders.metrics <- c()
orgs.metrics <- c()

for(i in hashtags)
{
  tweets.hashtag <- tweets[(tweets$hashtag == i),c("source","target")]
  
  # Generamos el grafo
  network <- graph.data.frame(tweets.hashtag, directed=TRUE)
  network <- simplify(network, remove.multiple = FALSE, remove.loops = TRUE)
  
  # Generamos el grafo
  network <- graph.data.frame(tweets.hashtag, directed=TRUE)
  network <- simplify(network, remove.multiple = FALSE, remove.loops = TRUE)
  
  # Trabajamos con el componente más grande
  #cl <- clusters(network)
  #network <- induced.subgraph(network, which(cl$membership == which.max(cl$csize)))
  
  # Calculamos las metricas por usuarios
  degree <- as.data.frame(degree(network))
  indegree <- as.data.frame(degree(network,mode="in"))
  outdegree <- as.data.frame(degree(network,mode="out"))
  hub <- as.data.frame(hub_score(network)$vector)
  authority <- as.data.frame(authority_score(network)$vector)
  page.rank <- as.data.frame(page.rank(network)$vector)
  betweenness <- betweenness(network, normalized=TRUE)
  closeness <- closeness(network)
  eigenvector <- eigen_centrality(network)$vector
  
  # Calculamos los valores
  username <- rownames(indegree)   
  degree <- degree[,1]
  indegree <-  indegree[,1]
  outdegree <-  outdegree[,1]
  hub <- hub[,1]
  authority <- authority[,1]
  page.rank <- page.rank[,1]
  
  # Armamos la tabla
  users.metrics <- cbind.data.frame(username, i, degree, indegree, outdegree, hub, authority, page.rank, betweenness, closeness, eigenvector)
  users.metrics <- as.data.frame(users.metrics)
  users.metrics[is.na(users.metrics)] <- 0   
  
  # Calculamos los valores descriptivos por grupos
  leaders.metrics <- rbind(leaders.metrics, na.omit(users.metrics[(users.metrics$username %in% leaders),]))
  orgs.metrics <- rbind(orgs.metrics, na.omit(users.metrics[(users.metrics$username %in% orgs),]))
  
  # Cerramos
  rm(users.metrics, tweets.hashtag)
}

# Calculamos para los grupos
leaders.metrics$username <- NULL
orgs.metrics$username <- NULL
leaders.metrics$i <- NULL
orgs.metrics$i <- NULL

final <- c()
for(j in 1:ncol(leaders.metrics))
{
  row <- c()
  row$metric <- colnames(leaders.metrics)[j]
  row$leaders.m <- round(mean(leaders.metrics[,j]),3)
  row$leaders.sd <- round(sd(leaders.metrics[,j]),3)
  row$orgs.m <- round(mean(orgs.metrics[,j]),3)
  row$orgs.sd <- round(sd(orgs.metrics[,j]),3)
  row$p.wilcox <- wilcox.test(leaders.metrics[,j], orgs.metrics[,j])$p.value
  
  # Juntamos las filas
  row <- as.data.frame(row)
  final <- rbind(final, row)
}

# Revisamos a los mejores
top.leaders <- leaders.metrics[order(-leaders.metrics$degree),][1:100,]
top.orgs <- orgs.metrics[order(-orgs.metrics$degree),][1:100,]

# Calculamos
final.100 <- c()
for(j in 1:ncol(leaders.metrics))
{
  row <- c()
  row$metric <- colnames(top.leaders)[j]
  row$leaders.m <- round(mean(top.leaders[,j]),3)
  row$leaders.sd <- round(sd(top.leaders[,j]),3)
  row$orgs.m <- round(mean(top.orgs[,j]),3)
  row$orgs.sd <- round(sd(top.orgs[,j]),3)
  row$p.wilcox <- round(wilcox.test(top.leaders[,j], top.orgs[,j])$p.value,3)
  
  # Juntamos las filas
  row <- as.data.frame(row)
  final.100 <- rbind(final.100, row)
}

# Guardamos la tabla
xtable(final[order(-final$p.wilcox),], digits = 3, align="|l|r|r|r|r|r|r|", auto = TRUE, caption="Metrics for the entire network", label="network_metrics_hashtags")
xtable(final.100, digits = 3, align="|l|r|r|r|r|r|", auto = TRUE, caption="Metrics for the 100 top-ranked users by degree", label="")

plot(density(top.orgs$indegree), log="x")
lines(density(top.leaders$indegree),col="green", log="x")

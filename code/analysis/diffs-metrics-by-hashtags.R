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
final <- c()
users.metrics <- c()

for(i in hashtags.ranking[,"hashtag"][1:10])
{
  tweets.hashtag <- tweets[(tweets$hashtag == i),c("source","target")]
  
  # Generamos el grafo
  network <- graph.data.frame(tweets.hashtag, directed=TRUE)
  network <- simplify(network, remove.multiple = FALSE, remove.loops = TRUE)
  
  # Generamos el grafo
  network <- graph.data.frame(tweets.hashtag, directed=TRUE)
  network <- simplify(network, remove.multiple = FALSE, remove.loops = TRUE)
  
  # Trabajamos con el componente más grande
  cl <- clusters(network)
  network <- induced.subgraph(network, which(cl$membership == which.max(cl$csize)))
  
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
  indegree <-  indegree[,1]
  outdegree <-  outdegree[,1]
  hub <- hub[,1]
  authority <- authority[,1]
  page.rank <- page.rank[,1]
  
  # Armamos la tabla
  users.metrics <- cbind.data.frame(username, indegree, outdegree, hub, authority, page.rank, betweenness, closeness, eigenvector)
  users.metrics <- as.data.frame(users.metrics)
  users.metrics[is.na(users.metrics)] <- 0   
  
  # Calculamos los valores descriptivos por grupos
  leaders.metrics <- na.omit(users.metrics[(users.metrics$username %in% leaders),])
  orgs.metrics <- na.omit(users.metrics[(users.metrics$username %in% orgs),])
  
  # Calculamos para los grupos
  leaders.metrics$username <- NULL
  orgs.metrics$username <- NULL
  
  # Generamos la tabla de medias
  types.m <- c()
  types.m <- rbind(types.m, colMeans(leaders.metrics))
  types.m <- rbind(types.m, colMeans(orgs.metrics))
  types.m <- as.data.frame(types.m)
  rownames(types.m) <- c("leaders", "orgs")
  
  # Generamos la tabla de desviacion 
  types.sd <- c()
  types.sd <- rbind(types.sd, apply(leaders.metrics, 2, sd))
  types.sd <- rbind(types.sd, apply(orgs.metrics, 2, sd))
  types.sd <- as.data.frame(types.sd)
  rownames(types.sd) <- c("leaders", "orgs")
  
  # Consolidamos en una tabla
  data <- c()
  
  for(j in 1:ncol(leaders.metrics))
  {
    row <- c()
    row$hashtag <- i
    row$metric <- colnames(leaders.metrics)[j]
    row$leaders.n <- nrow(leaders.metrics)
    row$leaders.m <- round(types.m[1,j],3)
    row$leaders.sd <- round(types.sd[1,j],3)
    row$orgs.n <- nrow(orgs.metrics)
    row$orgs.m <- round(types.m[2,j],3)
    row$orgs.sd <- round(types.sd[2,j],3)
    row$p.wilcox <- wilcox.test(leaders.metrics[,j], orgs.metrics[,j], exact = FALSE, correct = TRUE)$p.value
    row$cohen.d <- cohensD(leaders.metrics[,j], orgs.metrics[,j], method="unequal")
     
    # Juntamos las filas
    row <- as.data.frame(row)
    final <- rbind(final, row)
  }
  # Cerramos
  rm(users.metrics, types.m, types.sd, row, tweets.hashtag)
}

# Guardamos la tabla
xtable(final, digits = 3, align="|l|r|r|r|r|r|", auto = TRUE)
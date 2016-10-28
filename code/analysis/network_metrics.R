folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
library(xtable)
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

# Solo dejamos a los líderes, organizaciones, y common-people
tweets <- tweets[!(tweets$source %in% movs),]
tweets <- tweets[!(tweets$source %in% celebrities),]
tweets <- tweets[!(tweets$source %in% media),]
tweets <- tweets[!(tweets$target %in% movs),]
tweets <- tweets[!(tweets$target %in% celebrities),]
tweets <- tweets[!(tweets$target %in% media),]

# Obtenemos los usuarios
users.source <- sort(unique(unlist(tweets$source, use.names = FALSE)))
users.target <- sort(unique(unlist(tweets$target, use.names = FALSE)))
users <- c(users.source, users.target)
users <- as.data.frame(sort(unique(unlist(users, use.names = FALSE))))
colnames(users) <- c("username")
rm(users.source, users.target)

# Revisamos por año
networks <- c()
descriptive <- c()
descriptive.acumulado <- c()
types.metrics.m.acumulado <- c()
types.metrics.sd.acumulado <- c()
users.metrics <- c()

for(i in 2011:2013)
{
  tweetsYear <- selectByDate(tweets, year = i)
  tweetsYear <- tweetsYear[,c("source","target")]
  
  # Generamos el grafo
  network <- graph.data.frame(tweetsYear, directed=TRUE)
  
  # Sacamos los valores
  vcount <- vcount(network) # Number of nodes
  ecount <- ecount(network) # Number of edges
  density <- graph.density(network)
  reciprocity <- reciprocity(network, mode="default")
  degree <- degree(network)
  
  # Calculamos las metricas por usuarios
  indegree <- as.data.frame(degree(network,mode="in"))
  outdegree <- as.data.frame(degree(network,mode="out"))
  hub <- as.data.frame(hub_score(network)$vector)
  authority <- as.data.frame(authority_score(network)$vector)
  clusterCoefficient <- as.data.frame(transitivity(network, type="local"))
  page.rank <- as.data.frame(page.rank(network)$vector)
  betweenness <- betweenness(network, normalized=TRUE)
  closeness <- closeness(network)
  
  username <- rownames(indegree)    
  indegree <-  indegree[,1]
  outdegree <-  outdegree[,1]
  hub <- hub[,1]
  authority <- authority[,1]
  clusterCoefficient[is.na(clusterCoefficient),1] <- 0
  clusterCoefficient <- clusterCoefficient[,1]
  page.rank <- page.rank[,1]
  
  # Armamos la tabla
  RatioInNetwork <- cbind.data.frame(username, indegree, outdegree, hub, authority, clusterCoefficient, page.rank, betweenness, closeness)
  RatioInNetwork <- as.data.frame(RatioInNetwork)
  RatioInNetwork[is.na(RatioInNetwork)] <- 0   
  
  users.metrics <- c()
  if(nrow(RatioInNetwork) > 0){
    colnames(RatioInNetwork) <- c("username", "indegree", "outdegree", "hub", "authority", "clusterCoefficient", "page.rank", "betweenness", "closeness")
    users.metrics <- rbind(users.metrics, RatioInNetwork)
  }
  
  # Imprimimos las estadisticas descriptivas
  descriptive <- c()
  descriptive <- rbind(descriptive, c(i, vcount, ecount, density, reciprocity))
  descriptive <- as.data.frame(descriptive)
  colnames(descriptive) <- c("year", "vcount", "ecount", "density", "reciprocity")
  
  # Calculamos los valores descriptivos por grupos
  leaders.metrics <- na.omit(users.metrics[(users.metrics$username %in% leaders),])
  orgs.metrics <- na.omit(users.metrics[(users.metrics$username %in% orgs),])
  people.metrics <- na.omit(users.metrics[!(users.metrics$username %in% leaders | users.metrics$username %in% orgs),])
  
  # Calculamos para los grupos
  leaders.metrics$username <- NULL
  orgs.metrics$username <- NULL
  people.metrics$username <- NULL
  
  # Generamos la tabla de medias
  types.m <- c()
  types.m <- rbind(types.m, colMeans(leaders.metrics))
  types.m <- rbind(types.m, colMeans(orgs.metrics))
  types.m <- rbind(types.m, colMeans(people.metrics))
  types.m <- as.data.frame(types.m)
  rownames(types.m) <- c("leaders", "orgs", "people")
  
  # Generamos la tabla de desviacion 
  types.sd <- c()
  types.sd <- rbind(types.sd, apply(leaders.metrics, 2, sd))
  types.sd <- rbind(types.sd, apply(orgs.metrics, 2, sd))
  types.sd <- rbind(types.sd, apply(people.metrics, 2, sd))
  types.sd <- as.data.frame(types.sd)
  rownames(types.sd) <- c("leaders", "orgs", "people")
  
  # Asignamos las variables
  assign(paste("users.metrics.", i, sep = ""), users.metrics)
  assign(paste("descriptive", i, sep = ""), descriptive)
  assign(paste("types.m.", i, sep = ""), types.m)
  assign(paste("types.sd.", i, sep = ""), types.sd)
  
  # Cerramos
  descriptive.acumulado <- rbind(descriptive.acumulado, descriptive)
  types.metrics.m.acumulado <- rbind(types.metrics.m.acumulado, types.m)
  types.metrics.sd.acumulado <- rbind(types.metrics.sd.acumulado, types.sd)
  rm(descriptive, users.metrics, types.m, types.sd)
}

# Guardamos la tabla
final <- matrix(0, nrow = ncol(types.metrics.m.acumulado)*3, ncol = 5)
final <- as.data.frame(final)
colnames(final) <- c("group", "2011", "2012", "2013")

for(i in 1:nrow(types.metrics.m.acumulado))
{
  for(j in 1:ncol(types.metrics.m.acumulado))
  {
    new_i <- 1 + (i-1)%%3 + (j-1)*3
    new_j <- 2 + trunc((i-1)/3)
    
    if(new_j < 3){
      final[new_i, 1] <- rownames(types.metrics.m.acumulado[i,])
    }
    
    final[new_i,new_j] <- paste("$ ", round(types.metrics.m.acumulado[i,j],3)," pm{",round(types.metrics.sd.acumulado[i,j],3), "} $", sep="")
  }
}

# Damos el formato final a la tabla
final$metric <- colnames(types.metrics.m.acumulado)[1]
final[4:6, ]$metric <- colnames(types.metrics.m.acumulado)[2]
final[7:9, ]$metric <- colnames(types.metrics.m.acumulado)[3]
final[10:12, ]$metric <- colnames(types.metrics.m.acumulado)[4]
final[13:15, ]$metric <- colnames(types.metrics.m.acumulado)[5]
final[16:18, ]$metric <- colnames(types.metrics.m.acumulado)[6]
final[19:21, ]$metric <- colnames(types.metrics.m.acumulado)[7]
final[22:24, ]$metric <- colnames(types.metrics.m.acumulado)[8]
final <- final[, c("metric", "group", "2011", "2012", "2013")]
xtable(final)

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

# Revisamos por año
table <- c()
table.100 <- c()

# Analizamos por hashtags
for(k in c("retweet", "reply", "mention"))
{
  leaders.metrics <- c()
  orgs.metrics <- c()
  
  for(i in hashtags.ranking[1:20,]$hashtag)
  {
    # Generamos el grafo
    network <- graph.data.frame(tweets[(tweets$hashtag == i & tweets$type == k),c("source","target")], directed=TRUE)
    network <- simplify(network, remove.multiple = FALSE, remove.loops = TRUE)
    
    # Trabajamos con el componente más grande
    #cl <- cluster_walktrap(network)
    #network <- induced.subgraph(network, which(cl$membership == which.max(cl$csize)))
    
    # Creamos los k-cores
    #coreness <- graph.coreness(network)
    #network <- induced.subgraph(network, which(coreness > 3))
    
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
  
  if(is.null(table)){ 
    table <- final
  }else{
    final$metric <- NULL
    table <- cbind(table, final)
  }
  
  # Revisamos a los mejores
  top.leaders <- leaders.metrics[order(-leaders.metrics$degree),][1:100,]
  top.orgs <- orgs.metrics[order(-orgs.metrics$degree),][1:100,]
  
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
  
  if(is.null(table.100)){ 
    table.100 <- final.100
  }else{
    final.100$metric <- NULL
    table.100 <- cbind(table.100, final.100)
  }
  
  rm(final, final.100, leaders.metrics, orgs.metrics)
}

final <- matrix(0, nrow = nrow(table), ncol = 6)
final <- as.data.frame(final)
rownames(final) <- table$metric
colnames(final) <- c("RT leaders", "RT orgs", "Reply leaders", "Reply orgs", "Mention leaders", "Mention orgs")
indexs <- c(2,4,7,9,12,14)

for(i in 1:nrow(final))
{
  for(j in 1:ncol(final))
  {
    if(i%%2 == 0){
      final[i,j] <- paste(round(table[i,indexs[j]],3)," (",round(table[i,1+indexs[j]],2), ")", sep="")
    } else {
      final[i,j] <- paste(round(table[i,indexs[j]],3)," (",round(table[i,1+indexs[j]],2), ")", sep="")
    }
  }
}
xtable(final)

folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
source(file='../libraries.R')
source(file='../dbConnect.R')
source(file='../accountsList.R')
library(xtable)
library(lsr)
library(sna)

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
users.metrics <- c()

# Analizamos por hashtags
for(k in c("retweet", "reply", "mention"))
{
  leaders.metrics <- c()
  orgs.metrics <- c()
  
  for(i in hashtags.ranking[1:20,]$hashtag)
  {
    # Generamos el grafo
    g <- graph.data.frame(tweets[(tweets$hashtag == i & tweets$type == k),c("source","target")], directed=TRUE)
    g <- simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
    
    # Obtenemos la matriz de adjcaencia
    a <- as.matrix(get.adjacency(g))
    V(g)$type <- "people"
    V(g)[V(g)$name %in% leaders]$type <- "leader"
    V(g)[V(g)$name %in% orgs]$type <- "organization"
    
    # Calculamos las metricas por usuarios
    brokerage <- brokerage(a, V(g)$type)$raw.nli
    #brokerage.exp.nli <- brokerage(a, V(g)$type)$exp.nli
    #brokerage.z.nli <- brokerage(a, V(g)$type)$z.nli
    
    # Calculamos los valores
    username <- rownames(brokerage) 
    
    # Armamos la tabla
    users.metrics <- cbind.data.frame(username, i, brokerage, stringsAsFactors=FALSE)
    
    # Calculamos los valores descriptivos por grupos
    leaders.metrics <- rbind(leaders.metrics, users.metrics[(users.metrics$username %in% leaders),])
    orgs.metrics <- rbind(orgs.metrics, users.metrics[(users.metrics$username %in% orgs),])
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
    row$leaders.m <- round(mean(leaders.metrics[,j], na.rm=TRUE),3)
    row$leaders.sd <- round(sd(leaders.metrics[,j], na.rm=TRUE),3)
    row$orgs.m <- round(mean(orgs.metrics[,j], na.rm=TRUE),3)
    row$orgs.sd <- round(sd(orgs.metrics[,j], na.rm=TRUE),3)
    row$p.wilcox <- wilcox.test(leaders.metrics[,j], orgs.metrics[,j], na.rm=TRUE)$p.value
    
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
  
  rm(final, leaders.metrics, orgs.metrics) 
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

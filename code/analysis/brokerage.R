folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
source(file='../libraries.R')
source(file='../dbConnect.R')
source(file='../accountsList.R')
library(xtable)
library(lsr)

# Configuro UTF-8
dbSendQuery(mydb, "SET NAMES utf8")

# Obtenemos los tweets dirigidos
q <- paste('SELECT lower(source) as source, lower(target) as target, type, hashtag, datetime 
           FROM hashtags_network',sep="")
tweets <- dbGetQuery(mydb, q)
tweets$datetime <- as.POSIXct(tweets$datetime, format="%a %b %d %H:%M:%S %z %Y") 

# Solo dejamos a los líderes, organizaciones, y common-people
# tweets <- tweets[!(tweets$source %in% movs),]
# tweets <- tweets[!(tweets$source %in% celebrities),]
# tweets <- tweets[!(tweets$source %in% media),]
# tweets <- tweets[!(tweets$target %in% movs),]
# tweets <- tweets[!(tweets$target %in% celebrities),]
# tweets <- tweets[!(tweets$target %in% media),]

# Listado de hashtags
hashtags <- sort(unique(unlist(tweets$hashtag, use.names = FALSE)))

# Revisamos por año
final <- c()
users.metrics <- c()

for(i in hashtags)
{
  tweets.hashtag <- tweets[(tweets$hashtag == i),c("source","target")]
  
  # Generamos el grafo
  network <- graph.data.frame(tweets.hashtag, directed=TRUE)
  network <- simplify(network, remove.multiple = FALSE, remove.loops = TRUE)
  
  # Obtenemos la matriz de adjcaencia
  a <- as.matrix(get.adjacency(network))
  V(network)$type <- "people"
  V(network)[V(network)$name %in% leaders]$type <- "leader"
  V(network)[V(network)$name %in% orgs]$type <- "organization"
  
  # Calculamos las metricas por usuarios
  brokerage <- brokerage(a, V(network)$type)$raw.nli
  
  # Calculamos los valores
  username <- rownames(indegree) 
  brokerage <- brokerage[,1]
  
  # Armamos la tabla
  users.metrics <- cbind.data.frame(username, brokerage)
  users.metrics <- as.data.frame(users.metrics)
  users.metrics[is.na(users.metrics)] <- 0   
  
  # Calculamos los valores descriptivos por grupos
  leaders.metrics <- na.omit(users.metrics[(users.metrics$username %in% leaders),])
  orgs.metrics <- na.omit(users.metrics[(users.metrics$username %in% orgs),])
  
  # Calculamos para los grupos
  leaders.metrics$username <- NULL
  orgs.metrics$username <- NULL
  
  # Consolidamos en una tabla
  data <- c()
  for(j in 1:ncol(leaders.metrics))
  {
    row <- c()
    row$leaders.m <- types.m[1,j]
    row$leaders.sd <- types.sd[1,j]
    row$orgs.m <- types.m[2,j]
    row$orgs.sd <- types.sd[2,j]
    row$p.wilcox <- wilcox.test(leaders.metrics[,j], orgs.metrics[,j], exact = FALSE, correct = TRUE)$p.value
    row$cohen.d <- cohensD(leaders.metrics[,j], orgs.metrics[,j], method="unequal")
    
    # Normalizamos la daa
    new.data <- as.data.frame(rbind(cbind(leaders.metrics[,j], "leader"), cbind(orgs.metrics[,j], "org")))
    new.data[,1] <- as.numeric(new.data[,1])
    new.data[,1] <- (new.data[,1] - min(new.data[,1])) / (max(new.data[,1]) - min(new.data[,1]))
    colnames(new.data) <- c("value", "group")
    
    # Test t-student
    tryCatch({
      row$p.welch <- t.test(leaders.metrics[,j], orgs.metrics[,j],  alternative="two.sided", var.equal=FALSE)$p.value
      # If n < 60, do not apply this test.
    }, error = function(err) {
      row$p.welch <- 0
    })
    
    # Juntamos las filas
    row <- as.data.frame(row)
    rownames(row) <- colnames(leaders.metrics)[j]
    final <- rbind(final, row)
    
    # Juntamos
    data <- rbind(data, cbind(new.data, colnames(leaders.metrics)[j]))
  }
  
  # Cerramos
  rm(users.metrics, types.m, types.sd, row, tweetsYear, data, new.data)
}

# Guardamos la tabla
xtable(final, digits = 3, align="|l|r|r|r|r|r|", display=c("s", "e", "e", "e", "e", "f"), auto = TRUE)
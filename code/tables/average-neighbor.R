folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
source(file='../libraries.R')
source(file='../dbConnect.R')
source(file='../accountsList.R')
library(MASS)
library(lsr)

# Obtenemos los tweets dirigidos
q <- paste('SELECT source, target, type, hashtag, datetime  
           FROM hashtags_network',sep="")
tweets <- dbGetQuery(mydb, q)
tweets$tweetid <- NULL
tweets$datetime <- as.POSIXct(tweets$datetime, format="%a %b %d %H:%M:%S %z %Y") 

# Revisamos por año
table <- c()

# Realizamos el for
for(i in 2011:2013)
{
  tweetsYear <- selectByDate(tweets, year = i)
  tweetsYear <- tweetsYear[,c("source","target")]
  
  # Generamos el grafo
  network <- graph.data.frame(tweetsYear, directed=TRUE)
  
  # Asignamos a todos como people
  V(network)$group <- "People"
  V(network)$size <- 1
  V(network)$alpha <- 0.8
  
  # Asignamos los grupos
  V(network)[V(network)$name %in% leaders]$group <- "Student Leaders"
  V(network)[V(network)$name %in% orgs]$group <- "Student Organizations"
  
  # Average neighbor degree versus vertex degree (log–log scale)
  network.simplified <- simplify(network)
  a.nn.deg.network <- graph.knn(network.simplified,V(network.simplified))$knn 
  d.network.simplified <- degree(network.simplified)
  
  # Reviso los vecinos por grupo
  neighbors.leaders <- a.nn.deg.network[names(a.nn.deg.network) %in% leaders]
  neighbors.orgs <- a.nn.deg.network[names(a.nn.deg.network) %in% orgs]
  
  # Leaders
  row <- c()
  row$m.leaders <- mean(neighbors.leaders)
  row$sd.leaders <- sd(neighbors.leaders)
  row$m.orgs <- mean(neighbors.orgs)
  row$sd.orgs <- sd(neighbors.orgs)
  row$cohen.d <- cohensD(neighbors.leaders, neighbors.orgs, method="unequal")
  row <- as.data.frame(row)
  rownames(row) <- i
  table <- rbind(table, row)
}

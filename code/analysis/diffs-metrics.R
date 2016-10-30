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

# Solo dejamos a los líderes, organizaciones, y common-people
# tweets <- tweets[!(tweets$source %in% movs),]
# tweets <- tweets[!(tweets$source %in% celebrities),]
# tweets <- tweets[!(tweets$source %in% media),]
# tweets <- tweets[!(tweets$target %in% movs),]
# tweets <- tweets[!(tweets$target %in% celebrities),]
# tweets <- tweets[!(tweets$target %in% media),]

# Obtenemos los usuarios
users.source <- sort(unique(unlist(tweets$source, use.names = FALSE)))
users.target <- sort(unique(unlist(tweets$target, use.names = FALSE)))
users <- c(users.source, users.target)
users <- as.data.frame(sort(unique(unlist(users, use.names = FALSE))))
colnames(users) <- c("username")
rm(users.source, users.target)

# Revisamos por año
final <- c()
users.metrics <- c()

for(i in 2011:2013)
{
  tweetsYear <- selectByDate(tweets, year = i)
  tweetsYear <- tweetsYear[(tweetsYear$type == "retweet"),]
  tweetsYear <- tweetsYear[,c("source","target")]
  
  # Generamos el grafo
  network <- graph.data.frame(tweetsYear, directed=TRUE)
  network <- simplify(network, remove.multiple = FALSE, remove.loops = TRUE)
  
  # Generamos el grafo
  network <- graph.data.frame(tweetsYear, directed=TRUE)
  network <- simplify(network, remove.multiple = FALSE, remove.loops = TRUE)
  
  # Trabajamos con el componente más grande
  cl <- clusters(network)
  network <- induced.subgraph(network, which(cl$membership == which.max(cl$csize)))
  
  # Reducimos a k-core
  coreness <- graph.coreness(network)
  network <- induced.subgraph(network, which(coreness > 1))
  
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
  data.year <- c()
  
  for(j in 1:ncol(leaders.metrics))
  {
    row <- c()
    row$leaders.m <- types.m[1,j]
    row$leaders.sd <- types.sd[1,j]
    row$orgs.m <- types.m[2,j]
    row$orgs.sd <- types.sd[2,j]
    #row$p.wilcox <- wilcox.test(leaders.metrics[,j], orgs.metrics[,j], exact = FALSE, correct = TRUE)$p.value
    row$cohen.d <- cohensD(leaders.metrics[,j], orgs.metrics[,j], method="unequal")
      
    # Normalizamos la daa
    new.data <- as.data.frame(rbind(cbind(leaders.metrics[,j], "leader"), cbind(orgs.metrics[,j], "org")))
    new.data[,1] <- as.numeric(new.data[,1])
    new.data[,1] <- (new.data[,1] - min(new.data[,1])) / (max(new.data[,1]) - min(new.data[,1]))
    colnames(new.data) <- c("value", "group")
    
    # Test t-student
    #tryCatch({
      #row$p.welch <- t.test(leaders.metrics[,j], orgs.metrics[,j],  alternative="two.sided", var.equal=FALSE)$p.value
      # If n < 60, do not apply this test.
    #}, error = function(err) {
     # row$p.welch <- 0
    #})
    
    # Juntamos las filas
    row <- as.data.frame(row)
    rownames(row) <- colnames(leaders.metrics)[j]
    final <- rbind(final, row)
    
    # Juntamos
    data <- rbind(data, cbind(new.data, colnames(leaders.metrics)[j]))
  }
  
  # Creamos el violet plot
  colnames(data) <- c("value", "group", "metric")
  data$value <- as.numeric(data$value)
  p <- ggplot(data, aes(factor(group), value, fill=factor(group))) +
      geom_violin() + geom_boxplot(width=0.1, fill="white") + 
      ggtitle(i) + 
      xlab("Groups") +
      facet_wrap(~metric, ncol = 4) +
      theme(legend.position="bottom")
  assign(paste("plot.", i, sep = ""), p)
  
  # Cerramos
  rm(users.metrics, types.m, types.sd, row, tweetsYear, data, new.data)
}

# Guardamos la tabla
xtable(final, digits = 3, align="|l|r|r|r|r|r|", auto = TRUE)

# Creamos el grafico de lineas
df <- data.frame(
  trt = factor(c(2011, 2011, 2012, 2012, 2013, 2013)),
  resp = c(final["outdegree",1], final["outdegree",3], final["outdegree1",1], final["outdegree1",3], final["outdegree2",1], final["outdegree2",3]),
  group = factor(c("leaders", "orgs", "leaders", "orgs", "leaders", "orgs")),
  se = c(final["outdegree",2], final["outdegree",4], final["outdegree1",2], final["outdegree1",4], final["outdegree2",2], final["outdegree2",4])
)
limits <- aes(ymax = resp + se, ymin=resp - se)
p <- ggplot(df, aes(colour=group, y=resp, x=trt))
p + geom_line(aes(group=group)) 
  #+ geom_errorbar(limits, width=0.2)


# Creamos la leyenda
g <- ggplotGrob(plot.2011 + theme(legend.position="bottom", legend.text=element_text(size = 12)))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)

#Plot final
pdf("../../plots/network_diff.pdf",8,12)
plot.final <- grid.arrange(arrangeGrob(plot.2011 + theme(legend.position="none"),
                                       plot.2012 + theme(legend.position="none"),
                                       plot.2013 + theme(legend.position="none"),
                                       nrow=3),
                           legend, nrow=2,heights=unit.c(unit(1, "npc") - lheight, lheight)) 
dev.off()
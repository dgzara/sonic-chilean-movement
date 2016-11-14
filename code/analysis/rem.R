folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
library(statnet)
library(relevent)
library(informR)
library(stats)
source(file='../libraries.R')
source(file='../dbConnect.R')
source(file='../accountsList.R')

as.sociomatrix.eventlist <- function (eventlist, n) 
{
  g <- matrix(0, n, n)
  if (NROW(eventlist) > 0) {
    tabmat <- table(eventlist[, -1, drop = FALSE])
    g[as.numeric(dimnames(tabmat)[[1]]), as.numeric(dimnames(tabmat)[[2]])] <- tabmat
  }
  g
}

format_data <- function(data)
{
  # Obtenemos los usuarios
  users <- data.frame(sort(unique(c(unique(sort(data$FromId)), unique(sort(data$ToId))))), stringsAsFactors=FALSE)
  users <- cbind(as.numeric(rownames(users)), users)
  colnames(users) <- c("id", "username")
  
  # Identiticamos el tipo de usuario
  users$is_leader <- (users$username %in% leaders)
  users$is_org <- (users$username %in% orgs)
  users$type <- 0
  if(nrow(users[(users$is_leader == 1),])>0){users[(users$is_leader == 1),]$type <- 1}
  if(nrow(users[(users$is_org == 1),])>0){users[(users$is_org == 1),]$type <- 2}
  
  # Reemplazamos los nombres por ids
  data$FromId <- match(data$FromId,users$username)
  data$ToId <- match(data$ToId,users$username)
  
  # Ordenamos la matriz por la fecha
  data$StartTime <- as.numeric(data$StartTime)
  data$StartTime <- data$StartTime - min(data$StartTime)
  data <- data[order(data[,1]),]
  rownames(data) <- NULL
  
  # Si vemos tiempos iguales, le agregamos una unidad. 
  temp <- 0
  for(i in 2:nrow(data))
  {
    if(data[i,]$StartTime == temp)
    {
      repetidos <- data[(data$StartTime == temp),]
      for(j in 1:nrow(repetidos)-1)
      {
        data[i +(j-1),]$StartTime <- temp + j/10
      }
      rm(repetidos)
    } else {
      temp <- data[i,]$StartTime
    }
  }
  
  # Ponemos la fila final
  data <- rbind(data, c(max(data$StartTime)+1,NA,NA, NA, NA))
  
  return(list(data, users))
}

# Obtenemos los tweets dirigidos
q <- paste('SELECT lower(source) as source, lower(target) as target, type, hashtag, datetime  
           FROM hashtags_network',sep="")
tweets <- dbGetQuery(mydb, q)
tweets$tweetid <- NULL
tweets$datetime <- as.POSIXct(tweets$datetime, format="%a %b %d %H:%M:%S %z %Y") 

# Solo dejamos a los líderes, organizaciones, y common-people
tweets <- tweets[!(tweets$source %in% movs),]
tweets <- tweets[!(tweets$source %in% celebrities),]
tweets <- tweets[!(tweets$source %in% media),]
tweets <- tweets[!(tweets$target %in% movs),]
tweets <- tweets[!(tweets$target %in% celebrities),]
tweets <- tweets[!(tweets$target %in% media),]

# Seleccionamos un hashtag de ejemplo, en la cadena de retweets
data <- tweets[(tweets$type != "retweet"),]
data[,c("type", "hashtag")] <- NULL
data <- data[, c("datetime", "source", "target")]

# Creamos la red
g <- graph.data.frame(data[,c("source","target")], directed=TRUE)
E(g)$timestamp <- data$datetime

# Simplificamos
g <- simplify(g, remove.multiple = FALSE, remove.loops = TRUE)

# Seteamos los tipos
V(g)$type <- 0
V(g)[V(g)$name %in% leaders]$type <- 1
V(g)[V(g)$name %in% orgs]$type <- 2

# Obtengo las mejores 10 organizaciones y 10 líderes
top.leaders <- rev(sort(degree(g, mode="in")[V(g)$name %in%leaders]))[1:5]
top.orgs <- rev(sort(degree(g, mode="in")[V(g)$name %in%orgs]))[1:5]

# Generamos de nuevo la red
g <- induced.subgraph(g, which(V(g)$type==0 | V(g)$name %in% names(top.leaders) | V(g)$name %in% names(top.orgs)))

# Creamos los k-cores
coreness <- graph.coreness(g)
g <- induced.subgraph(g, which(coreness > 5))

# Generamos los clusters
cluster <- cluster_walktrap(g)
cluster.affiliation <- membership(cluster)   # affiliation list

# Obtengo los clústers de los líderes y orgs
com.leaders <- cluster.affiliation[(names(cluster.affiliation) %in% leaders)]
com.orgs <- cluster.affiliation[(names(cluster.affiliation) %in% orgs)]

# Genero las redes donde hay líderes y organizaciones
g.leaders <- induced.subgraph(g, which(cluster$membership %in% com.leaders & V(g)$type != 2))
g.orgs <- induced.subgraph(g, which(cluster$membership %in% com.orgs & V(g)$type != 1))

# Visualizamos
plot(g.leaders, vertex.color=V(g.leaders)$type)
plot(g.orgs, vertex.color=V(g.orgs)$type)

# Generamos la lista de edges
data.leaders <- data.frame(E(g.leaders)$timestamp, get.edgelist(g.leaders)[,1], get.edgelist(g.leaders)[,2], stringsAsFactors=FALSE)
data.orgs <- data.frame(E(g.orgs)$timestamp, get.edgelist(g.orgs)[,1], get.edgelist(g.orgs)[,2], stringsAsFactors=FALSE)
colnames(data.leaders) <- colnames(data.orgs) <- c("StartTime", "FromId", "ToId") 

# Formateamos la data
data.leaders <- format_data(data.leaders)
data.orgs <- format_data(data.orgs)

################################
####
## LIDERES
####
################################

# Creamos la primera red
network.leaders <- as.sociomatrix.eventlist(data.leaders[[1]], nrow(data.leaders[[2]]))
gplot(network.leaders, vertex.col=4-2*data.leaders[[2]]$type, vertex.cex=2, edge.lwd=network.leaders^0.75)

# 1st model: intercept model, containing only a vector of 1s (ClassIntercept) 
intercept.leaders <- rep(1, nrow(data.leaders[[2]])) 
classfit.leaders.1 <- rem.dyad(data.leaders[[1]], n=nrow(data.leaders[[2]]), effects=c("CovSnd"), covar=list(CovSnd=intercept.leaders), ordinal=FALSE, hessian=TRUE)
summary(classfit.leaders.1)

# we incorporate the appropriate P-shift effects into our cumulative model:
classfit.leaders.2 <- rem.dyad(data.leaders[[1]], n=nrow(data.leaders[[2]]), effects=c("CovSnd","CovRec", "RRecSnd","PSAB-BA","PSAB-AY","PSAB-BY"),
                      covar=list(CovSnd=cbind(intercept.leaders, data.leaders[[2]]$is_leader),
                                 CovRec=cbind(data.leaders[[2]]$is_leader)),ordinal=FALSE,hessian=TRUE)
summary(classfit.leaders.2)
classfit.leaders.1$BIC-classfit.leaders.2$BIC #Enhanced model is again preferred

# we incorporate the appropriate P-shift effects into our cumulative model:
classfit.leaders.3 <- rem.dyad(data.leaders[[1]], n=nrow(data.leaders[[2]]), effects=c("CovSnd", "CovRec", "RRecSnd", "RSndSnd", "PSAB-BA", "PSAB-AY", "PSAB-BY", "PSA0-XA", "PSAB-XA", "PSAB-XB"),
                            covar=list(CovSnd=cbind(intercept.leaders, data.leaders[[2]]$is_leader),
                                       CovRec=cbind(data.leaders[[2]]$is_leader)),ordinal=FALSE,hessian=TRUE)
summary(classfit.leaders.3)
classfit.leaders.2$BIC-classfit.leaders.3$BIC 

exp(classfit.leaders.2$coef[c("CovSnd.2")])
exp(classfit.leaders.2$coef[c("PSAB-BA")]) # Reciprocity
exp(classfit.leaders.2$coef[c("PSAB-AY")]) # 
exp(classfit.leaders.2$coef[c("PSAB-BY")]) # Transivity

# Checking the model
hist(classfit.leaders.2$residuals)
mean(apply(classfit.leaders.2$predicted.match,1,all))
mean(apply(classfit.leaders.2$predicted.match,1,any))

################################
####
## ORGS
####
################################

# Creamos la segunda red
network.orgs <- as.sociomatrix.eventlist(data.orgs[[1]], nrow(data.orgs[[2]]))
gplot(network.orgs, vertex.col=4-2*data.orgs[[2]]$type, vertex.cex=2, edge.lwd=network.orgs^0.75)

# 1st model: intercept model, containing only a vector of 1s (ClassIntercept) 
intercept.orgs <- rep(1, nrow(data.orgs[[2]])) 
classfit.orgs.1 <- rem.dyad(data.orgs[[1]], n=nrow(data.orgs[[2]]), effects=c("CovSnd"), covar=list(CovSnd=intercept.orgs), ordinal=FALSE, hessian=TRUE)
summary(classfit.orgs.1)

# we incorporate the appropriate P-shift effects into our cumulative model:
classfit.orgs.2 <- rem.dyad(data.orgs[[1]], n=nrow(data.orgs[[2]]), effects=c("CovSnd","CovRec","RRecSnd","RSndSnd","PSAB-BA","PSAB-AY","PSAB-BY"),
                               covar=list(CovSnd=cbind(intercept.orgs, data.orgs[[2]]$is_org),
                                          CovRec=cbind(data.orgs[[2]]$is_org)),ordinal=FALSE,hessian=TRUE)
summary(classfit.orgs.2)
classfit.orgs.1$BIC-classfit.orgs.2$BIC 

# we incorporate the appropriate P-shift effects into our cumulative model:
classfit.orgs.3 <- rem.dyad(data.orgs[[1]], n=nrow(data.orgs[[2]]), effects=c("CovSnd", "CovRec", "RRecSnd", "RSndSnd", "PSAB-BA", "PSAB-AY", "PSAB-BY", "PSA0-XA", "PSAB-XA", "PSAB-XB"),
                            covar=list(CovSnd=cbind(intercept.orgs, data.orgs[[2]]$is_org),
                                       CovRec=cbind(data.orgs[[2]]$is_org)),ordinal=FALSE,hessian=TRUE)
summary(classfit.orgs.3)
classfit.orgs.2$BIC-classfit.orgs.3$BIC 

exp(classfit.orgs.2$coef[c("CovSnd.2")]) 
exp(classfit.orgs.2$coef[c("PSAB-BA")]) # Reciprocity
exp(classfit.orgs.2$coef[c("PSAB-AY")]) # 
exp(classfit.orgs.2$coef[c("PSAB-BY")]) # Transivity

#Organization responding to another student
exp(sum(classfit.orgs.3$coef[c("CovSnd.1","CovSnd.2","PSAB-AY")]))

#Get the surprising events, and display as a network
surprising<-as.sociomatrix.eventlist(data.orgs[[1]][classfit.orgs.3$observed.rank>19,], nrow(data.orgs[[2]]))
gplot(surprising, vertex.col=4-2*data.orgs[[2]]$type, vertex.cex=2)

#Show how the "surprising" events fit into the broader communication structure 
edgecol<-matrix(rgb(surprising/(network.orgs+0.01),0,0),nrow(data.orgs[[2]]),nrow(data.orgs[[2]])) #Color me surprised 
gplot(network.orgs,edge.col=edgecol,edge.lwd=network.orgs^0.75,vertex.col=4-2*data.orgs[[2]]$type,vertex.cex=2)

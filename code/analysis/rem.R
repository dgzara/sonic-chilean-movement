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
data <- tweets[(tweets$hashtag == "4deagosto" & tweets$type == "retweet"),]
data[,c("type", "hashtag")] <- NULL
data <- data[, c("datetime", "source", "target")]

# Buscamos los usuarios que participan
users <- sort(unique(c(unique(sort(data$source)), unique(sort(data$target)))))
users <- as.data.frame(users)
colnames(users) <- c("username")

# Seleccionamos si son lideres o no
users$type <- 0
users[(users$username %in% leaders),]$type <- 1
users[(users$username %in% orgs),]$type <- 2

# Eliminamos las interacciones persona-persona
# data <- merge(data, users[,c("username","type")], by.x = "source", by.y = "username")
# data <- merge(data, users[,c("username","type")], by.x = "target", by.y = "username")
# data <- data[(data$type.x > 0 | data$type.y > 0 ),]
# data <- data[,c("datetime", "source", "target")]

# Removemos a los usuarios de las comunidades más isoladas
set.seed(245)
graph <- graph.data.frame(data[,2:3], directed = TRUE)
wc <- cluster_walktrap(graph)
V(graph)$community <- membership(wc)
vertex.isolated <- V(graph)[community > 2]$name
data <- data[!(data$source %in% vertex.isolated),]
data <- data[!(data$target %in% vertex.isolated),]

# Actualizamos la lista
users <- as.data.frame(sort(unique(c(unique(sort(data$source)), unique(sort(data$target))))))
users <- cbind(rownames(users), users)
colnames(users) <- c("id", "username")
users$is_leader <- (users$username %in% leaders)
users$is_org <- (users$username %in% orgs)
users$type <- 0
users[(users$is_leader == 1),]$type <- 1
users[(users$is_org == 1),]$type <- 2

# Reemplazamos los nombres por ids
data$source <- match(data$source,users$username)
data$target <- match(data$target,users$username)

# Ordenamos la matriz por la fecha
data$time <- as.numeric(data$datetime)
data$time <- data$time - min(data$time)
data <- data[order(data[,4]),]
data <- data[,c("time", "source", "target")]
colnames(data) <- c("StartTime", "FromId", "ToId")
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
data <- rbind(data, c(max(data$StartTime)+1,NA,NA))

# We build the network
network <- as.sociomatrix.eventlist(data, nrow(users))
gplot(network, vertex.col=4-2*users$type, vertex.cex=2, edge.lwd=network^0.75)

# 1st model: intercept model, containing only a vector of 1s (ClassIntercept) 
intercept <- rep(1, nrow(users)) 
classfit1 <- rem.dyad(data, n=nrow(users), effects=c("CovSnd"), covar=list(CovSnd=intercept), ordinal=FALSE, hessian=TRUE)
summary(classfit1)

# Results
(classfit1$m-1)/max(data[,1])                     #Events per minute (on average) 
nrow(users)*(nrow(users)-1)*exp(classfit1$coef)   #Predicted events per minute

# 2nd model: Adding the effects of Leadership/Orgs
classfit2 <- rem.dyad(data,n=nrow(users),effects=c("CovSnd","CovRec"), 
                    covar=list(CovSnd=cbind(intercept,users$is_leader,users$is_org), 
                               CovRec=cbind(users$is_leader,users$is_org)), ordinal=FALSE, hessian=TRUE)
summary(classfit2)
classfit1$BIC-classfit2$BIC

# 3rd model: Probando como atributo
classfit3 <- rem.dyad(data,n=nrow(users),effects=c("CovSnd","CovRec"), 
                      covar=list(CovSnd=cbind(intercept,users$type), 
                                 CovRec=cbind(users$type)), ordinal=FALSE, hessian=TRUE)
summary(classfit3)
classfit2$BIC-classfit3$BIC         # Descartado

# 4th model: Removing the type variable in propensity to send
classfit4 <-rem.dyad(data,n=nrow(users),effects=c("CovSnd","CovRec"),
                     covar=list(CovSnd=cbind(intercept),
                                CovRec=cbind(users$is_leader,users$is_org)),ordinal=FALSE,hessian=TRUE)
summary(classfit4)
classfit3$BIC-classfit4$BIC #Reduced model is indeed preferred

# 5th mdoel: Recency effects would seem to be a reasonable bet
classfit5 <- rem.dyad(data,n=nrow(users),effects=c("CovSnd","CovRec","RRecSnd","RSndSnd"),
                    covar=list(CovSnd=cbind(intercept),
                               CovRec=cbind(users$is_leader,users$is_org)),ordinal=FALSE,hessian=TRUE)
summary(classfit5)
classfit4$BIC-classfit5$BIC 

# we incorporate the appropriate P-shift effects into our cumulative model:
classfit6 <- rem.dyad(data,n=nrow(users),effects=c("CovSnd","CovRec", "RRecSnd","PSAB-BA","PSAB-AY","PSAB-BY"),
                      covar=list(CovSnd=cbind(intercept),
                                 CovRec=cbind(users$is_leader,users$is_org)),ordinal=FALSE,hessian=TRUE)
summary(classfit6)
classfit5$BIC-classfit6$BIC #Enhanced model is again preferred

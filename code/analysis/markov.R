folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
library(plyr)
library(reshape2)
library(markovchain)
library(diagram)
library(xtable)
source(file='../libraries.R')
source(file='../dbConnect.R')
source(file='../accountsList.R')

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

# Generamos la red
network <- tweets[,c("source", "target", "type")]
network$source_group <- "People"
network$target_group <- "People"

for(i in 1:nrow(network))
{
  if(network[i,]$source %in% orgs) { network[i,]$source_group <- "Orgs" 
  } else if(network[i,]$source %in% leaders) { network[i,]$source_group <- "Leaders" }
  
  if(network[i,]$target %in% orgs){ network[i,]$target_group <- "Orgs" 
  } else if(network[i,]$target %in% leaders) { network[i,]$target_group <- "Leaders"}
}

# Calculamos las probabilidades
proportions <- c()
proportions <- c()
groups <- c("Orgs", "Leaders", "People")

for(i in groups)
{
  for(j in groups)
  {
    tweetsReply <- nrow(network[(network$source_group == i & network$target_group == j & network$type == "reply"), ])
    tweetsMention <- nrow(network[(network$source_group == i & network$target_group == j & network$type == "mention"), ])
    tweetsRetweet <- nrow(network[(network$source_group == i & network$target_group == j & network$type == "retweet"), ])
    
    row <- c()
    row$source <- i
    row$target <- j
    row$reply <- tweetsReply
    row$mention <- tweetsMention
    row$retweet <- tweetsRetweet
    proportions <- rbind(proportions, as.data.frame(row))
  }
}
rm(tweetsMention, tweetsReply, tweetsRetweet, row)

# Replies
total.reply <- proportions[,c('source', 'target', 'reply')]
total.reply <- dcast(total.reply, source~target, value.var="reply")
rownames(total.reply) <- total.reply$source
total.reply$source <- NULL

total.retweet <- proportions[,c('source', 'target', 'retweet')]
total.retweet <- dcast(total.retweet, source~target, value.var="retweet")
rownames(total.retweet) <- total.retweet$source
total.retweet$source <- NULL

total.mention <- proportions[,c('source', 'target', 'mention')]
total.mention <- dcast(total.mention, source~target, value.var="mention")
rownames(total.mention) <- total.mention$source
total.mention$source <- NULL

proportions.reply <- as.data.frame(t(total.reply))
proportions.reply <- as.data.frame(lapply(proportions.reply, function(x) x/sum(x, na.rm=TRUE)))
rownames(proportions.reply) <- colnames(proportions.reply) #columnas es la fuente, #filas es el objetivo
proportions.reply <- as.data.frame(t(proportions.reply))
rownames(proportions.reply) <- colnames(proportions.reply) #columnas es la fuente, #filas es el objetivo

proportions.retweet <- as.data.frame(t(total.retweet))
proportions.retweet <- as.data.frame(lapply(proportions.retweet, function(x) x/sum(x, na.rm=TRUE)))
rownames(proportions.retweet) <- colnames(proportions.retweet) #columnas es la fuente, #filas es el objetivo
proportions.retweet <- as.data.frame(t(proportions.retweet))
rownames(proportions.retweet) <- colnames(proportions.retweet) #columnas es la fuente, #filas es el objetivo

proportions.mention <- as.data.frame(t(total.mention))
proportions.mention <- as.data.frame(lapply(proportions.mention, function(x) x/sum(x, na.rm=TRUE)))
rownames(proportions.mention) <- colnames(proportions.mention) #columnas es la fuente, #filas es el objetivo
proportions.mention <- as.data.frame(t(proportions.mention))
rownames(proportions.mention) <- colnames(proportions.mention) #columnas es la fuente, #filas es el objetivo

# Imprimimos en Latex
xtable(100*proportions.mention)
xtable(100*proportions.reply)
xtable(100*proportions.retweet)

# Plots
transitionMatrix.mention <- data.matrix(t(proportions.mention))
transitionMatrix.reply <- data.matrix(t(proportions.reply))
transitionMatrix.retweet <- data.matrix(t(proportions.retweet))

# Creamos los usuarios
users.source <- sort(unique(unlist(tweets$source, use.names = FALSE)))
users.target <- sort(unique(unlist(tweets$target, use.names = FALSE)))
users <- c(users.source, users.target)
users <- as.data.frame(sort(unique(unlist(users, use.names = FALSE))))
colnames(users) <- c("username")
users$username <- as.character(users$username)

# Calculamos el número de usuarios
numberUsers <- c()
numberUsers$group <- c("Orgs", "Leaders", "People")
numberUsers$number <- c(length(users[(users$username %in% orgs), ]), length(users[(users$username %in% leaders), ]), length(users[!(users$username %in% orgs | users$username %in% leaders), ]))
numberUsers <- as.data.frame(numberUsers)
numberUsers$group <- as.character(numberUsers$group)

# MARKOV
nodeSizes <- setNames(c(0.12,0.12,0.2), numberUsers$group)
distances.x <- c(0.1, -0.12, 0.27)
distances.y <- c(-0.12, -0.12, 0)
  
# Configuramos las curvas
curves.reply <- matrix(nrow = nrow(transitionMatrix.reply), ncol = ncol(transitionMatrix.reply), 0.1)
curves.mention <- matrix(nrow = nrow(transitionMatrix.mention), ncol = ncol(transitionMatrix.mention), 0.1)
curves.retweet <- matrix(nrow = nrow(transitionMatrix.retweet), ncol = ncol(transitionMatrix.retweet), 0.1)

#Plot final
pdf("../../plots/markov.pdf",9,3)
par(mfrow=c(1,3)) 
p1 <- plotmat(100*round(transitionMatrix.reply, 2), curve = curves.reply,
              name = colnames(transitionMatrix.reply), lwd = 1, box.lwd = 2,
              cex.txt = 1, box.cex = 1, box.size = nodeSizes,
              arr.lwd = 5*transitionMatrix.reply,
              box.type = "ellipse", box.prop = 1, self.lwd = 5*diag(transitionMatrix.reply),
              shadow.size = 0, self.cex = 0.4, my = -0.075, mx = -0.01,
              relsize = 0.9, self.shiftx = distances.x,
              self.shifty = distances.y, main = "a) Replies proportion (%)")
p2 <- plotmat(100*round(transitionMatrix.retweet, 2), curve = curves.retweet,
              name = colnames(transitionMatrix.retweet), lwd = 1, box.lwd = 2,
              cex.txt = 1, box.cex = 1, box.size = nodeSizes,
              arr.lwd = 5*transitionMatrix.retweet,
              box.type = "ellipse", box.prop = 1, self.lwd = 5*diag(transitionMatrix.retweet),
              shadow.size = 0, self.cex = 0.4, my = -0.075, mx = -0.01,
              relsize = 0.9, self.shiftx = distances.x,
              self.shifty = distances.y, main = "b) Retweets proportion (%)")
p3 <- plotmat(100*round(transitionMatrix.mention, 2), curve = curves.mention,
              name = colnames(transitionMatrix.mention), lwd = 1, box.lwd = 2,
              cex.txt = 1, box.cex = 1, box.size = nodeSizes,
              arr.lwd = 5*transitionMatrix.mention,
              box.type = "ellipse", box.prop = 1, self.lwd = 5*diag(transitionMatrix.mention),
              shadow.size = 0, self.cex = 0.4, my = -0.075, mx = -0.01,
              relsize = 0.9, self.shiftx = distances.x,
              self.shifty = distances.y, main = "c) Mentions proportion (%)")
dev.off()

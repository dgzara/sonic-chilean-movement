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

# Configuro UTF-8
dbSendQuery(mydb, "SET NAMES utf8")

# Obtenemos los tweets dirigidos
q <- paste('SELECT lower(source) as source, lower(target) as target, type, hashtag, datetime  
           FROM hashtags_network',sep="")
tweets <- dbGetQuery(mydb, q)
tweets$tweetid <- NULL
tweets$datetime <- as.POSIXct(tweets$datetime, format="%a %b %d %H:%M:%S %z %Y") 

# Obtengo los mejores hashtags
q <- paste('SELECT hashtag, COUNT(*) 
           FROM hashtags_network
           GROUP BY hashtag
           ORDER BY COUNT(*) DESC',sep="")
hashtags.ranking <- dbGetQuery(mydb, q)

# Dejamos a los tweets de los 20 hasthags
tweets <- tweets[c(tweets$hashtag %in% hashtags.ranking$hashtag[1:20]),]

# Solo dejamos a los líderes, organizaciones, y common-people
#tweets <- tweets[!(tweets$source %in% movs),]
#tweets <- tweets[!(tweets$source %in% celebrities),]
#tweets <- tweets[!(tweets$source %in% media),]
#tweets <- tweets[!(tweets$target %in% movs),]
#tweets <- tweets[!(tweets$target %in% celebrities),]
#tweets <- tweets[!(tweets$target %in% media),]

# Generamos la red
network <- tweets[,c("source", "target", "type")]
network$source_group <- "People"
network$target_group <- "People"

# Seteamos los grupos
network[(network$source %in% leaders),]$source_group <- "Leaders"
network[(network$source %in% orgs),]$source_group <- "Orgs"
network[(network$target %in% leaders),]$target_group <- "Leaders"
network[(network$target %in% orgs),]$target_group <- "Orgs"

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

# Imprimimos en Latex
xtable(total.mention)
xtable(total.retweet)
xtable(total.reply)

# Convertimos a log10
#total.retweet <- log(total.retweet)
#total.reply <- log(total.reply)
#total.mention <- log(total.mention)
  
# Calculamos las proporciones
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

# Plots
transitionMatrix.mention <- data.matrix(t(log10(total.mention)))
transitionMatrix.reply <- data.matrix(t(log10(total.reply)))
transitionMatrix.retweet <- data.matrix(t(log10(total.retweet)))

# Agregamos el prefijo
proportion.mention.string <- matrix(nrow = nrow(proportions.mention), ncol = ncol(proportions.mention), "")
proportion.reply.string <- matrix(nrow = nrow(proportions.reply), ncol = ncol(proportions.reply), "")
proportion.retweet.string <- matrix(nrow = nrow(proportions.retweet), ncol = ncol(proportions.retweet), "")

for(i in 1:nrow(proportions.mention)){
  for(j in 1:ncol(proportions.mention)){
    proportion.mention.string[i,j] <- paste0(round(100*proportions.mention[i,j],1)," ",sep="")
    proportion.reply.string[i,j] <- paste0(round(100*proportions.reply[i,j],1)," ",sep="")
    proportion.retweet.string[i,j] <- paste0(round(100*proportions.retweet[i,j],1)," ",sep="")
  }
}

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
nodeSizes <- setNames(0.06*log10(numberUsers$number), numberUsers$group)
distances.x <- c(0.15, -0.15, 0.36)
distances.y <- c(-0.09, -0.09, 0)

# Configuramos las curvas
curves.reply <- matrix(nrow = nrow(transitionMatrix.reply), ncol = ncol(transitionMatrix.reply), 0.05)
curves.mention <- matrix(nrow = nrow(transitionMatrix.mention), ncol = ncol(transitionMatrix.mention), 0.05)
curves.retweet <- matrix(nrow = nrow(transitionMatrix.retweet), ncol = ncol(transitionMatrix.retweet), 0.05)

colors <- matrix(nrow = nrow(transitionMatrix.reply), ncol = ncol(transitionMatrix.reply), "black")
colors[,1] <- "#8AB6FA"
colors[,2] <- "#24C467"
colors[,3] <- "#F87570"
colors.nodes <- c("#8AB6FA", "#24C467", "#F87570")
cex.text <- 0.92
arr.lwd <- 0.1
arr.length <- 0.07
arr.width <- 0.07
arr.pos <- matrix(nrow = nrow(transitionMatrix.reply), ncol = ncol(transitionMatrix.reply), 0.5)
arr.pos[2,3] <- 0.60
arr.pos[3,2] <- 0.45
arr.pos[1,3] <- 0.60
arr.pos[3,1] <- 0.45
dtext <- 0.15

#Plot final
pdf("../../plots/markov.pdf",9,3)
par(mfrow=c(1,3)) 
p1 <- plotmat(t(proportion.retweet.string), curve = curves.retweet,
              name = colnames(transitionMatrix.retweet), lwd = 1, box.lwd = 2,
              cex.txt = cex.text, box.cex = 1, box.size = nodeSizes,
              arr.pos = arr.pos, arr.lwd = arr.lwd*transitionMatrix.retweet, arr.width = arr.width*transitionMatrix.retweet, arr.length = arr.length*transitionMatrix.retweet,
              dtext = dtext, box.type = "ellipse", box.lcol = colors.nodes, box.col = colors.nodes, box.prop = 1, self.lwd = 0.05*diag(transitionMatrix.retweet),
              shadow.size = 0, self.cex = 0.3, my = -0.15, mx = -0.01,
              relsize = 0.85, self.shiftx = distances.x, 
              arr.type = "triangle", self.shifty = distances.y, arr.col = colors, arr.lcol = colors, main = "a) Retweets proportion (%)")
p2 <- plotmat(t(proportion.reply.string), curve = curves.reply,
              name = colnames(transitionMatrix.reply), lwd = 1, box.lwd = 2,
              cex.txt = cex.text, box.cex = 1, box.size = nodeSizes,
              arr.pos = arr.pos, arr.lwd = arr.lwd*transitionMatrix.reply, arr.width = arr.width*transitionMatrix.reply, arr.length = arr.length*transitionMatrix.reply,
              dtext = dtext, box.type = "ellipse", box.lcol = colors.nodes, box.col = colors.nodes, box.prop = 1, self.lwd = 0.05*diag(transitionMatrix.reply),
              shadow.size = 0, self.cex = 0.3, my = -0.15, mx = -0.01,
              relsize = 0.85, self.shiftx = distances.x,
              arr.type = "triangle", self.shifty = distances.y, arr.col = colors, arr.lcol = colors, main = "b) Replies proportion (%)")
p3 <- plotmat(t(proportion.mention.string), curve = curves.mention,
              name = colnames(transitionMatrix.mention), lwd = 1, box.lwd = 2,
              cex.txt = cex.text, box.cex = 1, box.size = nodeSizes,
              arr.pos = arr.pos, arr.lwd = arr.lwd*transitionMatrix.mention, arr.width = arr.width*transitionMatrix.mention, arr.length = arr.length*transitionMatrix.mention,
              dtext = dtext, box.type = "ellipse", box.lcol = colors.nodes, box.col = colors.nodes, box.prop = 1, self.lwd = 0.05*diag(transitionMatrix.mention),
              shadow.size = 0, self.cex = 0.3, my = -0.15, mx = -0.01,
              relsize = 0.85, self.shiftx = distances.x,
              arr.type = "triangle", self.shifty = distances.y, arr.col = colors, arr.lcol = colors, main = "c) Mentions proportion (%)")
dev.off()

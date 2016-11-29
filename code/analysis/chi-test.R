folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
library(stats)
library(reshape2)
library(diagram)
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

# Generamos la red
network <- tweets[,c("source", "target", "type")]
network$source_group <- "People"
network$target_group <- "People"

# Seteamos los grupos
network[(network$source %in% leaders),]$source_group <- "Leaders"
network[(network$source %in% orgs),]$source_group <- "Orgs"
network[(network$target %in% leaders),]$target_group <- "Leaders"
network[(network$target %in% orgs),]$target_group <- "Orgs"

# Revisamos por año
table <- c()
table.reply <- c()
table.mention <- c()
table.retweet <- c()

for(i in c("People", "Orgs", "Leaders"))
{
  row <- c()
  row$retweets <- nrow(network[(network$source_group == i & network$type == "retweet"),])
  row$replies <- nrow(network[(network$source_group == i & network$type == "reply"),])
  row$mentions <- nrow(network[(network$source_group == i & network$type == "mention"),])
  row <- as.data.frame(row)
  rownames(row) <- i
  table <- rbind(table, row)
  
  for(j in c("People", "Orgs", "Leaders"))
  {
    table.reply <- rbind(table.reply, data.frame(i, j, nrow(network[(network$source_group == i & network$target_group == j & network$type == "reply"), ]), stringsAsFactors = FALSE))
    table.mention <- rbind(table.mention, data.frame(i, j, nrow(network[(network$source_group == i & network$target_group == j & network$type == "mention"), ]), stringsAsFactors = FALSE))
    table.retweet <- rbind(table.retweet, data.frame(i, j, nrow(network[(network$source_group == i & network$target_group == j & network$type == "retweet"), ]), stringsAsFactors = FALSE))
  }
}

colnames(table.reply) <- colnames(table.mention) <- colnames(table.retweet) <- c("source", "target", "value")
table.reply <- dcast(table.reply, source~target)
table.mention <- dcast(table.mention, source~target)
table.retweet <- dcast(table.retweet, source~target)
table.reply$source <- table.retweet$source <- table.mention$source <- NULL
rownames(table.reply) <- rownames(table.mention) <- rownames(table.retweet) <- c("Leaders", "Orgs", "People")

# Pearson's Chi-squared test
Xsq <- chisq.test(table)
print(Xsq)
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals

# Only leaders and organizations
table.leadership <- table[(2:3),]

# Pearson's Chi-squared test
Xsq2 <- chisq.test(table.leadership)
print(Xsq2)
Xsq2$observed   # observed counts (same as M)
Xsq2$expected   # expected counts under the null
Xsq2$residuals  # Pearson residuals
Xsq2$stdres     # standardized residuals

# Pearson's Chi-squared test
Xsq.retweet <- chisq.test(table.retweet)
print(Xsq.retweet)
Xsq.retweet$observed   # observed counts (same as M)
Xsq.retweet$expected   # expected counts under the null
Xsq.retweet$residuals  # Pearson residuals
Xsq.retweet$stdres     # standardized residuals
Xsq.retweet$observed - Xsq.retweet$expected

# Table Retweet
table.retweet.xsq <- data.frame(Xsq.retweet$observed, Xsq.retweet$expected, Xsq.retweet$stdres, stringsAsFactors = FALSE)
table.retweet.xsq <- table.retweet.xsq[,c("Leaders", "Leaders.1", "Leaders.2", "Orgs", "Orgs.1", "Orgs.2", "People", "People.1", "People.2")]
table.retweet.xsq <- round(table.retweet.xsq, 2)
colnames(table.retweet.xsq) <- c("Observed", "Expected", "Residuals","Observed", "Expected", "Residuals","Observed", "Expected", "Residuals") 
print(table.retweet.xsq)

# Pearson's Chi-squared test
Xsq.reply <- chisq.test(table.reply)
print(Xsq.reply)
Xsq.reply$observed   # observed counts (same as M)
Xsq.reply$expected   # expected counts under the null
Xsq.reply$residuals  # Pearson residuals
Xsq.reply$stdres     # standardized residuals
Xsq.reply$observed - Xsq.reply$expected

# Table Reply
table.reply.xsq <- data.frame(Xsq.reply$observed, Xsq.reply$expected, Xsq.reply$stdres, stringsAsFactors = FALSE)
table.reply.xsq <- table.reply.xsq[,c("Leaders", "Leaders.1", "Leaders.2", "Orgs", "Orgs.1", "Orgs.2", "People", "People.1", "People.2")]
table.reply.xsq <- round(table.reply.xsq, 2)
colnames(table.reply.xsq) <- c("Observed", "Expected", "Residuals","Observed", "Expected", "Residuals","Observed", "Expected", "Residuals") 
print(table.reply.xsq)

# Pearson's Chi-squared test
Xsq.mention <- chisq.test(table.mention)
print(Xsq.mention)
Xsq.mention$observed   # observed counts (same as M)
Xsq.mention$expected   # expected counts under the null
Xsq.mention$residuals  # Pearson residuals
Xsq.mention$stdres     # standardized residuals
Xsq.mention$observed - Xsq.mention$expected

# Table Mention
table.mention.xsq <- data.frame(Xsq.mention$observed, Xsq.mention$expected, Xsq.mention$stdres, stringsAsFactors = FALSE)
table.mention.xsq <- table.mention.xsq[,c("Leaders", "Leaders.1", "Leaders.2", "Orgs", "Orgs.1", "Orgs.2", "People", "People.1", "People.2")]
table.mention.xsq <- round(table.mention.xsq, 2)
colnames(table.mention.xsq) <- c("Observed", "Expected", "Residuals","Observed", "Expected", "Residuals","Observed", "Expected", "Residuals") 
print(table.mention.xsq)

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
nodeSizes <- setNames(0.04*log10(numberUsers$number), numberUsers$group)
distances.x <- c(0.09, -0.09, 0.23)
distances.y <- c(-0.09, -0.09, 0.09)

# Configuramos las curvas
curves.reply <- matrix(nrow = nrow(Xsq.reply$stdres), ncol = ncol((Xsq.reply$stdres)), 0.05)
curves.mention <- matrix(nrow = nrow(Xsq.mention$stdres), ncol = ncol(Xsq.mention$stdres), 0.05)
curves.retweet <- matrix(nrow = nrow(Xsq.retweet$stdres), ncol = ncol(Xsq.retweet$stdres), 0.05)

colors <- matrix(nrow = nrow(Xsq.mention$stdres), ncol = ncol(Xsq.mention$stdres), "black")
colors[,1] <- "#8AB6FA"
colors[,2] <- "#24C467"
colors[,3] <- "#F87570"
colors.nodes <- c("#8AB6FA", "#24C467", "#F87570")
cex.text <- 1
arr.lwd <- 0.35
arr.length <- 0.01
arr.width <- 0.01
arr.pos <- matrix(nrow = nrow(Xsq.mention$stdres), ncol = ncol(Xsq.mention$stdres), .74)
arr.pos[3,2] <- 0.6
arr.pos[3,1] <- 0.6
arr.pos[2,1] <- 0.75
arr.pos[1,2] <- 0.75
dtext <- matrix(nrow = nrow(Xsq.mention$stdres), ncol = ncol(Xsq.mention$stdres), 0)

# Plot
pdf("../../plots/markov_chi.pdf",9,3)
par(mfrow=c(1,3)) 
p1 <- plotmat(t(round(Xsq.retweet$stdres,2)), curve = curves.retweet,
              name = colnames(Xsq.retweet$stdres), lwd = 1, box.lwd = 2,
              cex.txt = cex.text, box.cex = 1, box.size = nodeSizes,
              arr.pos = arr.pos, arr.lwd = arr.lwd*t(abs(Xsq.retweet$stdres)), arr.width = arr.width*t(abs(Xsq.retweet$stdres)), arr.length = arr.length*t(abs(Xsq.retweet$stdres)),
              dtext = dtext, box.type = "ellipse", box.lcol = colors.nodes, box.col = colors.nodes, box.prop = 1, self.lwd = arr.lwd*diag(abs(Xsq.retweet$stdres)),
              shadow.size = 0, self.cex = 0.3, my = -0.15, mx = -0.01,
              relsize = 0.85, self.shiftx = distances.x, 
              arr.type = "triangle", self.shifty = distances.y, arr.col = colors, arr.lcol = colors, main = "a) Retweets standard residuals")
p2 <- plotmat(t(round(Xsq.mention$stdres,2)), curve = curves.mention,
              name = colnames(Xsq.mention$stdres), lwd = 1, box.lwd = 2,
              cex.txt = cex.text, box.cex = 1, box.size = nodeSizes,
              arr.pos = arr.pos, arr.lwd = arr.lwd*t(abs(Xsq.mention$stdres)), arr.width = arr.width*t(abs(Xsq.mention$stdres)), arr.length = arr.length*t(abs(Xsq.mention$stdres)),
              dtext = dtext, box.type = "ellipse", box.lcol = colors.nodes, box.col = colors.nodes, box.prop = 1, self.lwd = arr.lwd*diag(abs(Xsq.mention$stdres)),
              shadow.size = 0, self.cex = 0.3, my = -0.15, mx = -0.01,
              relsize = 0.85, self.shiftx = distances.x, 
              arr.type = "triangle", self.shifty = distances.y, arr.col = colors, arr.lcol = colors, main = "b) Mentions standard residuals")
p3 <- plotmat(t(round(Xsq.reply$stdres,2)), curve = curves.reply,
              name = colnames(Xsq.reply$stdres), lwd = 1, box.lwd = 2,
              cex.txt = cex.text, box.cex = 1, box.size = nodeSizes,
              arr.pos = arr.pos, arr.lwd = arr.lwd*abs(t(Xsq.reply$stdres)), arr.width = arr.width*abs(t(Xsq.reply$stdres)), arr.length = arr.length*abs(t(Xsq.reply$stdres)),
              dtext = dtext, box.type = "ellipse", box.lcol = colors.nodes, box.col = colors.nodes, box.prop = 1, self.lwd = arr.lwd*diag(abs(Xsq.reply$stdres)),
              shadow.size = 0, self.cex = 0.3, my = -0.15, mx = -0.01,
              relsize = 0.85, self.shiftx = distances.x, 
              arr.type = "triangle", self.shifty = distances.y, arr.col = colors, arr.lcol = colors, main = "c) Replies standard residuals")
dev.off()

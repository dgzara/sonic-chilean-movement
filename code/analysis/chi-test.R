folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
library(stats)
library(reshape2)
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

# Pearson's Chi-squared test
Xsq.reply <- chisq.test(table.reply)
print(Xsq.reply)
Xsq.reply$observed   # observed counts (same as M)
Xsq.reply$expected   # expected counts under the null
Xsq.reply$residuals  # Pearson residuals
Xsq.reply$stdres     # standardized residuals
Xsq.reply$observed - Xsq.reply$expected

# Pearson's Chi-squared test
Xsq.mention <- chisq.test(table.mention)
print(Xsq.mention)
Xsq.mention$observed   # observed counts (same as M)
Xsq.mention$expected   # expected counts under the null
Xsq.mention$residuals  # Pearson residuals
Xsq.mention$stdres     # standardized residuals
Xsq.mention$observed - Xsq.mention$expected

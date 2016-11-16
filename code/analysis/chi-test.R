folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
library(stats)
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
for(i in c("People", "Orgs", "Leaders"))
{
  row <- c()
  row$retweets <- nrow(network[(network$source_group == i & network$type == "retweet"),])
  row$replies <- nrow(network[(network$source_group == i & network$type == "reply"),])
  row$mentions <- nrow(network[(network$source_group == i & network$type == "mention"),])
  row <- as.data.frame(row)
  rownames(row) <- i
  table <- rbind(table, row)
}

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


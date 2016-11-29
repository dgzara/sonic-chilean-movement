folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
source(file='../libraries.R')
source(file='../dbConnect.R')
source(file='../accountsList.R')
library(poweRlaw)

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

# Test if the entire graph follows a power-law
g <- graph.data.frame(tweets[,c("source","target")], directed=TRUE)
degree <- degree(g, V(g))
m_pl <- displ$new(degree)
est <- estimate_xmin(m_pl)
m_pl$setXmin(est)
plot(m_pl)
lines(m_pl, col=2)
m_pl$pars
bs_p <- bootstrap_p(m_pl)

for(i in c("retweet", "reply", "mention")){
  g <- graph.data.frame(tweets[(tweets$type == i),c("source","target")], directed=TRUE)
  
  # Obtenemos la distribución
  degree <- degree(g, V(g))
  m_pl <- displ$new(degree)
  est <- estimate_xmin(m_pl)
  m_pl$setXmin(est)
  m_pl$pars
  #bs_p <- bootstrap_p(m_pl)
  
  assign(paste("m_pl.", i, sep = ""), m_pl)
  #assign(paste("bs_p.", i, sep = ""), m_pl)
  
  #Plot final
  pdf(paste("../../plots/distribucion_",i,".pdf",sep=""),6,5)
  plot(m_pl, main=paste(i," network",sep=""), xlab="User degree", ylab="Distribution")
  lines(m_pl, col=2)
  lines(m_ln, col="orange")
  dev.off()
}

# Chequeamos
bs_p.mention <- bootstrap_p(m_pl.mention)
bs_p.mention$p

bs_p.reply <- bootstrap_p(m_pl.reply)
bs_p.reply$p

bs_p.retweet <- bootstrap_p(m_pl.retweet)
bs_p.retweet$p

# Vemos para el caso de los retweets
g.retweet <- graph.data.frame(tweets[(tweets$type == "retweet"),c("source","target")], directed=TRUE)
data_pl <- displ$new(degree(g.retweet, V(g.retweet)))
est.retweet <- estimate_xmin(data_pl)
data_pl$setXmin(est.retweet)

data_alt <- dislnorm$new(degree(g.retweet, V(g.retweet)))
data_alt$xmin <- est.retweet$xmin
data_alt$pars <- estimate_pars(data_alt)
comp <- compare_distributions(data_pl, data_alt)

comp$p_two_sided
comp$test_statistic

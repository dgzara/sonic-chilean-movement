folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
source(file='../libraries.R')
source(file='../dbConnect.R')
source(file='../accountsList.R')
source(file='functions.R')
library(MASS)

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

# Revisamos por red
for(i in c("retweet", "reply", "mention"))
{
  g <- graph.data.frame(tweets[(tweets$type == i),c("source","target")], directed=TRUE)
  
  # Asignamos a todos como people
  V(g)$group <- "People"
  V(g)$size <- 1
  V(g)$alpha <- 0.8
  
  # Asignamos los grupos
  V(g)[V(g)$name %in% leaders]$group <- "Student Leaders"
  V(g)[V(g)$name %in% orgs]$group <- "Student Organizations"
  V(g)[V(g)$name %in% leaders]$size <- 2
  V(g)[V(g)$name %in% orgs]$size <- 2
  V(g)[V(g)$name %in% leaders]$alpha <- 1
  V(g)[V(g)$name %in% orgs]$alpha <- 1
  
  # Average neighbor indegree versus vertex indegree (log–log scale)
  g.simplified <- simplify(g)
  a <- adjacent_vertices(g.simplified, V(g.simplified), mode = c("out"))
  a.nn.deg.network <- lapply(a, function(x){
    mean(degree(g.simplified, V(g.simplified)[V(g.simplified) %in% x[]], mode="in"))
  })
  d.network.simplified <- degree(g.simplified, mode="in")
  
  # Asignamos
  assign(paste("a.", i, sep = ""), a)
  assign(paste("a.nn.deg.network.", i, sep = ""), a.nn.deg.network)
  
  # Genero los datos
  x <- as.numeric(d.network.simplified)
  y <- as.numeric(a.nn.deg.network)
  z <- V(g.simplified)$group
  l <- V(g.simplified)$alpha
  k <- V(g.simplified)$size
  d <- na.omit(data.frame(x, y, z, l, k))
  
  # Descubro el máximo
  d.ordered <- d[order(d[,1]),]
  max_values <- c()
  for(j in unique(d.ordered$x))
  {
    row <- c()
    row$x <- j
    row$y <- max(d.ordered[d.ordered$x == j,]$y)
    max_values <- rbind(max_values, as.data.frame(row))
  }
  max_values <- max_values[2:nrow(max_values),] 
  
  # Obtengo los parámetros del fit
  m.leaders <- lm(log(y) ~ log(x), d[(d$z=="Student Leaders" & d$x > 1),]) 
  summary(m.leaders)
  
  m.organizations <- lm(log(y) ~ poly(x, 2, raw=TRUE), d[(d$z=="Student Organizations" & d$x > 1),]) 
  summary(m.organizations)
  
  eq <- paste("gamma~'='~",b = format(-coef(m)[2], digits = 2),"~','~~italic(r)^2~'='~",format(summary(m)$r.squared, digits = 2), sep="")
  
  # Obtengo los parámetros del fit
  m.leaders <- lm(log(y) ~ log(x), d[(d$z=="Student Leaders" & d$x >0),c("x","y")]) 
  m.orgs <- lm(log(y) ~ log(x), d[(d$z=="Student Organizations" & d$x >0),c("x","y")]) 
  eq <- paste("gamma~'='~",b = format(-coef(m)[2], digits = 2),"~','~~italic(r)^2~'='~",format(summary(m)$r.squared, digits = 2), sep="")
  
  # Construyo el grafico
  p1 <- ggplot(max_values, aes(x=x, y=y)) + 
    #geom_point() + 
    geom_point(data = d, aes(x=x, y=y,color = z, shape = z, alpha = l)) +
    stat_smooth(method="lm", formula = y ~ x, color="red", se=0, na.rm = TRUE, size=1) +
    scale_size_continuous(range = c(1, 2)) +
    scale_alpha_continuous(range = c(0.1,1)) +
    scale_x_log10(limits=c(1, max(d$x))) + 
    scale_y_log10(limits=c(1, max(d$y))) +
    theme_bw() +
    theme(legend.justification=c(1,1), legend.position=c(1,1),legend.title=element_blank(), panel.grid.minor = element_line(color="grey", linetype="dotted"), panel.grid.major = element_line(color="grey", linetype="dotted")) +
    xlab("Log Vertex Indegree") +
    ylab("Log Average Neighbor Indegree") +
    annotation_logticks(base = 10) + 
    guides(size = FALSE, alpha = FALSE) + scale_fill_discrete("")
  #annotate("text", x = 250, y = 250, label = as.character(as.expression(eq)), parse=TRUE, color="red")
  
  p3 <- ggplot(d, aes(x=x, y=y, color=z, fill=z)) + 
    stat_smooth(formula = y ~ x, na.rm = TRUE, size=1, alpha = 0.1) +
    geom_point(aes(x=x, y=y,color = z, shape = z, alpha = l)) +
    scale_size_continuous(range = c(1, 2)) +
    scale_alpha_continuous(range = c(0.1,1)) +
    scale_x_log10(limits=c(1, max(d$x))) + 
    scale_y_log10(limits=c(1, max(d$y))) +
    theme_bw() +
    theme(legend.justification=c(1,0), legend.position=c(1,0),legend.title=element_blank(), panel.grid.minor = element_line(color="grey", linetype="dotted"), panel.grid.major = element_line(color="grey", linetype="dotted")) +
    xlab("Log Vertex Indegree") +
    ylab("Log Average Neighbor Indegree") +
    annotation_logticks(base = 10) + ggtitle(paste(i," network", sep="")) +
    guides(size = FALSE, alpha = FALSE) + scale_fill_discrete(guide=FALSE)
  
  p4 <- ggplot(d[(d$z != "People"),], aes(x=x, y=y, fill=z, color=z)) + 
    geom_point(aes(x=x, y=y,color = z, shape = z, alpha = l)) +
    stat_smooth(formula = y ~ x, na.rm = TRUE, size=1, alpha = 0.1) +
    scale_size_continuous(range = c(1, 2)) +
    scale_alpha_continuous(range = c(0.1,1)) +
    scale_x_log10(limits=c(1, max(d$x))) + 
    scale_y_log10(limits=c(1, max(d$y))) +
    theme_bw() +
    theme(legend.justification=c(1,0), legend.position=c(1,0),legend.title=element_blank(), panel.grid.minor = element_line(color="grey", linetype="dotted"), panel.grid.major = element_line(color="grey", linetype="dotted")) +
    xlab("Log Vertex Indegree") +
    ylab("Log Average Neighbor Indegree") +
    annotation_logticks(base = 10) + ggtitle(paste(i," network - without people", sep="")) +
    scale_fill_manual(values=c("#24C467", "#8AB6FA")) +
    scale_color_manual(values=c("#24C467", "#8AB6FA")) +
    guides(size = FALSE, alpha = FALSE)
  
  #Plot final
  pdf(paste("../../plots/average_neighbor/neighbor_",i,"_p1.pdf", sep=""),6,5)
  p1
  dev.off()
  
  pdf(paste("../../plots/average_neighbor/neighbor_",i,"_p3.pdf", sep=""),6,5)
  p3
  dev.off()
  
  pdf(paste("../../plots/average_neighbor/neighbor_",i,"_p4.pdf", sep=""),6,5)
  p4
  dev.off()
  
  # Resumen
  a.nn.deg.network.leaders <- d[(d$z=="Student Leaders" & d$x > 1),]$y
  a.nn.deg.network.orgs <- d[(d$z=="Student Organizations" & d$x > 1),]$y
  
  # Comprobamos
  wilcox.test(a.nn.deg.network.leaders, a.nn.deg.network.orgs, alternative = "greater", na.rm=TRUE)$p.value
  
  # Comparamos las varianzas
  f.test <- var.test(a.nn.deg.network.leaders, a.nn.deg.network.orgs)
  f.test$p.value
  f.test$statistic
  
  a.nn.deg.histogram.leaders <- as.data.frame(unlist(a.nn.deg.network[names(a.nn.deg.network)%in%leaders]), stringsAsFactor=FALSE)
  a.nn.deg.histogram.leaders$group <- "leaders"
  a.nn.deg.histogram.orgs <- as.data.frame(unlist(a.nn.deg.network[names(a.nn.deg.network)%in%orgs]), stringsAsFactor=FALSE)
  a.nn.deg.histogram.orgs$group <- "orgs"
  colnames(a.nn.deg.histogram.orgs) <- colnames(a.nn.deg.histogram.leaders) <- c("value", "group")
  a.nn.deg.histogram <- na.omit(rbind(a.nn.deg.histogram.orgs, a.nn.deg.histogram.leaders))
  rm(a.nn.deg.histogram.orgs, a.nn.deg.histogram.leaders)
  
  p2 <- ggplot(a.nn.deg.histogram, aes(x=value, fill=group)) + 
    geom_histogram(position="dodge") + theme_bw() +
    theme(legend.justification=c(1,1), legend.position=c(1,1), legend.title=element_blank(), panel.grid.minor = element_line(color="grey", linetype="dotted"), panel.grid.major = element_line(color="grey", linetype="dotted")) +
    xlab("Average Neighbor Indegree") +
    ylab("Frequency") + 
    scale_fill_manual(values=c("#24C467", "#8AB6FA"))
  #scale_x_log10() +
  p2
  
  # Plot distribution
  pdf(paste("../../plots/average_neighbor/histogram_neighbor_",i,".pdf",sep=""),6,5)
  p2
  dev.off()
  
  # Tablas
  summary(a.nn.deg.network.leaders)
  summary(a.nn.deg.network.orgs)
  
  # Combinado
  #pdf("../../plots/average_neighbor/neighbor.pdf",10,4.5)
  #multiplot(p1, p2, cols=2)
  #dev.off()
}

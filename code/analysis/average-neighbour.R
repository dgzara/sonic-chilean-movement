folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
source(file='../libraries.R')
source(file='../dbConnect.R')
source(file='../accountsList.R')
library(MASS)

# Obtenemos los tweets dirigidos
q <- paste('SELECT source, target, type, hashtag, datetime  
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

# Revisamos por año
networks <- c()
descriptive <- c()
users <- c()

# Realizamos el for
for(i in 2011:2013)
{
  tweetsYear <- selectByDate(tweets, year = i)
  tweetsYear <- tweetsYear[,c("source","target")]
  
  # Generamos el grafo
  network <- graph.data.frame(tweetsYear, directed=TRUE)
  
  # Asignamos a todos como people
  V(network)$group <- "People"
  V(network)$size <- 1
  V(network)$alpha <- 0.8
  
  # Asignamos los grupos
  V(network)[V(network)$name %in% leaders]$group <- "Student Leaders"
  V(network)[V(network)$name %in% orgs]$group <- "Student Organizations"
  V(network)[V(network)$name %in% leaders]$size <- 2
  V(network)[V(network)$name %in% orgs]$size <- 2
  V(network)[V(network)$name %in% leaders]$alpha <- 1
  V(network)[V(network)$name %in% orgs]$alpha <- 1
  
  # Average neighbor degree versus vertex degree (log–log scale)
  network.simplified <- simplify(network)
  a.nn.deg.network <- graph.knn(network.simplified,V(network.simplified))$knn 
  d.network.simplified <- degree(network.simplified)
  
  # Genero los datos
  x <- as.numeric(d.network.simplified)
  y <- as.numeric(a.nn.deg.network)
  z <- V(network.simplified)$group
  l <- V(network.simplified)$alpha
  k <- V(network.simplified)$size
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
  
  # Obtengo los parámetros del fit
  m <- lm(log(y) ~ log(x), max_values) 
  eq <- paste("gamma~'='~",b = format(-coef(m)[2], digits = 2),"~','~~italic(r)^2~'='~",format(summary(m)$r.squared, digits = 2), sep="")
  
  # Conf
  if(i == 2013){xlab <- 100; ylab <- 100}else{xlab <- 200; ylab <- 200}
  
  # Construyo el grafico
  a <- ggplot(max_values, aes(x=x, y=y)) + 
      #geom_point() + 
      geom_point(data = d, aes(x=x, y=y,color = z, shape = z, alpha = l)) +
      stat_smooth(method="lm", formula = y ~ x, color="red", se=0, na.rm = TRUE, size=1) +
      scale_size_continuous(range = c(1, 2)) +
      scale_alpha_continuous(range = c(0.1,1)) +
      scale_x_log10(limits=c(1, max(d$x))) + 
      scale_y_log10(limits=c(1, max(d$y))) +
      theme_bw() +
      theme(panel.grid.minor = element_line(color="grey", linetype="dotted"), panel.grid.major = element_line(color="grey", linetype="dotted")) +
      xlab("Log Vertex Degree") +
      ylab("Log Average Neighbor Degree") +
      annotation_logticks(base = 10) + ggtitle(i) +
      guides(size = FALSE, alpha = FALSE) + scale_fill_discrete("") +
      annotate("text", x = xlab, y = ylab, label = as.character(as.expression(eq)), parse=TRUE, color="red")
  
  #Asigno el grafico a una variable
  assign(paste("plot", i, sep = ""), a)
}

# Creamos la leyenda
g <- ggplotGrob(plot2011 + theme(legend.position="bottom", legend.text=element_text(size = 12)))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)

#Plot final
pdf("../../plots/neighbor_final.pdf",12,4.5)
plot.final <- grid.arrange(arrangeGrob(plot2011 + theme(legend.position="none"),
                                       plot2012 + theme(legend.position="none"),
                                       plot2013 + theme(legend.position="none"),
                                       nrow=1),
                           legend, nrow=2,heights=unit.c(unit(1, "npc") - lheight, lheight)) 
dev.off()
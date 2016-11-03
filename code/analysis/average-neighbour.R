folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
source(file='../libraries.R')
source(file='../dbConnect.R')
source(file='../accountsList.R')
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

# Solo dejamos a los líderes, organizaciones, y common-people
#tweets <- tweets[!(tweets$source %in% movs),]
#tweets <- tweets[!(tweets$source %in% celebrities),]
#tweets <- tweets[!(tweets$source %in% media),]
#tweets <- tweets[!(tweets$target %in% movs),]
#tweets <- tweets[!(tweets$target %in% celebrities),]
#tweets <- tweets[!(tweets$target %in% media),]

# Dejamos a los tweets de los 20 hasthags
tweets <- tweets[c(tweets$hashtag %in% hashtags.ranking$hashtag[1:20]),]

# Revisamos por año
networks <- c()
descriptive <- c()
users <- c()

# Generamos el grafo
network <- graph.data.frame(tweets[,c("source","target")], directed=TRUE)

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
d.network.simplified <- degree(network.simplified, mode="in")

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
max_values <- max_values[2:nrow(max_values),] 
  
# Obtengo los parámetros del fit
m <- lm(log(y) ~ log(x), max_values) 
eq <- paste("gamma~'='~",b = format(-coef(m)[2], digits = 2),"~','~~italic(r)^2~'='~",format(summary(m)$r.squared, digits = 2), sep="")

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
    theme(legend.position= "bottom",legend.title=element_blank(), panel.grid.minor = element_line(color="grey", linetype="dotted"), panel.grid.major = element_line(color="grey", linetype="dotted")) +
    xlab("Log Vertex Indegree") +
    ylab("Log Average Neighbor Degree") +
    annotation_logticks(base = 10) + 
    guides(size = FALSE, alpha = FALSE) + scale_fill_discrete("")
    #annotate("text", x = 250, y = 250, label = as.character(as.expression(eq)), parse=TRUE, color="red")

#Plot final
pdf("../../plots/neighbor_total.pdf",6,5)
a
dev.off()

# Resumen
a.nn.deg.network.leaders <- a.nn.deg.network[names(a.nn.deg.network)%in%leaders]
a.nn.deg.network.orgs <- a.nn.deg.network[names(a.nn.deg.network)%in%orgs]

# Comprobamos
wilcox.test(a.nn.deg.network.leaders, a.nn.deg.network.orgs)$p.value

# Dibujamos el plot
data <- as.data.frame(rbind(cbind(a.nn.deg.network.leaders, "leader"), cbind(a.nn.deg.network.orgs, "org")))
colnames(data) <- c("value", "group")
data$value <- as.numeric(as.character(data$value))

p <- ggplot(data, aes(factor(group), value, fill=factor(group))) +
  geom_violin() + geom_boxplot(width=0.1, fill="white") + 
  xlab("Groups") + 
  scale_y_log10() +
  theme(legend.position="bottom")
p

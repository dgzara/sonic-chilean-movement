folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
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

# Revisamos por año
networks <- c()
descriptive <- c()
users <- c()

# Generamos el grafo
network <- graph.data.frame(tweets[,c("source","target")], directed=TRUE)

# Obtenemos el resto
tweets.people <- V(network)[!((V(network)$name %in% orgs) | (V(network)$name %in% leaders))]$name
people <- unique(unlist(tweets.people, use.names = FALSE))

# Obtenemos la distribución
degree <- degree(network, V(network))

# Obtenemos las distribuciones separadas
degree.leaders <- na.omit(degree[leaders])
degree.orgs <- na.omit(degree[orgs])
degree.people <- na.omit(degree[people])

degree.leaders[degree.leaders==0] <- NA
degree.orgs[degree.orgs==0] <- NA
degree.people[degree.people==0] <- NA

degree.leaders <- na.omit(degree.leaders)
degree.orgs <- na.omit(degree.orgs)
degree.people <- na.omit(degree.people)

# Degree de los normales
degree.people.df <- data.frame(table(degree=factor(degree.people, levels=seq_len(max(degree.people)))))
degree.people.df$degree <- as.numeric(as.character(degree.people.df$degree))  
degree.people.df <- degree.people.df[(degree.people.df$Freq != 0),]

# Degree de los líderes
degree.leaders.df <- data.frame(table(degree=factor(degree.leaders, levels=seq_len(max(degree.leaders)))))
degree.leaders.df$degree <- as.numeric(as.character(degree.leaders.df$degree))
degree.leaders.df <- degree.leaders.df[(degree.leaders.df$Freq != 0),] 

# Degree de las organizaciones
degree.orgs.df <- data.frame(table(degree=factor(degree.orgs, levels=seq_len(max(degree.orgs)))))
degree.orgs.df$degree <- as.numeric(as.character(degree.orgs.df$degree))
degree.orgs.df <- degree.orgs.df[(degree.orgs.df$Freq != 0),] 

# Obtengo los parámetros del fit
m <- lm(log(Freq) ~ log(degree), degree.people.df)
eq <- paste("gamma~'='~",b = format(-coef(m)[2], digits = 3),"~','~~italic(r)^2~'='~",format(summary(m)$r.squared, digits = 2), sep="")

# Creamos el grafico
p1 <- ggplot(data = degree.people.df, aes(x=degree, y=Freq))+ geom_point(data = degree.people.df, aes(x=degree, y=Freq, color = "People", shape="People"), alpha = 0.8) 
p1 <- p1 + geom_point(data = degree.leaders.df, aes(x=degree, y=Freq, color = "Student Leaders", shape="Student Leaders"), size= 2) 
p1 <- p1 + geom_point(data = degree.orgs.df, aes(x=degree, y=Freq, color = "Student Organizations", shape="Student Organizations"), size= 2)
p1 <- p1 + xlab("Degree") + ylab("Frequency") + labs(colour = "Types", shape = "Types")
p1 <- p1 + stat_smooth(method="lm", formula = y ~ x, color="red", na.rm = TRUE, size=0.5)
p1 <- p1 + scale_x_log10(limits=c(NA,max(degree.people.df$degree))) + scale_y_log10(limits=c(1,max(degree.people.df$Freq)))
p1 <- p1 + theme_bw() + theme(legend.position="bottom", legend.title=element_blank(), panel.grid.minor = element_line(color="grey", linetype="dotted"), panel.grid.major = element_line(color="grey", linetype="dotted"))
p1 <- p1 + guides(size = FALSE, alpha = FALSE) 
#p1 <- p1 + annotate("text", x = 120, y = 150, label = as.character(as.expression(eq)), parse=TRUE, color="red")

#Plot final
pdf("../../plots/distribucion_total.pdf",6,5)
p1
dev.off()

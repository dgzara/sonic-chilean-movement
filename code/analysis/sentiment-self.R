## Sentiments for paper
folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
library(ggplot2)
library(gridExtra)
library(grid)

# ===
# Leaders -----------------
# ===

# Cargamos los datos del sentimiento propio generado
data.sentiment.self <- read.csv2("../../data/sentiment/leaders/2011camila_vallejo2.txt", header = TRUE, sep = "|", dec = ".")
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/leaders/2012camila_vallejo2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/leaders/2013camila_vallejo2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/leaders/2011gabrielboric2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/leaders/2012gabrielboric2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/leaders/2013gabrielboric2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/leaders/2011GiorgioJackson2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/leaders/2012GiorgioJackson2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/leaders/2013GiorgioJackson2.txt", header = TRUE, sep = "|", dec = "."))

# Creamos el date
data.sentiment.self$date <- as.Date(paste(data.sentiment.self$year,"-",sprintf("%02d", data.sentiment.self$month),"-01",sep=""), format="%Y-%m-%d")

# Ploteamos
deputies <- c("camila_vallejo","gabrielboric","GiorgioJackson")
election_date <- c(22,25,25)
j <- 0
for(i in deputies){
    j <- j + 1 
    data.deputy <- data.sentiment.self[(data.sentiment.self$account == i),]
    data.deputy.positive <- data.deputy[,c("account", "date", "posemo")]
    data.deputy.negative <- data.deputy[,c("account", "date", "negemo")]
    colnames(data.deputy.positive) <- c("account", "date", "value")
    colnames(data.deputy.negative) <- c("account", "date", "value")
    data.deputy.positive$type <- "positive"
    data.deputy.negative$type <- "negative"
    data.deputy.negative$value <- abs(data.deputy.negative$value)
    data.deputy <- rbind(data.deputy.positive, data.deputy.negative)
    data.deputy$type <- factor(data.deputy$type, levels = c("positive", "negative"))
    rm(data.deputy.positive, data.deputy.negative)
    
    # election_date <- as.numeric(rownames(data.deputy[data.deputy$date == "2013-01-01",])[1])
    print(paste0("Election date is: ", election_date[j]))
    
    p1 <- ggplot(data = data.deputy, aes(x=date, y = value, group = type, color=type)) +
        xlab("") + ylab("Sentiment") + labs(colour = "Type") +
        geom_line() +
        stat_smooth() + 
        # geom_point() + 
        ggtitle(i) +
        theme(panel.background = element_rect(fill = "white")) +
        coord_cartesian(ylim=c(0.8,3.25 )) +
        geom_vline( aes(xintercept = as.numeric( date[c(election_date[j])]) ), colour="black", linetype = "longdash") +
        #facet_wrap(~type, ncol = 1, scales = "free_y") +
        scale_color_manual(values=c("#56B4E9", "#D55E00"))
    assign(paste("plot.", gsub("@", "", i), sep = ""), p1)
}

g <- ggplotGrob(plot.camila_vallejo + theme(legend.position="bottom"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)

# Guardamos
pdf("../../plots/sentiment_leaders.pdf",14,4)
plot.final <- grid.arrange(arrangeGrob(plot.camila_vallejo + theme(legend.position="none"),
                                       plot.gabrielboric + theme(legend.position="none"),
                                       plot.GiorgioJackson + theme(legend.position="none"),
                                       nrow=1),
                           legend, nrow=2,heights=unit.c(unit(1, "npc") - lheight, lheight))
dev.off()

# ===
# Organizations -----------------
# ===

# Cargamos los datos del sentimiento propio generado
data.sentiment.self <- read.csv2("../../data/sentiment/organizations/2011Feuc2.txt", header = TRUE, sep = "|", dec = ".")
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/organizations/2011feusach2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/organizations/2011la_Fech2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/organizations/2012Feuc2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/organizations/2012feusach2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/organizations/2012la_Fech2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/organizations/2013Feuc2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/organizations/2013feusach2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../../data/sentiment/organizations/2013la_Fech2.txt", header = TRUE, sep = "|", dec = "."))

# Creamos el date
data.sentiment.self$date <- as.Date(paste(data.sentiment.self$year,"-",sprintf("%02d", data.sentiment.self$month),"-01",sep=""), format="%Y-%m-%d")

# federations <- c("Confech","Feuc","la_Fech","feusach")
federations <- c("Feuc","la_Fech","feusach")
election_date <- c(25,25,25)
j <- 0
# Ploteamos
for(i in federations){
    j <- j + 1
    data.federation <- data.sentiment.self[(data.sentiment.self$account == i),]
    data.federation.positive <- data.federation[,c("account", "date", "posemo")]
    data.federation.negative <- data.federation[,c("account", "date", "negemo")]
    colnames(data.federation.positive) <- c("account", "date", "value")
    colnames(data.federation.negative) <- c("account", "date", "value")
    data.federation.positive$type <- "positive"
    data.federation.negative$type <- "negative"
    data.federation.negative$value <- abs(data.federation.negative$value)
    data.federation <- rbind(data.federation.positive, data.federation.negative)
    data.federation$type <- factor(data.federation$type, levels = c("positive", "negative"))
    rm(data.federation.positive, data.federation.negative)
    
    p1 <- ggplot(data = data.federation, aes(x=date, y = value, group = type, color=type)) +
        xlab("") + ylab("Sentiment") + labs(colour = "Type") +
        geom_line() +
        stat_smooth() + 
        ggtitle(i) +
        theme(panel.background = element_rect(fill = "white")) +
        geom_vline( aes(xintercept = as.numeric( date[c(election_date[j])]) ), colour="black", linetype = "longdash") +        
        coord_cartesian(ylim=c(0.2,3.25 )) +
        #facet_wrap(~type, ncol = 1, scales = "free_y") +
        scale_color_manual(values=c("#56B4E9", "#D55E00"))
    assign(paste("plot.", gsub("@", "", i), sep = ""), p1)
}

g <- ggplotGrob(plot.Feuc + theme(legend.position="bottom"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)

# Guardamos
pdf("../../plots/sentiment_organizations.pdf",14,4)
plot.final <- grid.arrange(arrangeGrob(plot.feusach + theme(legend.position="none"),
                                       plot.Feuc + theme(legend.position="none"),
                                       plot.la_Fech + theme(legend.position="none"),
                                       nrow=1),
                           legend, nrow=2,heights=unit.c(unit(1, "npc") - lheight, lheight)) 
dev.off()
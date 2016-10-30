folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
library(ggplot2)
library(gridExtra)
library(grid)

# Cargamos los datos
data.organizations <- read.csv2("../../data/sentiment/organizations/2011Feuc2.txt", header = TRUE, sep = "|", dec = ".")
data.organizations <- rbind(data.organizations, read.csv2("../../data/sentiment/organizations/2011feusach2.txt", header = TRUE, sep = "|", dec = "."))
data.organizations <- rbind(data.organizations, read.csv2("../../data/sentiment/organizations/2011la_Fech2.txt", header = TRUE, sep = "|", dec = "."))
data.organizations <- rbind(data.organizations, read.csv2("../../data/sentiment/organizations/2012Feuc2.txt", header = TRUE, sep = "|", dec = "."))
data.organizations <- rbind(data.organizations, read.csv2("../../data/sentiment/organizations/2012feusach2.txt", header = TRUE, sep = "|", dec = "."))
data.organizations <- rbind(data.organizations, read.csv2("../../data/sentiment/organizations/2012la_Fech2.txt", header = TRUE, sep = "|", dec = "."))
data.organizations <- rbind(data.organizations, read.csv2("../../data/sentiment/organizations/2013Feuc2.txt", header = TRUE, sep = "|", dec = "."))
data.organizations <- rbind(data.organizations, read.csv2("../../data/sentiment/organizations/2013feusach2.txt", header = TRUE, sep = "|", dec = "."))
data.organizations <- rbind(data.organizations, read.csv2("../../data/sentiment/organizations/2013la_Fech2.txt", header = TRUE, sep = "|", dec = "."))
data.organizations$date <- as.Date(paste(data.organizations$year,"-",sprintf("%02d", data.organizations$month),"-01",sep=""), format="%Y-%m-%d")

# Cargamos los datos
data.leaders <- read.csv2("../../data/sentiment/leaders/2011camila_vallejo2.txt", header = TRUE, sep = "|", dec = ".")
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2012camila_vallejo2.txt", header = TRUE, sep = "|", dec = "."))
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2013camila_vallejo2.txt", header = TRUE, sep = "|", dec = "."))
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2011gabrielboric2.txt", header = TRUE, sep = "|", dec = "."))
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2012gabrielboric2.txt", header = TRUE, sep = "|", dec = "."))
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2013gabrielboric2.txt", header = TRUE, sep = "|", dec = "."))
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2011GiorgioJackson2.txt", header = TRUE, sep = "|", dec = "."))
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2012GiorgioJackson2.txt", header = TRUE, sep = "|", dec = "."))
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2013GiorgioJackson2.txt", header = TRUE, sep = "|", dec = "."))
data.leaders$date <- as.Date(paste(data.leaders$year,"-",sprintf("%02d", data.leaders$month),"-01",sep=""), format="%Y-%m-%d")

# Cambiamos el signo
data.leaders$negemo <- -(data.leaders$negemo)
data.organizations$negemo <- -(data.organizations$negemo)

# Analizamos del 2013 en adelante
data.leaders.2013 <- data.leaders[(data.leaders$year == 2013),]
data.orgs.2013 <- data.organizations[(data.organizations$year == 2013),]

# Sacamos las pendientes
table <- c()
for(i in sort(unique(data.leaders.2013$account)))
{
  lm.pos <- lm(posemo ~ month,data.leaders.2013[(data.leaders.2013$account==i),c(3,4)])
  lm.neg <- lm(negemo ~ month,data.leaders.2013[(data.leaders.2013$account==i),c(3,5)])
  
  pos.fun <- function(x){lm.pos$coefficients[1]+lm.pos$coefficients[2]*x}
  neg.fun <- function(x){lm.neg$coefficients[1]+lm.neg$coefficients[2]*x}
  
  row <- c()
  
  row$posemo.diff <- pos.fun(12)-pos.fun(1)
  #row$posemo.diff <- tail(data.leaders.2013[(data.leaders.2013$account==i),]$posemo, n=1) - data.leaders.2013[(data.leaders.2013$account==i),]$posemo[1]
  row$posemo.m <- lm.pos$coefficients[2]
  row$posemo.p <- pf(summary(lm.pos)$fstatistic[1], summary(lm.pos)$fstatistic[2], summary(lm.pos)$fstatistic[3], lower.tail=F) 
  
  row$negemo.diff <- neg.fun(12)-neg.fun(1)
  #row$negemo.diff <- tail(data.leaders.2013[(data.leaders.2013$account==i),]$negemo, n=1) - data.leaders.2013[(data.leaders.2013$account==i),]$negemo[1]
  row$negemo.m <- lm.neg$coefficients[2]
  row$negemo.p <- pf(summary(lm.neg)$fstatistic[1], summary(lm.neg)$fstatistic[2], summary(lm.neg)$fstatistic[3], lower.tail=F) 
  
  row <- as.data.frame(row)
  rownames(row) <- i
  table <- rbind(table, row)
}

for(i in sort(unique(data.orgs.2013$account)))
{
  lm.pos <- lm(posemo ~ month,data.orgs.2013[(data.orgs.2013$account==i),c(3,4)])
  lm.neg <- lm(negemo ~ month,data.orgs.2013[(data.orgs.2013$account==i),c(3,5)])
  
  pos.fun <- function(x){lm.pos$coefficients[1]+lm.pos$coefficients[2]*x}
  neg.fun <- function(x){lm.neg$coefficients[1]+lm.neg$coefficients[2]*x}
  
  row <- c()
  row$posemo.diff <- pos.fun(12)-pos.fun(1)
  #row$posemo.diff <- tail(data.orgs.2013[(data.orgs.2013$account==i),]$posemo, n=1) - data.orgs.2013[(data.orgs.2013$account==i),]$posemo[1]
  row$posemo.m <- lm.pos$coefficients[2]
  row$posemo.p <- pf(summary(lm.pos)$fstatistic[1], summary(lm.pos)$fstatistic[2], summary(lm.pos)$fstatistic[3], lower.tail=F) 
  
  row$negemo.diff <- neg.fun(12)-neg.fun(1)
  #row$negemo.diff <- tail(data.orgs.2013[(data.orgs.2013$account==i),]$negemo, n=1) - data.orgs.2013[(data.orgs.2013$account==i),]$negemo[1]
  row$negemo.m <- lm.neg$coefficients[2]
  row$negemo.p <- pf(summary(lm.neg)$fstatistic[1], summary(lm.neg)$fstatistic[2], summary(lm.neg)$fstatistic[3], lower.tail=F) 
  
  row <- as.data.frame(row)
  rownames(row) <- i
  table <- rbind(table, row)
}

l.camila <- lm(month~posemo,data.leaders.2013[(data.leaders.2013$account=="camila_vallejo"),c(3,4)])
l.giorgio <- lm(month~posemo,data.leaders.2013[(data.leaders.2013$account=="GiorgioJackson"),c(3,4)])
l.gabriel <- lm(month~posemo,data.leaders.2013[(data.leaders.2013$account=="gabrielboric"),c(3,4)])

summary(l.gabriel)


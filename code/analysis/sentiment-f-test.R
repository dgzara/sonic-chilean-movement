## Sentiments for paper
folder <- paste0(getwd(),"/code/analysis")
setwd(folder)
library(ggplot2)
library(gridExtra)
library(grid)
library(TSA)
library(forecast)

leaders <- c("camila_vallejo","gabrielboric","GiorgioJackson")
orgs <- c("Feuc","la_Fech","feusach")

# ===
# Leaders -----------------
# ===

# Cargamos los datos del sentimiento propio generado
data.leaders <- read.csv2("../../data/sentiment/leaders/2011camila_vallejo2.txt", header = TRUE, sep = "|", dec = ".", stringsAsFactors = FALSE)
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2012camila_vallejo2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2013camila_vallejo2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2011gabrielboric2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2012gabrielboric2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2013gabrielboric2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2011GiorgioJackson2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2012GiorgioJackson2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)
data.leaders <- rbind(data.leaders, read.csv2("../../data/sentiment/leaders/2013GiorgioJackson2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)

# Creamos el date
data.leaders$date <- as.Date(paste(data.leaders$year,"-",sprintf("%02d", data.leaders$month),"-01",sep=""), format="%Y-%m-%d")
data.leaders$diff <- data.leaders$posemo + data.leaders$negemo

# ===
# Organizations -----------------
# ===

# Cargamos los datos del sentimiento propio generado
data.orgs <- read.csv2("../../data/sentiment/organizations/2011Feuc2.txt", header = TRUE, sep = "|", dec = ".", stringsAsFactors = FALSE)
data.orgs <- rbind(data.orgs, read.csv2("../../data/sentiment/organizations/2011feusach2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)
data.orgs <- rbind(data.orgs, read.csv2("../../data/sentiment/organizations/2011la_Fech2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)
data.orgs <- rbind(data.orgs, read.csv2("../../data/sentiment/organizations/2012Feuc2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)
data.orgs <- rbind(data.orgs, read.csv2("../../data/sentiment/organizations/2012feusach2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)
data.orgs <- rbind(data.orgs, read.csv2("../../data/sentiment/organizations/2012la_Fech2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)
data.orgs <- rbind(data.orgs, read.csv2("../../data/sentiment/organizations/2013Feuc2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)
data.orgs <- rbind(data.orgs, read.csv2("../../data/sentiment/organizations/2013feusach2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)
data.orgs <- rbind(data.orgs, read.csv2("../../data/sentiment/organizations/2013la_Fech2.txt", header = TRUE, sep = "|", dec = "."), stringsAsFactors = FALSE)

# Creamos el date
data.orgs$date <- as.Date(paste(data.orgs$year,"-",sprintf("%02d", data.orgs$month),"-01",sep=""), format="%Y-%m-%d")
data.orgs$diff <- data.orgs$posemo - data.orgs$negemo

table <- c()
for(i in leaders){
  v.pos <- var.test(data.leaders[(data.leaders$account == i & data.leaders$year == 2012),]$posemo, data.leaders[(data.leaders$account == i & data.leaders$year == 2013),]$posemo)
  v.neg <- var.test(data.leaders[(data.leaders$account == i & data.leaders$year == 2012),]$negemo, data.leaders[(data.leaders$account == i & data.leaders$year == 2013),]$negemo)
  v.dif <- var.test(data.leaders[(data.leaders$account == i & data.leaders$year == 2012),]$diff, data.leaders[(data.leaders$account == i & data.leaders$year == 2013),]$diff)
  row <- c()
  row$account <- i
  row$pos.statistic <- v.pos$statistic
  row$pos.p.value <- round(v.pos$p.value,4)
  row$neg.statistic <- v.neg$statistic
  row$neg.p.value <- round(v.neg$p.value,4)
  row$dif.statistic <- v.dif$statistic
  row$dif.p.value <- round(v.dif$p.value,4)
  table <- rbind(table, data.frame(row, stringsAsFactors = FALSE))
}
for(i in orgs){
  v.pos <- var.test(data.orgs[(data.orgs$account == i & data.orgs$year == 2012),]$posemo, data.orgs[(data.orgs$account == i & data.orgs$year == 2013),]$posemo)
  v.neg <- var.test(data.orgs[(data.orgs$account == i & data.orgs$year == 2012),]$negemo, data.orgs[(data.orgs$account == i & data.orgs$year == 2013),]$negemo)
  v.dif <- var.test(data.orgs[(data.orgs$account == i & data.orgs$year == 2012),]$diff, data.orgs[(data.orgs$account == i & data.orgs$year == 2013),]$diff)
  row <- c()
  row$account <- i
  row$pos.statistic <- v.pos$statistic
  row$pos.p.value <- round(v.pos$p.value,4)
  row$neg.statistic <- v.neg$statistic
  row$neg.p.value <- round(v.neg$p.value,4)
  row$dif.statistic <- v.dif$statistic
  row$dif.p.value <- round(v.dif$p.value,4)
  table <- rbind(table, data.frame(row, stringsAsFactors = FALSE))
}

# Varianza 2
table2 <- c()
for(i in leaders){
  for(j in orgs){
    row <- c()
    row$leader <- i
    row$org <- j
    row$p.value <- round(var.test(data.leaders[(data.leaders$account == i),]$diff, data.orgs[(data.orgs$account == j),]$diff)$p.value, 4)
    table2 <- rbind(table2, data.frame(row, stringsAsFactors = FALSE))
  }
  
}
a <- letters[1:3]
table(a, sample(a)) 

table(table2[,c("leader","org")], table2$p.value)

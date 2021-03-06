install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("tidyverse")
library(tidyverse)
install.packages("readxl")
library(readxl)
install.packages("knitr")
library(knitr)
library(ggplot2)
install.packages("lubridate")
library(lubridate)
install.packages("plyr")
library(plyr)
library(dplyr)
wisudaBenar <- read_excel('C:/Users/Florecita/Documents/SKRIPSI/DaWisBenar.xlsx')
wisudaBenar <- wisudaBenar[complete.cases(wisudaBenar), ]
wisudaBenar %>% mutate(JalurMasuk = as.factor(JalurMasuk))
glimpse(wisudaBenar)
summary(wisudaBenar)
tranwisuda <- wisudaBenar %>% unite(tranwisuda, c("JalurMasuk", "AsalSekolah", "JenisKelamin", "Prodi", "LamaStudi", "Ipk"), sep = ",")
tranwisuda$mhsNpm <- NULL
write.csv(tranwisuda,"C:/Users/Florecita/Documents/SKRIPSI/market_basket.csv", quote = FALSE, row.names = FALSE)
cb <- read.transactions('C:/Users/Florecita/Documents/SKRIPSI/market_basket.csv', format = 'basket', sep = ',')
'trObj<-as(dataframe.dat,"tranw")'
cb
summary(cb)
if(!require("RColorBrewer")){ 
  install.packages("RColorBrewer") 
  library(RColorBrewer)
  }
itemFrequencyPlot(cb,topN=20,type = "relative",col=brewer.pal(8,'Pastel2'), main = "Relative Item Frequent Frequency Plot")
itemFrequencyPlot(cb,topN=20,type = "absolute",col=brewer.pal(8,'Pastel2'), main = "Absolute Item Frequent Frequency Plot")
library(arules)
association.rules <- apriori(cb, parameter = list(supp=0.001, conf=0.8, maxlen=10))
summary(association.rules)
inspect(association.rules[1:10])
shorter.association.rules <- apriori(tr, parameter =  list(supp=0.001, conf=0.8,maxlen=3))
subset.rules <- which(colSums(is.subset(association.rules, association.rules))>1)
length(subset.rules)
subset.association.rules. <- association.rules[-subset.rules]
subRules <- association.rules[quality(association.rules)$confidence>0.4]
plot(subRules)
plot(subRules,method="two-key plot")
plotly_arules(subRules)
top10subRules <- head(subRules, n = 6221, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")

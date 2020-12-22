library(arules)
library(arulesViz)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
retail <- read_excel('D:/Online_Retail.xls')
retail <- read_excel('D:/Online_Retail.xlsx')
retail <- retail[complete.cases(retail),]

retail %>% mutate(Description = as.factor(Description))
library(dlpyr)
install.packages("plyr")
library(plyr)
library(dplyr)
retail %>% mutate(Description = as.factor(Description))
retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
TransTime <- format(retail$InvoiceDate,"%H:%M:%S")
InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
cbind(retail,TransTime)
cbind(retail,InvoiceNo)
glimpse(retail)
transactionData <- ddply(retail,c("InvoiceNO","Date"), function(df1)paste(df1$Description, collapse = ","))
transactionData <- ddply(retail,c("InvoiceNo","Date"), function(df1)paste(df1$Description, collapse = ","))
transactionData
transactionData$InvoiceNo <- NULL
transactionData$Date <- NULL
colnames(transactionData) <- c("items")
transactionData
write.csv(transactionData, "D:/market_basket_transaction.csv", quote = FALSE, row.names = FALSE)
tr <- read.transactions('D:/market_basket_transactions.csv', format = 'basket', sep =',')
library(arules)
tr <- read.transactions('D:/market_basket_transactions.csv', format = 'basket', sep =',')
tr <- read.transactions('D:/market_basket_transaction.csv', format = 'basket', sep =',')
trObj<-as(dataframe.dat,"transactions")
'trObj<-as(dataframe.dat,"transactions")'
tr
summary(tr)
if(!require("RColorBrewer")){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
itemFrequencyPlot(tr,topN=20,type = "relative",col=brewer.pal(8,'Pastel2'), main = "Relative Item Frequent Frequency Plot")
itemFrequencyPlot(tr,topN=30,type = "relative",col=brewer.pal(8,'Pastel2'), main = "Relative Item Frequent Frequency Plot")
itemFrequencyPlot(tr,topN=10,type = "relative",col=brewer.pal(8,'Pastel2'), main = "Relative Item Frequent Frequency Plot")
itemFrequencyPlot(tr,topN=20,type = "relative",col=brewer.pal(8,'Pastel2'), main = "Relative Item Frequent Frequency Plot")
itemFrequencyPlot(tr,topN=5,type = "relative",col=brewer.pal(8,'Pastel2'), main = "Relative Item Frequent Frequency Plot")
itemFrequencyPlot(tr,topN=20,type = "relative",col=brewer.pal(8,'Pastel2'),main = "Relative Item Frequent Frequency Plot")

association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8, maxlen=10))
library(arules)
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8, maxlen=10))

association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8, maxlen=10))

summary(association.rules)
inspect(association.rules[1:10])  
shorter.association.rules <- apriori(tr, parameter =  list(supp=0.001, conf=0.8,maxlen=3))
subset.rules <- which(colSums(is.subset(association.rules, association.rules))>1)
length(subset.rules)
subset.association.rules. <- association.rules[-subset.rules]
metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(default="lhs",rhs="METAL"))
inspect(head(metal.association.rules))
subRules <- association.rules[quality(association.rules)$confidence>0.4]
plot(subRules)
library(arulesViz)

#rm(list=ls(all=TRUE))
#setwd('f:/ML/assignment-1-lokeshy/src/')
#source('common.r')

names(bankdata)
str(bankdata)

#### Take numerical attr out #### 

bankdataNum = subset(bankdata , select = c(Age,Income,CCAvg,Mortgage))

cor(bankdataNum[,-1])
summary(bankdataNum)

apply(bankdataNum , 2 , mean)
apply(bankdataNum , 2 , var)

### Normalize values 
library(vegan)
bankdataNum = decostand(bankdataNum,method = "range")

apply(bankdataNum , 2 , mean)
apply(bankdataNum , 2 , var)

pca_data = princomp(bankdataNum)
summary(pca_data)
plot(pca_data)
print(pca_data)
pca_data$loadings[,]
data2<-data.frame(pca_data$scores[,1:4])

cor(bankdataNum)
pca_data <- prcomp(bankdataNum)
summary(pca_data)
plot(pca_data)
print(pca_data)
biplot (pca_data , scale =0)

bankdata = cbind(bankdata ,data2)



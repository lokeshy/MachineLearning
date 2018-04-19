rm(list=ls(all=TRUE))

setwd('f:/ML/assignment-1-lokeshy/src/')
source('common.r')

str(bankdata)
library(class)
set.seed(245)

train_RowIDs = sample(1:nrow(bankdata), nrow(bankdata)*0.6)
train = bankdata[train_RowIDs,]
test = bankdata[-train_RowIDs,]

pred=knn(train = train, test = test, train$PersonalLoan, k = 1)



buildConfusionMatrix(test$PersonalLoan , pred)

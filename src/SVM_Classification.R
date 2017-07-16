rm(list=ls(all=TRUE))

setwd('f:/ML/assignment-1-lokeshy/src/')
source('common.r')


# Load required libraries
library(vegan)
library(dummies)
library(e1071)
str(bankdata)


### Convert Factor to numeric ########

edu = dummy(bankdata$Education)
bankdata$Education = NULL

str(bankdata)

bankdata = cbind(bankdata, edu)

str(bankdata)

set.seed(123)

train_RowIDs = sample(1:nrow(bankdata), nrow(bankdata)*0.6)
train = bankdata[train_RowIDs,]
test = bankdata[-train_RowIDs,]
ind_Attr = setdiff(names(bankdata),'PersonalLoan')

model = svm(x = train[,ind_Attr], 
            y = train$PersonalLoan, 
            type = "C-classification", 
            kernel = "linear", cost = 10, gamma = 0.1) 

summary(model)

# Predict on train data  
pred_Train  =  predict(model, train[,ind_Attr])  

buildConfusionMatrix(train$PersonalLoan , pred_Train)

#predict on test
pred_Test = predict(model, test[,ind_Attr]) 
buildConfusionMatrix(test$PersonalLoan , pred_Test)








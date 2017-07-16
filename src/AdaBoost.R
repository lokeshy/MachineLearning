rm(list=ls(all=TRUE))

setwd('f:/ML/assignment-1-lokeshy/src/')
source('common.r')
str(bankdata)
library(vegan)
library(dummies)
library(ada) 

### convert cat attr to num #####

edu = dummy(bankdata$Education)

bankdata$Education = NULL 

bankdata = cbind(bankdata , edu)
str(bankdata)

set.seed(245)

train_RowIDs = sample(1:nrow(bankdata), nrow(bankdata)*0.6)
train = bankdata[train_RowIDs,]
test = bankdata[-train_RowIDs,]

ind_Attr = setdiff(colnames(bankdata),'PersonalLoan')

# Build best ada boost model 
model = ada(x = train[,ind_Attr], 
            y = train$PersonalLoan, 
            iter=20, loss="logistic") # 20 Iterations 
# Look at the model summary
model
summary(model)

# Predict on train data  
pred_Train  =  predict(model, train[,ind_Attr])  

buildConfusionMatrix(train$PersonalLoan, pred_Train)

# Predict on test data
pred_Test = predict(model, test[,ind_Attr]) 
table(pred_Test)
table(test$PersonalLoan)

buildConfusionMatrix(test$PersonalLoan, pred_Test)







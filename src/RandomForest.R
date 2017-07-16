rm(list=ls(all=TRUE))
setwd('f:/ML/assignment-1-lokeshy/src/')
source('common.r')


detectOutliers(bankdata , bankdata$CCAvg)
detectOutliers(bankdata , bankdata$Income)
detectOutliers(bankdata , bankdata$Age)



### divide test and train  60 , 40######
set.seed(5656)
dt = sort(sample(nrow(bankdata), nrow(bankdata)*.6))
train = bankdata[dt,]
test = bankdata[-dt,]
rm(dt)



library(randomForest)

rfModel = randomForest(PersonalLoan~.,
                         data=train, 
                         keep.forest=TRUE,ntree=50) 

print(rfModel)

pred_Train = predict(rfModel, 
                     test[,setdiff(names(test), "PersonalLoan")],
                     type="response", 
                     norm.votes=TRUE)

buildConfusionMatrix(test$PersonalLoan , pred_Train)


rfModel$importance 
round(importance(rfModel), 2)
varImpPlot(rfModel)

# Extract and store important variables obtained from the random forest model
rf_Imp_Attr = data.frame(rfModel$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr = rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]

########## Build RF with top imp Attribute ##########

top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:7])

model_Imp = randomForest(PersonalLoan~.,
                         data=train[,c(top_Imp_Attr,"PersonalLoan")], 
                         keep.forest=TRUE,ntree=50) 

print(model_Imp)

pred_ImpModel = predict(model_Imp, 
                     test[,setdiff(names(test), "PersonalLoan")],
                     type="response", 
                     norm.votes=TRUE)
buildConfusionMatrix(test$PersonalLoan , pred_ImpModel)


########## build Random Forset using sub sampling #############

modelLatest = randomForest(PersonalLoan ~ ., data=train, 
                     keep.forest=TRUE, ntree=200 , mtry=2,cutoff=c(.55,.45) , sampsize = c('0' = 100 , '1' =55), keep.inbag = TRUE ) 

pred_Model = predict(modelLatest, 
                        test[,setdiff(names(test), "PersonalLoan")],
                        type="response", 
                        norm.votes=TRUE)

buildConfusionMatrix(test$PersonalLoan , pred_Model)


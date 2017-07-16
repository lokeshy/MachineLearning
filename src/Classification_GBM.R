rm(list=ls(all=TRUE))
setwd('f:/ML/assignment-1-lokeshy/src/')
source('common.r')
str(bankdata)

#### Start H20 #######
library(h2o)
h2o.init(ip='localhost', port = 54321, max_mem_size = '1g')
### divide test and train  60 , 40######
set.seed(564)
dt = sort(sample(nrow(bankdata), nrow(bankdata)*.6))
train = bankdata[dt,]
test = bankdata[-dt,]
rm(dt)


train.hex <- as.h2o(x = train, destination_frame = "train.hex")

gbm <- h2o.gbm(model_id = "GBM.hex", ntrees = 125, 
               learn_rate=0.01, max_depth = 10,   
               y = "PersonalLoan", x = setdiff(names(train.hex), "PersonalLoan"),
               training_frame = train.hex)

# Get the auc of the GBM model
h2o.auc(gbm)

# Examine the performance of the best model
gbm
# Important Variables.
h2o.varimp(gbm)

# Import a local R test data frame to the H2O cloud
testData.hex <- as.h2o(x = test, destination_frame = "testData.hex")
# Predict on same training data set
predict.hex = h2o.predict(gbm,newdata = testData.hex[,setdiff(names(testData.hex), "PersonalLoan")])
data_GBM = h2o.cbind(testData.hex[,"PersonalLoan"], predict.hex)
# Copy predictions from H2O to R
pred_GBM = as.data.frame(data_GBM)

buildConfusionMatrix(pred_GBM$PersonalLoan , pred_GBM$predict)




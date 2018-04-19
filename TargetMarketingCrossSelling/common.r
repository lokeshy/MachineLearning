#### file with common utility function and operations ###

DetectOutliers = function(data_frame, feature) {
  col_name = eval(substitute(feature),eval(data_frame))
  tot = sum(!is.na(col_name))
  na1 = sum(is.na(col_name))
  mean_actual = mean(col_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(col_name, main="With outliers")
  hist(col_name, main="With outliers", xlab=NA, ylab=NA)
  outlier = boxplot.stats(col_name)$out
  mo = mean(outlier)
  col_name = ifelse(col_name %in% outlier, NA, col_name)
  boxplot(col_name, main="Without outliers")
  hist(col_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 = sum(is.na(col_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  mean_nil_outlier = mean(col_name, na.rm = T)
  message("Mean without removing outliers: ", mean_actual)
  message("Mean if we remove outliers: ", mean_nil_outlier)
}

CheckClassImbalance = function(target){
  class_Table = table(target)
  df = as.data.frame(class_Table)
  for(i in  1:nrow(df)){
    row <- df[i,]
    message("class : " , row[1] , " freq : " , row[2] , " percentage : " , round((row[2]/length(target))*100,4) ,"%")
  }
  return(class_Table) 
}

BuildConfusionMatrix = function(actualData, prediction){
  conf_matrix = table(actualData,prediction)
  print(conf_matrix)
  acc = sum(diag(conf_matrix))/ sum(conf_matrix)
  rec = (conf_matrix[2,2])/(conf_matrix[2,1]+conf_matrix[2,2])
  pre = conf_matrix[2,2]/sum(conf_matrix[,2])
  print(c("accuracy"=acc,"recall"=rec,"precision"=pre))
}

MeanCustomerReturnDays = function(avg,custmerIds,dataFrame){
  count = seq(1,length(custmerIds))
  i =1
  Mean.Days = c()
  for(i in count){
    curCustomer = dataFrame[dataFrame[,1] == custmerIds[i] ,]
    customerMean = avg[[i]]
    curCustomerAvg = rep(customerMean, nrow(curCustomer))
    Mean.Days = append(Mean.Days,curCustomerAvg)
  }
  return(Mean.Days)
}

NumberOfDaysBetweenCalls = function(dataFrame,custmerIds){
  NextCallDays =  numeric()
  count = seq(1,length(custmerIds))
  i =1
  for(i in count){
    curCustomer = dataFrame[dataFrame[,1] == custmerIds[i] ,]
    currCallDate = curCustomer$Call.Date
    prevCallDate = c(curCustomer$Call.Date[1],head(curCustomer$Call.Date,length(curCustomer$Call.Date)-1))
    dateDiff = as.numeric(as.Date(currCallDate) - as.Date(prevCallDate))
    NextCallDays = append(NextCallDays,dateDiff)
  }
  return (NextCallDays)
}

ChurnPercentageStateWise = function(states,dataFrame){
  count = seq(1,length(states))
  i =1
  churn.percentage = c()
  for(i in count){
    currState = dataFrame[dataFrame$State == states[i] ,]
    stateWiseChurn = ceiling((nrow(currState[currState$Churn == TRUE,])/nrow(currState))*100)/100;
    stateChurn = rep(stateWiseChurn, nrow(currState))
    churn.percentage = append(churn.percentage,stateChurn)
    
  }
  
  return(churn.percentage)
}

GetTestRows= function(dataFrame,custmerIds){
  
  count = seq(1,length(custmerIds))
  i =1
  testRows = c()
  for(i in count)
  {
    
    curCustomer = dataFrame[dataFrame$Customer.ID== custmerIds[i] ,]
    
    testRows = append(testRows, max(as.numeric(rownames(curCustomer)))) 
    
  }
  return(as.character(testRows))

}



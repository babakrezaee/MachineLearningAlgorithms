
print("This function recieves 2 elements: data and formula; Set the inputs accordingly")

doCVlogit<-function(data,formula){
  
  #Randomly shuffle the data
  data<-data[sample(nrow(data)),]
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
  
  fitted_values=rep(0,nrow(data))
  #Perform 10 fold cross validation
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- data[testIndexes, ]
    trainData <- data[-testIndexes, ]
    fm=glm(formula,data=trainData,family = "binomial")
    fitted_values[testIndexes]<-predict(fm, testData,, type="response")
    #Use test and train data partitions however you desire...
  }
  
  return(fitted_values)
}

results=doCVlogit(cd_1,as.formula(Violent1~.))


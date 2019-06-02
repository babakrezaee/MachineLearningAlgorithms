
print("This function recieves 3 elements: formula, data, and k (number of folds;default is 10); Set the inputs accordingly")

doCVlogit<-function(formula, data, k=10){
  
  #a little error checking
  if(!(is.data.frame(data))) {cat('error in docv: "data" is not a  data frame\n'); return(0)}
  if(!(is.formula(formula))) {cat('error in docv: "formula" is not a vector\n'); return(0)}
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if(!(is.wholenumber(k) | k==1)) {cat('error in docv: k is not an integer larger than 1\n'); return(0)}

  #Randomly shuffle the data
  data<-data[sample(nrow(data)),]
  #Create k equally size folds
  folds <- cut(seq(1,nrow(data)),breaks=k,labels=FALSE)
  
  fitted_values=rep(0,nrow(data))
  #Perform k fold cross validation
  for(i in 1:k){
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




print("This function recieves 3 elements: formula, data, and k (number of folds;default is 10); Set the inputs accordingly")

doCVlogit<-function(Formula, Data, k=10){
  
  #a little error checking
  if(!(is.data.frame(Data))) {cat('error in docv: "Data" is not a  data frame type\n'); return(0)}
  if(!(is.formula(Formula))) {cat('error in docv: "Formula" is not a formula type\n'); return(0)}
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if(!(is.wholenumber(k) | k==1)) {cat('error in docv: k is not an integer larger than 1\n'); return(0)}

  #Randomly shuffle the data
  Data<-Data[sample(nrow(Data)),]
  #Create k equally size folds
  folds <- cut(seq(1,nrow(Data)),breaks=k,labels=FALSE)
  
  fitted_values=rep(0,nrow(Data))
  #Perform k fold cross validation
  for(i in 1:k){
    #Segement your Data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- Data[testIndexes, ]
    trainData <- Data[-testIndexes, ]
    fm<-glm(Formula,Data=trainData,family = "binomial")
    #fitted_values[as.numeric(rownames(testData))]<-predict(fm, testData,, type="response")
    
  }
  
  return(fm)
}



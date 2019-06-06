###########################################################
############################################################
## Function to do cross validation.
## docv is a general method that takes a prediction function as an argument.
## docvknn is for kNN, it calls docv handing in a wrapper of kknn.
############################################################
############################################################

print("This function recieves 3 elements: formula, data, number of folds(default is 10), k of KNN (default is 20), and kernel (default is rectangular) ; Set the inputs accordingly")

doCVknnClassification<-function(formula, Data, nfolds=10,K=20, KKNkernel="rectangular"){
  
  #a little error checking
  if(!(is.data.frame(Data))) {cat('error in docv: "Data" is not a  data frame type\n'); return(0)}
  if(!(is.formula(formula))) {cat('error in docv: "formula" is not a formula type\n'); return(0)}
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if(!(is.wholenumber(nfolds) | nfolds==1)) {cat('error in docv: nfolds is not an integer larger than 1\n'); return(0)}

  #Randomly shuffle the data
  Data<-Data[sample(nrow(Data)),]
  #Create nfolds equally size folds
  folds <- cut(seq(1,nrow(Data)),breaks=nfolds,labels=FALSE)
  
  fitted_values=rep(0,nrow(Data))
  #Perform nfolds fold cross validation
  pb <- txtProgressBar(0, length(x), style = 3)
  for(i in 1:nfolds){
    #Segement your Data by fold using the which() function 
    setTxtProgressBar(pb, i)
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- Data[testIndexes, ]
    trainData <- Data[-testIndexes, ]
    fm<-kknn(formula, train=trainData, test=testData, k=K, kernel=KKNkernel)
    fitted_values[as.numeric(rownames(testData))]<-fm$prob[,2]
  }
  
  return(fitted_values)
}

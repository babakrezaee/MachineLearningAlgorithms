# read in data
smsRaw = read.csv("http://www.rob-mcculloch.org/data/sms_spam.csv", stringsAsFactors = FALSE)

#Set the type of text as factor
smsRaw$type=factor(smsRaw$type)

library(tm)
smsC = VCorpus(VectorSource(smsRaw$text))

smsDtm <- DocumentTermMatrix(smsC, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

trainfrac=.75
n= length(smsRaw$type)
nTrain=floor(trainfrac*n)
set.seed(99)

J=2
laplace_seq=seq(0, 2, by = .25) 
##predefining the size of list can speed up stroing the results
time_taken<-matrix(0, length(laplace_seq), J)   
misclass<-matrix(0, length(laplace_seq), J)   
perspam<-matrix(0, length(laplace_seq), J)  


library(e1071)
k=1 # This is just for makijg progress bar
for (j in seq(1, J, by = 1) ) {
  
  
  ##Now randomly divide the sample into train and test sub-samples:
  #Getting random indexes
  ii=sample(1:n,nTrain)
  
  #Using the random indices to decompose the sample to Train, Trainy, Test, and Testy (sub)-samples
  smsTrain=smsDtm[ii,]
  smsTest=smsDtm[-ii,]
  
  smsTrainy=smsRaw[ii,]$type
  smsTesty=smsRaw[-ii,]$type
  
  #Drop the words with less than 5 frequency in DTM
  
  smsFreqWords=findFreqTerms(smsTrain, 5) #words that appear at leat 5 times
  smsFreqTrain=smsTrain[,smsFreqWords]
  smsFreqTest=smsTest[,smsFreqWords]
  
  convertCounts <- function(x) {
    x <- ifelse(x > 0, "Yes", "No")
  }
  
  smsTrain=apply(smsFreqTrain, MARGIN = 2,convertCounts)
  smsTest=apply(smsFreqTest,MARGIN = 2, convertCounts)
  
  
  ###
  pb = txtProgressBar(min = 0, max = length(laplace_seq)*J, initial = 0) 
  ###
  
  i=1
  for (l in laplace_seq) {
    smsNB=naiveBayes(smsTrain, smsTrainy, laplace = l)
    start_time <- Sys.time()
    
    yhat=predict(smsNB,smsTest)
    
    end_time <- Sys.time()
    time_taken[i,j] <- end_time - start_time
    
    
    ctab=table(yhat, smsTesty)
    ctab
    misclass[i,j] =((sum(ctab)-sum(diag(ctab)))/sum(ctab))*100
    
    perspam[i,j] = 100-(ctab[2,2]/sum(ctab[,2]))*100
    
    #cat("For sample #=",j," Laplace=",l,"misclass,perspam,time: ", misclass[i,j],perspam[i,j],round(time_taken[i,j],digits=0), "second", "\n")
    cat(setTxtProgressBar(pb,round((k/(length(laplace_seq)*J))*100,digits = 0)), "%||",j, "out of", J, "random samples | Laplace=",l, "\n")
    i=i+1
    k=k+1
  }
  
}


Results_Missclass = data.frame(Laplace=laplace_seq, Mean=rowMeans(misclass), SD=apply(misclass, 1, sd))
Results_perspam = data.frame(Laplace=laplace_seq, Means=rowMeans(perspam), SD=apply(perspam, 1, sd))
Results_time = data.frame(Laplace=laplace_seq, Means=rowMeans(time_taken), SD=apply(time_taken, 1, sd))



####################################Total misclassification#####################
##Plotting the results
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(laplace_seq, Results_Missclass$Mean, pch=16, axes=FALSE, xlab="", ylab="", 
     type="b",col="black", main="Total misclassification(%)")
axis(2, ylim=c(min(Results_Missclass$Mean),max(Results_Missclass$Mean)),col="black",las=1)  ## las=1 makes horizontal labels
mtext("Mean",side=2,line=2.5)
box()

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(laplace_seq, Results_Missclass$SD, pch=15,  xlab="", ylab="", 
     axes=FALSE, type="b", col="red")
## a little farther out (line=4) to make room for labels
mtext("SD",side=4,col="red",line=4) 
axis(4,  ylim=c(min(Results_Missclass$SD),max(Results_Missclass$SD)), col="red",col.axis="red",las=1)

## Draw the laplace axis
axis(1,pretty(range(laplace_seq),5))
#Mark the lowest level of misclassification
abline(v=Results_Missclass$Laplace[Results_Missclass$Mean == min(Results_Missclass$Mean)], col="blue")
mtext("Laplace",side=1,col="black",line=2.5)  


## Add Legend
legend("bottomright",legend=c("Mean","SD"),
       text.col=c("black","red"),pch=c(16,15),col=c("black","red"))


####################################Spam misclassification#####################

##Plotting the results
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(laplace_seq, Results_perspam$Mean, pch=16, axes=FALSE,  xlab="", ylab="", 
     type="b",col="black", main="Spam misclassification(%)")
axis(2, ylim=c(min(Results_perspam$Mean),max(Results_perspam$Mean)),col="black",las=1)  ## las=1 makes horizontal labels
mtext("Mean",side=2,line=2.5)
box()

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(laplace_seq, Results_perspam$SD, pch=15,  xlab="", ylab="",
     axes=FALSE, type="b", col="red")
## a little farther out (line=4) to make room for labels
mtext("SD",side=4,col="red",line=4) 
axis(4, ylim=c(min(Results_perspam$SD),max(Results_perspam$SD)), col="red",col.axis="red",las=1)

## Draw the laplace axis
axis(1,pretty(range(laplace_seq),5))
#Mark the lowest level of misclassification
abline(v=Results_perspam$Laplace[Results_perspam$Mean == min(Results_perspam$Mean)], col="blue")
mtext("Laplace",side=1,col="black",line=2.5)  


legend("bottomright",legend=c("Mean","SD"),
       text.col=c("black","red"),pch=c(16,15),col=c("black","red"))

####################################Time#####################


##Plotting the results
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(laplace_seq, Results_time$Mean, pch=16, axes=FALSE,  xlab="", ylab="", 
     type="b",col="black", main="Computation time (Seconds)")
axis(2, ylim=c(min(Results_time$Mean),max(Results_time$Mean)),col="black",las=1)  ## las=1 makes horizontal labels
mtext("Mean",side=2,line=2.5)
box()

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(laplace_seq, Results_time$SD, pch=15,  xlab="", ylab="",
     axes=FALSE, type="b", col="red")
## a little farther out (line=4) to make room for labels
mtext("SD",side=4,col="red",line=4) 
axis(4, ylim=c(min(Results_time$SD),max(Results_time$SD)), col="red",col.axis="red",las=1)

## Draw the laplace axis
axis(1,pretty(range(laplace_seq),5))
#Mark the lowest level of misclassification
abline(v=Results_perspam$Laplace[Results_time$Mean == min(Results_time$Mean)], col="blue")
mtext("Laplace",side=1,col="black",line=2.5)  


legend("bottomleft",legend=c("Mean","SD"),
       text.col=c("black","red"),pch=c(16,15),col=c("black","red"))


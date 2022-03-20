library('rpart')
library('caret')
library('dplyr')
# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  warning("USAGE: Rscript hw3_110753165.R --fold 5 --input inputpath --output outputpath")
}
# parse parameters
foldnumber <-NA
in_f <- NA
out_f <- NA

i<-1 
while(i < length(args))
{
  if(args[i] == "--fold"){
    foldnumber<-args[i+1]
    i<-i+1
  }else if(args[i] == "--input"){
    in_f<-args[i+1]
    i<-i+1
  }else if(args[i] == "--output"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}
##deal missing tag
if(is.na(foldnumber)){
  stop("--fold tag is missing", call.=FALSE)
}
if(is.na(in_f)){
  stop("miss --input filename", call.=FALSE)
}
if(is.na(out_f)){
  stop("miss --output filename", call.=FALSE)
}

#print(foldnumber)
#print(in_f)
#print(out_f)

df <- read.csv(in_f, header = F)
#head(df)

#print(df[1, ])
#colnames(df)[colSums(is.na(df)) > 0]
#print(nrow(df))
#print(ncol(df))

##deal NA value column
df<-df[ ,apply(df, 2, function(x) !any(is.na(x)))]
foldnumber <- as.numeric(foldnumber)

##use caret split data
set.seed(1)
splitfold <- createFolds(df$V2, k = foldnumber)

set<-c()
train<-c()
validat<-c()
test<-c()

##use while and for calcuate each target fold


flag <- 1
#while(flag<=foldnumber)
while(flag<=foldnumber){
  empty_train <- data.frame()
  for (i in c(1:foldnumber)){
    if(flag!=foldnumber){
      if(i==flag){
        testing <-df[as.numeric(unlist(splitfold[i])),]
      }else if(i==flag+1){
        validation <-df[as.numeric(unlist(splitfold[i])),]
      }else{
        empty_train<-rbind(empty_train, df[as.numeric(unlist(splitfold[i])),])
      }
    }else{
      if(i==flag){
        testing <-df[as.numeric(unlist(splitfold[i])),]
      }else if(i==1){
        validation <-df[as.numeric(unlist(splitfold[i])),]
      }else{
        empty_train<-rbind(empty_train, df[as.numeric(unlist(splitfold[i])),])
      }
    }
  }
  #head(empty_train)
  setname<-paste('fold',as.character(flag),sep='')
  print(setname)
  set<-append(set,setname)
  model <- rpart(V2 ~ .,data=empty_train, control=rpart.control(maxdepth=4),method="class")
  
  
  resultframe <- data.frame(truth=empty_train$V2,pred=predict(model,empty_train,type="class"))
  traintab <- table(resultframe)
  trainvalue<-round(sum(diag(traintab))/sum(traintab),2)
  #print("train accuracy:")
  #print(trainvalue)
  train<-append(train,trainvalue)
  
  #print(validation$V2)
  resultframe2 <- data.frame(truth=validation$V2,pred=predict(model,validation,type="class"))
  validtab <- table(resultframe2)
  validvalue<-round(sum(diag(validtab))/sum(validtab),2)
  #print("valid accuracy:")
  #print(validvalue)
  validat<-append(validat,validvalue)
  
  resultframe3 <- data.frame(truth=testing$V2,pred=predict(model,testing,type="class"))
  testtab <- table(resultframe3)
  testvalue<-round(sum(diag(testtab))/sum(testtab),2)
  #print("test accuracy:")
  #print(testvalue)
  test<-append(test,testvalue)
  
  
  flag<-flag+1
}
#print("final without avg")
#print(set)
#print(train)
#print(test)
#print(validat)
set<-append(set,'ave.')
train<-append(train,round(mean(train),2))
test<-append(test,round(mean(test),2))
validat<-append(validat,round(mean(validat),2))
print("final")
print(set)
print(train)
print(test)
print(validat)

finaldata <- data.frame(set=set,training=train,validation=validat,test=test)
print(finaldata)
write.csv(finaldata,out_f, row.names = FALSE,quote=F)







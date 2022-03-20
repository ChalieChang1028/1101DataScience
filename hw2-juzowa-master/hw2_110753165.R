sens_func<-function(data,badthre,query_m){
  TP <- 0
  FN <- 0
  FP <- 0
  TN <- 0
  if(query_m=="bad"){
    for(i in 1:nrow(data)){
      if((data[i,3]>badthre)&&(data[i,2]==query_m)){
        TP<-TP+1
      }else if((data[i,3]<badthre)&&(data[i,2]==query_m)){
        FN<-FN+1
      }else if((data[i,3]>badthre)&&(data[i,2]!=query_m)){
        FP<-FP+1
      }else{
        TN<-TN+1
      }
    }
  }else{
    for(i in 1:nrow(data)){
      if((data[i,3]>badthre)&&(data[i,2]==query_m)){
        FN<-FN+1
      }else if((data[i,3]<badthre)&&(data[i,2]==query_m)){
        TP<-TP+1
      }else if((data[i,3]>badthre)&&(data[i,2]!=query_m)){
        TN<-TN+1
      }else{
        FP<-FP+1
      }
    }
  }
  return(round(TP/(TP+FN),2))
}

spec_func<-function(data,badthre,query_m){
  TP <- 0
  FN <- 0
  FP <- 0
  TN <- 0
  if(query_m=="bad"){
    for(i in 1:nrow(data)){
      if((data[i,3]>badthre)&&(data[i,2]==query_m)){
        TP<-TP+1
      }else if((data[i,3]<badthre)&&(data[i,2]==query_m)){
        FN<-FN+1
      }else if((data[i,3]>badthre)&&(data[i,2]!=query_m)){
        FP<-FP+1
      }else{
        TN<-TN+1
      }
    }
  }else{
    for(i in 1:nrow(data)){
      if((data[i,3]>badthre)&&(data[i,2]==query_m)){
        FN<-FN+1
      }else if((data[i,3]<badthre)&&(data[i,2]==query_m)){
        TP<-TP+1
      }else if((data[i,3]>badthre)&&(data[i,2]!=query_m)){
        TN<-TN+1
      }else{
        FP<-FP+1
      }
    }
  }

  return(round(TN/(TN+FP),2))
}

F1_func<-function(data,badthre,query_m){
  TP <- 0
  FN <- 0
  FP <- 0
  TN <- 0
  if(query_m=="bad"){
    for(i in 1:nrow(data)){
      if((data[i,3]>badthre)&&(data[i,2]==query_m)){
        TP<-TP+1
      }else if((data[i,3]<badthre)&&(data[i,2]==query_m)){
        FN<-FN+1
      }else if((data[i,3]>badthre)&&(data[i,2]!=query_m)){
        FP<-FP+1
      }else{
        TN<-TN+1
      }
    }
  }else{
    for(i in 1:nrow(data)){
      if((data[i,3]>badthre)&&(data[i,2]==query_m)){
        FN<-FN+1
      }else if((data[i,3]<badthre)&&(data[i,2]==query_m)){
        TP<-TP+1
      }else if((data[i,3]>badthre)&&(data[i,2]!=query_m)){
        TN<-TN+1
      }else{
        FP<-FP+1
      }
    }
  }
  #2*precision*recall/(precision+recall)
  precision<-TP/(TP+FP)
  recall<-TP/(TP+FN)
  return(round(2*precision*recall/(precision+recall),2))
}




# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  warning("USAGE: Rscript hw2_yourID.R --target bad/good --badthre <threshold> --input meth1 meth2 ... methx --output result.csv")
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    query_m<-args[i+1]
    i<-i+1
  }else if(args[i] == "--badthre"){
    badthre <-args[i+1]
    i<-i+1
  }else if(args[i] == "--input"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--output"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    warning("Unknown flag ", args[i])
  }
  i<-i+1
}

#print("PROCESS")
#print(paste("query mode :", query_m))
#print(paste("output file:", out_f))
#print(paste("files      :", files))
#print(paste("badthre :", badthre))
badthre <- as.numeric(badthre)

# read files
method<-c()
sensitivity<-c()
specificity<-c()
F1<-c()
logLikelihood<-c()
pseudoRsquared <- c()
for(file in files)
{
  methodname<-gsub(".csv", "", basename(file))
  method<-append(method,methodname)
  #print(method)
  d<-read.table(file, header=T,sep=",")
  
  #print("sensitivity:")
  #print(sens_func(d,badthre,query_m))
  sensitivity<-append(sensitivity,sens_func(d,badthre,query_m))
  
  #print("specificity:")
  #print(spec_func(d,badthre,query_m))
  specificity<-append(specificity,spec_func(d,badthre,query_m))
  
  #print("F1:")
  #print(F1_func(d,badthre,query_m))
  F1<-append(F1,F1_func(d,badthre,query_m))
  
  #print("logLikelihood:")
  logvalue<-sum(ifelse(d$reference=='bad',log(d$pred.score),log(1-d$pred.score)))
  #print(round(logvalue,2))
  logLikelihood<-append(logLikelihood,round(logvalue,2))
  
  #print("pseudoRsquared:")
  pNull <- sum(ifelse(d$reference=='bad',1,0))/dim(d)[[1]]
  Nullvalue<-sum(ifelse(d$reference=='bad',1,0))*log(pNull) +sum(ifelse(d$reference=='bad',0,1))*log(1-pNull)
  pseudovalue<-1-logvalue/Nullvalue
  #print(round(pseudovalue,2))
  pseudoRsquared<-append(pseudoRsquared,round(pseudovalue,2))
  
  
}


#print("=============FINAL=============")
#print(method)
#print(sensitivity)
#print(specificity)
#print(F1)
#print(logLikelihood)
#print(pseudoRsquared) 


method<-append(method,"max")
#print(method)

sensitivity<-append(sensitivity,method[which.max(sensitivity)])
#print(sensitivity)
specificity<-append(specificity,method[which.max(specificity)])
#print(specificity)
F1<-append(F1,method[which.max(F1)])
#print(F1)
logLikelihood<-append(logLikelihood,method[which.max(logLikelihood)])
#print(logLikelihood)
pseudoRsquared<-append(pseudoRsquared,method[which.max(pseudoRsquared)])
#print(pseudoRsquared)


df <- data.frame(method=method,sensitivity=sensitivity,specificity=specificity,F1=F1,logLikelihood=logLikelihood,pseudoRsquared=pseudoRsquared)
write.csv(df,out_f, row.names = FALSE,quote=F)

#write.table(out_data, file=out_f, row.names = F, quote = F)

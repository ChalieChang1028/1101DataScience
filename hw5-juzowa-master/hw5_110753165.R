library('rpart')
library('caret')
library('dplyr')
library('randomForest')

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)<10) {
  warning("USAGE: Rscript hw5_110753165.R --fold n --train Titanic_Data/train.csv --test Titanic_Data/test.csv --report performance.csv --predict predict.csv
")
}
# parse parameters
foldnumber <-NA
train_f <- NA
test_f <- NA
report_f<-NA
predict_f<-NA

i<-1 
while(i < length(args))
{
  if(args[i] == "--fold"){
    foldnumber<-args[i+1]
    i<-i+1
  }else if(args[i] == "--train"){
    train_f<-args[i+1]
    i<-i+1
  }else if(args[i] == "--test"){
    test_f<-args[i+1]
    i<-i+1
  }else if(args[i] == "--report"){
    report_f<-args[i+1]
    i<-i+1
  }else if(args[i] == "--predict"){
    predict_f<-args[i+1]
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
if(is.na(train_f)){
  stop("miss --train filename", call.=FALSE)
}
if(is.na(test_f)){
  stop("miss --test filename", call.=FALSE)
}
if(is.na(report_f)){
  stop("miss --report filename", call.=FALSE)
}
if(is.na(predict_f)){
  stop("miss --predict filename", call.=FALSE)
}
print(foldnumber)
print(train_f)
print(test_f)
print(report_f)
print(predict_f)


impute.age <- function(age,class){
  vector <- age
  for (i in 1:length(age)){
    if (is.na(age[i])){
      if (class[i] == 1){
        vector[i] <- round(mean(filter(titanic,Pclass==1)$Age, na.rm=TRUE),0)
      }else if (class[i] == 2){
        vector[i] <- round(mean(filter(titanic,Pclass==2)$Age, na.rm=TRUE),0)
      }else{
        vector[i] <- round(mean(filter(titanic,Pclass==3)$Age, na.rm=TRUE),0)
      }
    }else{
      vector[i]<-age[i]
    }
  }
  return(vector)
}


datapreprocess<-function(titanic){
  #Missing Fare Data Imputation
  titanic$Fare[is.na(titanic$Fare)==TRUE] = median(filter(titanic, Pclass==3 & Embarked=="S")$Fare, na.rm=TRUE)
  #Missing Embarked Data Imputation
  titanic$Embarked[titanic$Embarked==""] = "C"
  #Missing Age Data Imputation
  imputed.age <- impute.age(titanic$Age,titanic$Pclass)
  titanic$Age <- imputed.age
  #Feature Engineering
  titanic$Title <- gsub("^.*, (.*?)\\..*$", "\\1", titanic$Name)
  # Frequency of each title by sex
  #print(table(titanic$Sex, titanic$Title))
  titanic$Title[titanic$Title == 'Mlle' | titanic$Title == 'Ms'] <- 'Miss' 
  titanic$Title[titanic$Title == 'Mme']  <- 'Mrs' 
  
  #create a new category with low frequency of titles
  Other <- c('Dona', 'Dr', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Jonkheer', 'Major', 'Rev', 'Sir')
  titanic$Title[titanic$Title %in% Other]  <- 'Other'
  #Family number
  FamilySize <- titanic$SibSp + titanic$Parch + 1
  
  #create a new category with different Family number
  titanic$FamilySize <- sapply(1:nrow(titanic), function(x) 
            ifelse(FamilySize[x]==1, "Single", 
            ifelse(FamilySize[x]>4, "Large", "Small")))
  #use factor
  titanic$Survived = factor(titanic$Survived)
  titanic$Pclass = factor(titanic$Pclass)
  titanic$Sex = factor(titanic$Sex)
  titanic$Embarked = factor(titanic$Embarked)
  titanic$Title = factor(titanic$Title)
  titanic$FamilySize = factor(titanic$FamilySize, levels=c("Single","Small","Large"))
  
  #Checking the structure of the data

  # Checking missing values
  #print(colSums(is.na(titanic)|titanic==''))
  
  return(titanic)
}








titanic_train = read.csv(train_f)
titanic_test = read.csv(test_f)
#print(nrow(titanic_train))
#print(nrow(titanic_test))
titanic <- bind_rows(titanic_train, titanic_test)
#print(str(titanic))
afterprocess<-datapreprocess(titanic)
print("----------------------------")
print(str(afterprocess))
train_original <- afterprocess[1:891, c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","Title","FamilySize")]
test_original <- afterprocess[892:1309, c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","Title","FamilySize")]


foldnumber <- as.numeric(foldnumber)

##use caret split data
set.seed(1)
splitfold <- createFolds(train_original$Pclass, k = foldnumber)
#print(splitfold)
set<-c()
train<-c()
validat<-c()
test<-c()

##use while and for calcuate each target fold

flag <- 1

while(flag<=foldnumber){
  empty_train <- data.frame()
  for (i in c(1:foldnumber)){
    if(flag!=foldnumber){
      if(i==flag){
        testing <-train_original[as.numeric(unlist(splitfold[i])),]
      }else if(i==flag+1){
        validation <-train_original[as.numeric(unlist(splitfold[i])),]
      }else{
        empty_train<-rbind(empty_train, train_original[as.numeric(unlist(splitfold[i])),])
      }
    }else{
      if(i==flag){
        testing <-train_original[as.numeric(unlist(splitfold[i])),]
      }else if(i==1){
        validation <-train_original[as.numeric(unlist(splitfold[i])),]
      }else{
        empty_train<-rbind(empty_train, train_original[as.numeric(unlist(splitfold[i])),])
      }
    }
  }
  #print(head(empty_train))
  setname<-paste('fold',as.character(flag),sep='')
  print(setname)
  set<-append(set,setname)
  model <- rpart(Survived ~.,data=empty_train,method="class")
  resultframe <- data.frame(truth=empty_train$Survived,pred=predict(model,empty_train,type="class"))
  traintab <- table(resultframe)
  trainvalue<-round(sum(diag(traintab))/sum(traintab),2)
  print("train accuracy:")
  print(trainvalue)
  train<-append(train,trainvalue)
  
  #print(validation$V2)
  resultframe2 <- data.frame(truth=validation$Survived,pred=predict(model,validation,type="class"))
  validtab <- table(resultframe2)
  validvalue<-round(sum(diag(validtab))/sum(validtab),2)
  print("valid accuracy:")
  print(validvalue)
  validat<-append(validat,validvalue)
  
  resultframe3 <- data.frame(truth=testing$Survived,pred=predict(model,testing,type="class"))
  testtab <- table(resultframe3)
  testvalue<-round(sum(diag(testtab))/sum(testtab),2)
  print("test accuracy:")
  print(testvalue)
  test<-append(test,testvalue)
  
  flag<-flag+1
}
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


write.csv(finaldata,report_f, row.names = FALSE,quote=F)
# Save the results
results <- data.frame(PassengerID = titanic[892:1309,"PassengerId"], Survived=predict(model,test_original,type="class"))

# Write the results to a csv file
write.csv(results, file = predict_f, row.names = FALSE, quote=FALSE)




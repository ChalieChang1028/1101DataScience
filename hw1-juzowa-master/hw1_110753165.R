########################
# homework1 example
########################

args = commandArgs(trailingOnly=TRUE)
if (length(args)<4) {
  warning("pls use format Rscript hw1_exam.R --input inputpath --output outputpath")
} else if (length(args)==4) {
  first <- args[1]
  second <- args[2]
  third <- args[3]
  fourth <- args[4]
}else{
  warning("pls use format Rscript hw1_exam.R --input inputpath --output outputpath")
}

#handle command with --inpt --output
inoutflag <- 0
if(grepl( "--input",first)){
  inoutflag<-1
  inputpath<-second
}else if(grepl( "--output",first)){
  inoutflag<-2
  outputpath <- second
}else{
  warning("warning:pls use flag --inpt or --output")
}
if(inoutflag==1 && grepl( "--output",third)){
  outputpath <- fourth
}else if(inoutflag==2 && grepl( "--input",third)){
  inputpath <- fourth
}

#readcsv
inputcsv <- read.csv(file = inputpath,header = TRUE)

#check height and weight exit

if(is.null(inputcsv$weight)){
  warning("csv don't have weight column")
}else{
  maxweight <- inputcsv$weight[which.max(inputcsv$weight)]
}
if(is.null(inputcsv$height)){
  warning("csv don't have height column")
}else{
  maxheight <- inputcsv$height[which.max(inputcsv$height)]
}


#inputpath findsetname ans stripe .csv
list1<-strsplit(inputpath, "[/. ]")
setname <- sapply(list1, tail, 2)[1]


#outputpath check.csv
list2<-strsplit(outputpath, "[/]")
checkcsv <- sapply(list2, tail, 1)
#print(checkcsv)
#print(gregexpr(pattern ='.csv',checkcsv)[[1]][1])
if(gregexpr(pattern ='.csv',checkcsv)[[1]][1]==-1){
  warning("forget add .csv in your outputpath")
}

outputdata <- data.frame(setname, round(maxweight,2) ,round(maxheight,2))
outputheaders <- c("set", "weight", "height")
colnames(outputdata) <- outputheaders
print(head(outputdata))

write.csv(outputdata,outputpath,row.names = FALSE,quote=FALSE)







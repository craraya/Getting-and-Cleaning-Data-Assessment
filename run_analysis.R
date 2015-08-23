#setwd("/media/caraya/DATA/CURSOS/02-Datascience/Getting and Cleaning Data/Assessment")
#getwd()
#dir()
##----------------------------------------------------------------------------
################################################################################
## Some Functions

# dfL : dataframe Left
# dfR : dataframe Right
# selectL : select variables from dataframeL
# selectR : select variables from dataframeR
m.left_join<-function(dfL,dfR
                      ,selectL=names(dfL),selecctR=names(dfR)
                      ,by=c()){
  #dfL<-x
  #dfR<-y
  #by<-c("k1","k2")
  selectL<-names(dfL)
  selectR<-names(dfR)
  # verfy parameters
  if(!is.data.frame(dfL) || !is.data.frame(dfR)) {cat("Not a data.frame")}
  if(!all(selectL %in% names(dfL))) {cat("unselect variable dataframe L")}
  if(!all(selectR %in% names(dfR))) {cat("unselect variable dataframe R")}
  if(!all(by %in% selectL)) {cat("key from dataframe 1 not in base")}
  if(!all(by %in% selectR)) {cat("key from dataframe 2 not in base")}
  a<-dfL[selectL]
  b<-dfR[selectR]
  merge(a,b,by)
}

# Delete varc ocurrences on df
m.delete.na<-function(df,var=c()){
  aux<-df
  l<-length(var)
  for(i in 1:l){
    v<-var[i]
    aux<-aux[!is.na(aux[v]),]
  }
  aux
}

m.sort<-function(df,by=c(),type=c()){
  aux<-df
  l<-length(by)
  for(i in l:1){ #i<-1
    v<-by[i]
    aux<-aux[order(aux[v],na.last = F),]
  }
  aux
}

################################################################################
## Read the variables's names.

features<-read.table("UCI HAR Dataset/features.txt")
activity<-read.table("UCI HAR Dataset/activity_labels.txt")

X_test<-read.table("UCI HAR Dataset/test/X_test.txt")
y_test<-read.table("UCI HAR Dataset/test/y_test.txt")
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt")
X_train<-read.table("UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("UCI HAR Dataset/train/y_train.txt")
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt")

## 1.- Merges the training and the test sets to create one data set.
train<-cbind(rep("train",nrow(subject_train))
            ,subject_train
            ,m.left_join(y_train,activity,by=c("V1"))[,2]
            ,X_train)
colnames(train)<-c("partition","subject_train","activity",features$V2)

test<-cbind(rep("test",nrow(subject_test))
             ,subject_test
             ,m.left_join(y_test,activity,by=c("V1"))[,2]
             ,X_test)
colnames(test)<-c("partition","subject_train","activity",features$V2)

tidyData<-rbind(train,test)

write.table(tidyData,"Outputs/tidyData.txt",sep="\t",quote=F)

## 2.- Extracts only the measurements on the mean and standard deviation for each 
## measurement. 

data_mean<-cbind(features$V2,as.data.frame(apply(tidyData[,-c(1:3)],2,mean)))
data_std<-cbind(features$V2,as.data.frame(sqrt(apply(tidyData[,-c(1:3)],2,var))))
colnames(data_mean)<-c("features","mean")
colnames(data_std)<-c("features","standard deviation")

write.table(data_mean,"Outputs/data_mean.txt",sep="\t",quote=F)
write.table(data_std,"Outputs/data_std.txt",sep="\t",quote=F)

## From the data set in step 4, creates a second, independent tidy data set with 
## the average of each variable for each activity and each subject.

data2<-aggregate(. ~ activity + subject_train, tidyData, mean)
write.table(data2,"Outputs/aggregate_data.txt",sep="\t",quote=F)


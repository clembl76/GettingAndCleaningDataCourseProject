
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Set working directory
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#Set working directory
setwd("/Volumes/DANE ELEC/data scientist toolbox/R-wd/ProgrammingAssignement3") #mac
#setwd("G:/data scientist toolbox/R-wd/ProgrammingAssignement3") #pc

#Data is unzipped in the working directory, in folder /UCI HAR Dataset

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# load labels info
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#features.txt table has code/label correspondance for measurements
features<-read.table("./UCI HAR Dataset/features.txt")
names(features)<-c("featureCode","featureLabel")

# activity
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")
names(activity_labels)<-c("activityCode","activityLabel")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# load data
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")#,nrows=20) #,skip=4,na.strings = c("..",""), stringsAsFactors=FALSE)
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")#,nrows=20)
Y_test<-read.table("./UCI HAR Dataset/test/Y_test.txt")#,nrows=20)
names(subject_test)<-"subject"
names(Y_test)<-"activityCode"

subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")#,nrows=20)
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")#,nrows=20)
Y_train<-read.table("./UCI HAR Dataset/train/Y_train.txt")#,nrows=20)
names(subject_train)<-"subject"
names(Y_train)<-"activityCode" # "label"

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#merge train and test data sets
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#merge subject & labels & set
test<-cbind(subject_test,Y_test,X_test) 
train<-cbind(subject_train,Y_train,X_train)
#bind all rows from train and from test into "all" dataset
all<-rbind(test,train)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# je ne suis pas sure de comprendre => calculer les mean/standard dev pour chaque colonne? 
# ou filtrer uniquement les lignes pour lesquelles on est dans la moyenne/ecarttype?

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Uses descriptive activity names to name the activities in the data set
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#load data.table package to be able to add activityLabel through a join on activityCode
install.packages("data.table")
#library("data.table", lib.loc="D:/Program Files/R/R-3.0.3/library") #pc
library("data.table", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")#mac

# put both data and labels in data.table format
all_dt<-data.table(all)
activity_labels_dt<-data.table(activity_labels)

#set indexes
setkey(all_dt,activityCode)
setkey(activity_labels_dt,activityCode)

#left join
all_dt<-merge(all_dt,activity_labels,by="activityCode")

#delete work tables to clean workspace
rm(subject_test);rm(X_test);rm(Y_test);rm(subject_train);rm(X_train);rm(Y_train);rm(test);rm(train);rm(all) 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Appropriately labels the data set with descriptive variable names. 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
varNames<-as.vector(features[,2])
names(all_dt)<-c("activityCode","subject",varNames,"activityLabel")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#remove unused activityCode column
all_dt$activityCode <- NULL

#moyenne de chaque variable selon activity et subject

#load reshape package to be able to tidy and do calculation on dataset
install.packages("reshape")
library("reshape", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

# first, use melt to have a dataset with columns (activityLabel, subject, variable, value)
tidy0<-melt(all_dt,id=c("activityLabel","subject"))
# second, use cast to calculate mean of each value, grouped by (activityLabel, subject)
tidy<-cast(tidy0, activityLabel +  subject ~ value , mean)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


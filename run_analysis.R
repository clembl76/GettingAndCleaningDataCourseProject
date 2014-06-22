
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Set working directory
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#Set working directory
setwd("G:/data scientist toolbox/R-wd/ProgrammingAssignement3") #pc

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

subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test<-read.table("./UCI HAR Dataset/test/Y_test.txt")
names(subject_test)<-"subject"
names(Y_test)<-"activityCode"

subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train<-read.table("./UCI HAR Dataset/train/Y_train.txt")
names(subject_train)<-"subject"
names(Y_train)<-"activityCode"

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
# columns names are located in features[,2]
# create a pattern to select only columns with mean or std in their name
pattern = '(mean|std)\\(\\)'
# execute a pattern-matching function on features data to create an index vector
ind = grep(pattern, features$featureLabel, perl=T)
# use this index vector to extract only columns names I will need
selected_features = as.vector(paste("V", features[ind,1],sep=""))

all<-cbind(all[,c(1:2)],all[,c(selected_features)])

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Uses descriptive activity names to name the activities in the data set
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#load data.table package to be able to add activityLabel through a join on activityCode
install.packages("data.table")
library("data.table", lib.loc="D:/Program Files/R/R-3.0.3/library") #pc

# put both data and labels in data.table format
all_dt<-data.table(all)
activity_labels_dt<-data.table(activity_labels)

#set indexes
setkey(all_dt,activityCode)
setkey(activity_labels_dt,activityCode)

#left join
all_dt<-merge(all_dt,activity_labels,by="activityCode")

#delete work tables to clean workspace
rm(subject_test);rm(X_test);rm(Y_test);rm(subject_train);rm(X_train);rm(Y_train);rm(test);rm(train);rm(all) ;rm(activity_labels)
#remove unused activityCode column
all_dt$activityCode <- NULL

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Appropriately labels the data set with descriptive variable names. 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
varNames<-as.vector(features[ind,1])
names(all_dt)<-c("subject",varNames,"activityLabel")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#load reshape package to be able to tidy and do calculation on dataset
install.packages("reshape2")
library("reshape2", lib.loc="D:/Program Files/R/R-3.0.3/library") #pc


# first, use melt to have a dataset with columns (activityLabel, subject, variable, value)
tidy0<-melt(all_dt,id=c("activityLabel","subject"))
rm(all_dt)

# second, use cast to calculate mean of each value, grouped by (activityLabel, subject)

# put data in data.table format
tidy0<-data.table(tidy0)
#set indexes
setkey(tidy0,activityLabel,subject)

startTime<-date()
tidy<-cast(tidy0, activityLabel +  subject ~ value , mean)
endTime<-date()
calcTime<-endTime-startTime
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# export tidy data to a txt file
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

write.table(tidy, "./tidy.txt", sep=";")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# course project

# download and unzip data
if (!file.exists("./UCI HAR Dataset")) {
  fileurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileurl, destfile="data_course_project.zip")
  unzip("data_course_project.zip")
  dataDownloaded<-date()
}

# load libraries
library("plyr")
library("dplyr")

# Read data
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
features<-data.frame(read.table("./UCI HAR Dataset/features.txt"))
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")

#---------------------------------------------------------------
##1. Merges the training and the test sets to create one data set
# Merge subject_train, y_train and X_train
Xy_train<-cbind(subject_train, y_train, X_train)

# Merge subject_test, y_test and X_test
Xy_test<-cbind(subject_test, y_test, X_test)

# Merge train and test datasets
Xy_train_test<-data.frame(rbind(Xy_train, Xy_test))

#---------------------------------------------------------------
##2. Extracts only the measurements on the mean and standard deviation for each measurement.
# Convert 2nd column of features from factor to character 
features<-mutate(features, V2=as.character(V2))
# Define vector of cloumns whose name include "mean", "Mean" or "std"
col_mean_std<-c(grep("mean", features$V2), grep("Mean", features$V2), grep("std", features$V2))
Xy_train_test_temp<-select(Xy_train_test, -(V1:V1.1))
Xy_train_test_ext_temp<-Xy_train_test_temp[ ,col_mean_std]
 
Xy_train_test_ext<-data.frame(Subject=Xy_train_test[, 1], 
                              Activity=Xy_train_test[, 2], Xy_train_test_ext_temp)
colnames(Xy_train_test_ext)<-c("Subject", "Activity", features[col_mean_std, 2])

#---------------------------------------------------------------
##3. Uses descriptive activity names to name the activities in the data set
# Xy_train_test_ext<-arrange(Xy_train_test_ext, Activity)
for (i in 1:dim(activity_labels)[1]) {
  Xy_train_test_ext$Activity[Xy_train_test_ext$Activity==i]<-
    as.character(activity_labels[i, 2])
}

#---------------------------------------------------------------
##4. Appropriately labels the data set with descriptive variable names. 
colnames(Xy_train_test_ext)<-c("Subject", "Activity", features[col_mean_std, 2])

#---------------------------------------------------------------
##5. From the data set in step 4, creates a second, 
## independent tidy data set with the average of each variable for each activity and each subject.

# Compute average value of each variable for each activity and each subject
average_table<-data.frame()
for (i in 1:length(unique(Xy_train_test_ext$Subject))) {
  for (j in 1:length(unique(Xy_train_test_ext$Activity))) {
    table_ij<-filter(Xy_train_test_ext, Subject==i, Activity==activity_labels[j, 2])
    
    
    value_average<-c(i, j)
    for (k in 1:length(features[col_mean_std, 2])) {
      value_average<-c(value_average, mean(table_ij[, k+2]))
    }
    average_table<-rbind(average_table, value_average)
  }
}

# Define column names
names_average<-c()
for (k in 1:length(features[col_mean_std, 2])) {
  names_average<-c(names_average, paste("Average-", features[col_mean_std[k], 2]))
}
colnames(average_table)<-c("Subject", "Activity", names_average)

# Re-define names of activities using descriptive names
for (i in 1:dim(activity_labels)[1]) {
  average_table$Activity[average_table$Activity==i]<-
    as.character(activity_labels[i, 2])
}

# write data from step 5 to file
write.table(average_table, file = "data_course_project.txt", row.names = FALSE)


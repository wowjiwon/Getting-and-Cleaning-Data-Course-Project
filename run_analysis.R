### Getting and Cleaning Data Course Project
### Written by wowjiwon
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## 1. Merges the training and the test sets to create one data set.
# Downloaded the files in the working directory. My Mac automatically unzipped the files.
list.files("./UCI HAR Dataset", recursive=T)

# Read the training and test data sets as well as feature and activity labels.
xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt", header=F)
ytrain <- read.table("./UCI HAR Dataset/train/Y_train.txt", header=F)
subtrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", header=F)
xtest <- read.table(".//UCI HAR Dataset/test/X_test.txt", header=F)
ytest <- read.table("./UCI HAR Dataset/test/Y_test.txt", header=F)
subtest <- read.table("./UCI HAR Dataset/test/subject_test.txt", header=F)
feature <- read.table("./UCI HAR Dataset/features.txt", header=F)
activitylab <- read.table("./UCI HAR Dataset/activity_labels.txt", header=F)

# Label the column names of the data sets.
colnames(xtrain) <- feature[,2]
colnames(ytrain) <- c("activityID")
colnames(subtrain) <- c("subjectID")
colnames(xtest) <- feature[,2]
colnames(ytest) <- c("activityID")
colnames(subtest) <- c("subjectID")
colnames(activitylab) <- c("activityID","activityType")

# Merge the training and the test sets to create one data set.
train <- cbind(subtrain, ytrain, xtrain)
test <- cbind(subtest, ytest, xtest)
mergedData <- rbind(train, test)
View(mergedData)


## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
colNames <- names(mergedData)
meanstd <- grepl("activityID",colNames) | grepl("subjectID",colNames) | grepl("mean..",colNames) | grepl("std..",colNames)
meanstdData <- mergedData[, meanstd]
View(meanstdData)


## 3. Uses descriptive activity names to name the activities in the data set.
meanstdData$activityID <- activitylab[meanstdData$activityID, 2]
View(meanstdData)


## 4. Appropriately labels the data set with descriptive variable names.
meanstdcolNames <- names(meanstdData)
meanstdcolNames <- gsub("^t","time",meanstdcolNames)
meanstdcolNames <- gsub("^f","frequency",meanstdcolNames)
meanstdcolNames <- gsub("Acc","Accelerometer",meanstdcolNames)
meanstdcolNames <- gsub("Gyro","Gyroscope",meanstdcolNames)
meanstdcolNames <- gsub("Mag","Magnitude",meanstdcolNames)
meanstdcolNames <- gsub("BodyBody","Body",meanstdcolNames)
colnames(meanstdData) <- meanstdcolNames


## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
sum(is.na(meanstdData)) # No missing data
library(dplyr)
TidyData <- meanstdData %>% group_by(subjectID, activityID) %>% summarize_all(funs(mean))
write.table(TidyData, "Tidy Data.txt", row.name=F)


## 6. Made codebook for TidyData
library(dataMaid)
makeCodebook(TidyData)




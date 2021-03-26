# Getting and Cleaning Data Project John Hopkins Coursera

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#-------------------------------------------------------------------------------------------
library(dplyr)
library(data.table)
#-------------------------------------------------------------------------------------------
# 1. Merges the training and the test sets to create one data set.
## Load testing data
test_subject  <- read.table(file = "./test/subject_test.txt", header=FALSE)
test_activity <- read.table(file = "./test/y_test.txt", header=FALSE)
test_features <- read.table(file = "./test/X_test.txt", header=FALSE)

## Load training data
train_subject  <- read.table(file = "./train/subject_train.txt", header=FALSE)
train_activity <- read.table(file = "./train/y_train.txt",header=FALSE)
train_features <- read.table(file = "./train/X_train.txt",header=FALSE)

# Read in feature names 
feature_names   <- read.table("./features.txt")

# Read in activity labels
activity_labels = read.table('./activity_labels.txt', col.names = c("Activity", "ActivityType"))

# Assign Column Names
colnames(train_features) <- feature_names[,2] 
colnames(train_activity) <- "Activity"
colnames(train_subject)  <- "Subject"

colnames(test_features) <- feature_names[,2] 
colnames(test_activity) <- "Activity"
colnames(test_subject)  <- "Subject"

# Create One Data Set 
train_complete <- cbind(train_subject, train_activity, train_features)
test_complete  <- cbind(test_subject, test_activity, test_features)
OneDataSet     <- rbind(train_complete, test_complete)

# ------------------------------------------------------------------------------------------
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
mean_and_std  <- grepl("(-std\\(\\)|-mean\\(\\))",feature_names$V2)
OneDataSet_MS <- OneDataSet[, which(mean_and_std == TRUE)]

#--------------------------------------------------------------------------------------------
# 3. Uses descriptive activity names to name the activities in the data set
OneDataSet_MS[[2]] <- factor(OneDataSet_MS[, 2]
                                 , levels = activity_labels[["Activity"]]
                                 , labels = activity_labels[["ActivityType"]])

#--------------------------------------------------------------------------------------------
# 4. Appropriately labels the data set with descriptive variable names.
# Look at # 1 Assign Column Names

#--------------------------------------------------------------------------------------------
# 5. From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.

YData <- rbind(test_activity,train_activity)
Activity <- as.factor(YData$Activity)

SData <- rbind(test_subject,train_subject)
Subject <- as.factor(SData$Subject)


## Extract Tidy Data Set
OneDataSet_MS$Subject <- as.factor(OneDataSet_MS$Subject)
OneDataSet_MS <- data.table(OneDataSet_MS)

tidydata <- aggregate(. ~Subject + Activity, OneDataSet_MS, mean)
tidydata <- tidydata[order(tidydata$Subject,tidydata$Activity),]
write.table(x = tidydata,file = "tidydata.txt",row.names = FALSE)

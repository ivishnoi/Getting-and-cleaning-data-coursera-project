library(reshape2)

filename <- "getdata_dataset.zip"

## Download and unzip the dataset:
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")
}  


true_file <- "UCI HAR Dataset"
if (!file.exists(true_file)) {
  unzip(filename)
}



# read training data
trainSubjects <- read.table(file.path(true_file, "train", "subject_train.txt"))
trainValues <- read.table(file.path(true_file, "train", "X_train.txt"))
trainActivity <- read.table(file.path(true_file, "train", "y_train.txt"))

# read test data
testSubjects <- read.table(file.path(true_file, "test", "subject_test.txt"))
testValues <- read.table(file.path(true_file, "test", "X_test.txt"))
testActivity <- read.table(file.path(true_file, "test", "y_test.txt"))

# read features
features <- read.table(file.path(true_file, "features.txt"), as.is = TRUE)


# read activity labels
activities <- read.table(file.path(true_file, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

###### 1 ### Merge the training and the test sets to create one data set


humanActivity <- rbind(
  cbind(trainSubjects, trainValues, trainActivity),
  cbind(testSubjects, testValues, testActivity)
)

# remove individual data tables to save memory
rm(trainSubjects, trainValues, trainActivity, 
   testSubjects, testValues, testActivity)

# assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")


### 2 ### Extract the measurements on the mean and standard deviation

columnsRequired <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsRequired]

### 3 ### Use descriptive activity names to name the activities 

humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

#### 4 ### Label the dataset with descriptive variable names

humanActivityCols <- colnames(humanActivity)

humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)


humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)


humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# use new labels as column names
colnames(humanActivity) <- humanActivityCols


### 5 ### Create tidy set with the average of each variable for each activity and each subject

# group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)





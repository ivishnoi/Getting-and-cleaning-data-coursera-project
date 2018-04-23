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

activity_all <- rbind(
  cbind(trainSubjects, trainValues, trainActivity),
  cbind(testSubjects, testValues, testActivity)
)

# remove individual data tables to save memory
rm(trainSubjects, trainValues, trainActivity, 
   testSubjects, testValues, testActivity)

# assign column names
colnames(activity_all) <- c("subject", features[, 2], "activity")


### 2 ### Extract the measurements on the mean and standard deviation

columnsRequired <- grepl("subject|activity|mean|std", colnames(activity_all))
activity_all <- activity_all[, columnsRequired]

### 3 ### Use descriptive activity names to name the activities 

activity_all$activity <- factor(activity_all$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

#### 4 ### Label the dataset with descriptive variable names

ActivityCols <- colnames(activity_all)

ActivityCols <- gsub("[\\(\\)-]", "", ActivityCols)


ActivityCols <- gsub("^f", "frequencyDomain", ActivityCols)
ActivityCols <- gsub("^t", "timeDomain", ActivityCols)
ActivityCols <- gsub("Acc", "Accelerometer", ActivityCols)
ActivityCols <- gsub("Gyro", "Gyroscope", ActivityCols)
ActivityCols <- gsub("Mag", "Magnitude", ActivityCols)
ActivityCols <- gsub("Freq", "Frequency", ActivityCols)
ActivityCols <- gsub("mean", "Mean", ActivityCols)
ActivityCols <- gsub("std", "StandardDeviation", ActivityCols)


ActivityCols <- gsub("BodyBody", "Body", ActivityCols)

# use new labels as column names
colnames(activity_all) <- ActivityCols


### 5 ### Create tidy set with the average of each variable for each activity and each subject

# group by subject and activity and summarise using mean
humanActivityMeans <- activity_all %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)





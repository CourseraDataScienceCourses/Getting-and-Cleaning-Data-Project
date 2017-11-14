###############################################################################
## Getting and Cleaning Data on Coursera - Course Project
###############################################################################
###############################################################################


###############################################################################
## This script performs the following steps, in order:
##
## 0. If needed, downloads and unzips the data set.
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation
##    for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set.
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, it creates a second, independent tidy
##    data set with the average of each variable for each activity
##    and each subject.
###############################################################################


## Dependencies
###############################################################################

# the reshape2 package
library(reshape2)

# the data set URL
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"


## Step 0 - Download and unzip the data set
###############################################################################

filename <- "getdata_project_dataset.zip"

if (!file.exists(filename)){
  message("Downloading data set.")
  download.file(fileURL, filename, method = "curl", quiet = TRUE)
}  
if (!file.exists("UCI HAR Dataset")) {
  message("Unzipping data set.")
  unzip(filename) 
}


## Step 1 - Merge the training and test sets to create one data set
###############################################################################

message("Loading training sets.")
xTrain <- read.table(file.path("UCI HAR Dataset", "train", "X_train.txt"))
yTrain <- read.table(file.path("UCI HAR Dataset", "train", "y_train.txt"))
subjTrain <- read.table(file.path("UCI HAR Dataset", "train", "subject_train.txt"))

message("Loading test sets.")
xTest <- read.table(file.path("UCI HAR Dataset", "test", "X_test.txt"))
yTest <- read.table(file.path("UCI HAR Dataset", "test", "y_test.txt"))
subjTest <- read.table(file.path("UCI HAR Dataset", "test", "subject_test.txt"))

message("Merging training and test sets.")

# merge into 'data' data set
data <- rbind(xTrain, xTest)

# merge into 'activities' data set
activity <- rbind(yTrain, yTest)

# merge into 'subject' data set
subject <- rbind(subjTrain, subjTest)


# Step 2 - Extract only the measurements on the mean and standard deviation for each measurement
###############################################################################

message("Extracting mean and standard deviation for each measurement.")

# load features
features <- read.table(file.path("UCI HAR Dataset", "features.txt"))
features[,2] <- as.character(features[,2])

# select "wanted" features
featuresWanted <- grep(".*mean.*|.*std.*", features[,2])
featuresWanted.names <- features[featuresWanted,2]
featuresWanted.names <- gsub('-mean', 'Mean', featuresWanted.names)
featuresWanted.names <- gsub('-std', 'Std', featuresWanted.names)
featuresWanted.names <- gsub('[-()]', '', featuresWanted.names)

# extract "wanted" columns from data set
data <- data[featuresWanted]


# Step 3 - Use descriptive activity names to name the activities in the data set
###############################################################################

message("Re-labelling data set.")

# load activity labels
activityLabels <- read.table(file.path("UCI HAR Dataset", "activity_labels.txt"))
activityLabels[,2] <- as.character(activityLabels[,2])

activity[, 1] <- activityLabels[activity[, 1], 2]


# Step 4 - Appropriately label the data set with descriptive variable names
###############################################################################

# bind all the data in a single data set
allData <- cbind(subject, activity, data)

# give descriptive names
colnames(allData) <- c("Subject", "Activity", featuresWanted.names)


# Step 5 - Create a second, independent tidy data set with the average
# of each variable for each activity and each subject
###############################################################################

message("Creating output data set.")

# turn activities / subjects into factors
allData$Activity <- factor(allData$Activity, labels = activityLabels[,2])
allData$Subject <- as.factor(allData$Subject)

# calculate averages
allData.melted <- melt(allData, id = c("Subject", "Activity"))
allData.mean <- dcast(allData.melted, Subject + Activity ~ variable, mean)

# create output file
write.table(allData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)

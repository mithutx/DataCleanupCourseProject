#setwd("C:/LocalTraining/DataScience/DataCleanup")

# Downloading and Extracting data
if (!file.exists("data")){
  dir.create("data")
}
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip?AccessType=DOWNLOAD"
download.file(fileurl,destfile="./data/courseproj.zip",mode = "wb")
dateDownloaded <- date()
unzip("./data/courseproj.zip")

# Load required libraries
library(data.table)
library(dplyr)

# Read the metadata for the data
featureMetadata <- read.table("UCI HAR Dataset/features.txt")
activityMetadata <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

# Reading test data
testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
testActivity <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
testFeatures <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

# Reading training data
trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
trainActivity <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
trainFeatures <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

# Merge datasets
subjectMerged <- rbind(trainSubject, testSubject)
activityMerged <- rbind(trainActivity, testActivity)
featuresMerged <- rbind(trainFeatures, testFeatures)

# Add column names for features
colnames(featuresMerged) <- t(featureMetadata[2])

colnames(activityMerged) <- "Activity"
colnames(subjectMerged) <- "Subject"
combinedData <- cbind(featuresMerged, activityMerged, subjectMerged)

# Subset columns containing 'mean' or 'std'
columnsMeanOrStdDev <- grep(".*Mean.*|.*Std.*", names(combinedData), ignore.case=TRUE)
combinedColumns <- c(columnsMeanOrStdDev, 562, 563)
finalDataset <- combinedData[,combinedColumns]

# Cleanup the data types for the variables
finalDataset$Activity <- as.character(finalDataset$Activity)
for (i in 1:6){
  finalDataset$Activity[finalDataset$Activity == i] <- as.character(activityMetadata[i,2])
}

finalDataset$Activity <- as.factor(finalDataset$Activity)

# Replace with descriptive variable names
names(finalDataset)
names(finalDataset)<-gsub("BodyBody", "Body", names(finalDataset))
names(finalDataset)<-gsub(" ", "", names(finalDataset))
names(finalDataset)<-gsub("^t", "Time", names(finalDataset))
names(finalDataset)<-gsub("^f", "Frequency", names(finalDataset))
names(finalDataset)<-gsub("Acc", "Accelerometer", names(finalDataset))
names(finalDataset)<-gsub("Gyro", "Gyroscope", names(finalDataset))
names(finalDataset)<-gsub("Mag", "Magnitude", names(finalDataset))
names(finalDataset)<-gsub("tBody", "TimeBody", names(finalDataset))
names(finalDataset)<-gsub("-mean()", "Mean", names(finalDataset), ignore.case = TRUE)
names(finalDataset)<-gsub("-std()", "StdDev", names(finalDataset), ignore.case = TRUE)
names(finalDataset)<-gsub("-freq()", "Frequency", names(finalDataset), ignore.case = TRUE)
names(finalDataset)<-gsub("angle", "Angle", names(finalDataset))
names(finalDataset)<-gsub("gravity", "Gravity", names(finalDataset))


## Update datatype of the Subject column
finalDataset$Subject <- as.factor(finalDataset$Subject)
finalDataset <- data.table(finalDataset)

# Generate tidydata
tidyData <- aggregate(. ~Subject + Activity, finalDataset, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

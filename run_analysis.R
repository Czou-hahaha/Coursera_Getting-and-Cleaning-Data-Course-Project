library(dplyr)
setwd("/Users/corrine/Desktop/programming/Coursera_DS_R/1. JHU Data Science/03_cleaning and extract data/UCI HAR Dataset")
# get data
features <- read.table("features.txt", col.names = c("n","functions"))
activities <- read.table("activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("test/subject_test.txt", col.names = "subject")
x_test <- read.table("test/X_test.txt", col.names = features$functions)
y_test <- read.table("test/y_test.txt", col.names = "code")
subject_train <- read.table("train/subject_train.txt", col.names = "subject")
x_train <- read.table("train/X_train.txt", col.names = features$functions)
y_train <- read.table("train/y_train.txt", col.names = "code")

#reshape the data
## Step 1: Merges the trainning and the test sets to create one data set.
X <- rbind(x_train,x_test)
Y <- rbind(y_train,y_test)
Subject <- rbind(subject_train,subject_test)
Merged_data <- cbind(Subject,Y,X)

## Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
Tidydata <- Merged_data%>%
  select(subject,code,contains("mean"),contains("std"))

## Step 3: Uses descriptive activity names to name the activities int he data set.
Tidydata$code <- activities[Tidydata$code,2]

## Step 4: Appropriately labels the data set with descriptive variables names.
names(Tidydata)[2] = "activity"
names(Tidydata)<-gsub("Acc", "Accelerometer", names(Tidydata))
names(Tidydata)<-gsub("Gyro", "Gyroscope", names(Tidydata))
names(Tidydata)<-gsub("BodyBody", "Body", names(Tidydata))
names(Tidydata)<-gsub("Mag", "Magnitude", names(Tidydata))
names(Tidydata)<-gsub("^t", "Time", names(Tidydata))
names(Tidydata)<-gsub("^f", "Frequency", names(Tidydata))
names(Tidydata)<-gsub("tBody", "TimeBody", names(Tidydata))
names(Tidydata)<-gsub("-mean()", "Mean", names(Tidydata), ignore.case = TRUE)
names(Tidydata)<-gsub("-std()", "STD", names(Tidydata), ignore.case = TRUE)
names(Tidydata)<-gsub("-freq()", "Frequency", names(Tidydata), ignore.case = TRUE)
names(Tidydata)<-gsub("angle", "Angle", names(Tidydata))
names(Tidydata)<-gsub("gravity", "Gravity", names(Tidydata))

##Step 5: From the data set in step 4, creates a second, 
##independent tidy data set with the average of each variable for each activity and each subject.

FinalData <- Tidydata %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)

str(FinalData)








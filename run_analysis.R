#Getting and Cleaning Data Course Project

#Upload packages
library(dplyr)

#Read databases in the folder
features <- read.table("features.txt", col.names = c("n","functions"))
activities <- read.table("activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("test/subject_test.txt", col.names = "subject")
x_test <- read.table("test/X_test.txt", col.names = features$functions)
y_test <- read.table("test/y_test.txt", col.names = "code")
subject_train <- read.table("train/subject_train.txt", col.names = "subject")
x_train <- read.table("train/X_train.txt", col.names = features$functions)
y_train <- read.table("train/y_train.txt", col.names = "code")

#Step 1
#Merges the training and the test sets to create one data set.
x <- rbind(x_train, x_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
dataset <- cbind(subject, y, x)

#Step 2
#Extracts only the measurements on the mean and standard deviation for each measurement. 
step2 <- dataset %>% select(subject, code, contains("mean"), contains("std"))

#Step 3
#Uses descriptive activity names to name the activities in the data set
step2$code <- activities[step2$code, 2]

#Step 4
#Appropriately labels the data set with descriptive variable names. 
names(step2)[2] = "activity"
names(step2)<-gsub("Acc", "accelerometer", names(step2))
names(step2)<-gsub("Gyro", "gyroscope", names(step2))
names(step2)<-gsub("BodyBody", "body", names(step2))
names(step2)<-gsub("Mag", "magnitude", names(step2))
names(step2)<-gsub("^t", "time", names(step2))
names(step2)<-gsub("^f", "frequency", names(step2))
names(step2)<-gsub("tBody", "timeBody", names(step2))
names(step2)<-gsub("-mean()", "mean", names(step2), ignore.case = TRUE)
names(step2)<-gsub("-std()", "standarddeviation", names(step2), ignore.case = TRUE)
names(step2)<-gsub("-freq()", "frequency", names(step2), ignore.case = TRUE)
names(step2)<-gsub("angle", "angle", names(step2))
names(step2)<-gsub("gravity", "gravity", names(step2))

#Step 5
#From the data set in step 4, creates a second, independent tidy data set with the 
#average of each variable for each activity and each subject.
tidydata <- step2 %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(tidydata, "tidydata.txt", row.name=FALSE)





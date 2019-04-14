# Getting and cleaning data: Week 4 project assignment
# WLE20190414

setwd("~/Google Drive/DataScience/Getting_and_cleaning_data/Week4/ProjectAssignment/")

# Load the required packages
library(tidyverse)
library(data.table)
library(mgsub)
library(plyr)

# Download the data file
Download.Date <- Sys.Date() # "2019-04-14"

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "dat.zip", method = "curl")
unzip(zipfile = "dat.zip", exdir = "./data")

# Load descriptive tables and data sets

activity_labels <- read.table("data/UCI HAR Dataset/activity_labels.txt")
features <- read.table("data/UCI HAR Dataset/features.txt")

subject_test <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("data/UCI HAR Dataset/test/y_test.txt") # activity

subject_train <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("data/UCI HAR Dataset/train/y_train.txt") # activity


# 1. Merge the training and the test sets to create one data set

df <- tbl_df(rbind(x_train, x_test))

# 2. Extract only the measurements on the mean and standard deviation for each measurement

mean.std.cols <- grep("mean()|std()", features[,2])
df <- df[, mean.std.cols]

# 3. Use descriptive activity names in "activity" to rename the numbered activities in the data set 

activity <- rbind(y_train, y_test)
named.activity <- mgsub::mgsub(string = activity, pattern = activity_labels[,1], 
                               replacement = activity_labels[,2])
df <- cbind(named.activity, df)

# 4. Label the data set with descriptive meand and std variable names 

variables <- c("activity", as.character(features[mean.std.cols, 2]))
colnames(df) <- variables

# 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.                 

subjects <- tbl_df(rbind(subject_train, subject_test))
colnames(subjects) <- "subject"
df <- cbind(subjects, df)
df <-  df %>% group_by(subject, activity) %>% summarise_all(funs(mean))
write.table(df, file = "tidy.data.txt", sep = "\t")



#STEP 0
#Dowloding and unzipping dataset


if(!file.exists("./data")){dir.create("./data")}

#dataset URl 
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#downloading and unzipping file
destFile <- "./data/Dataset.zip"
download.file(fileUrl, destFile)
unzip(zipfile = destFile, exdir = "./data")

#STEP 1
#Merging the training and the test sets 
#to create one data set

#Reading testing tables
dest <- "./data/UCI HAR Dataset/test"
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/Y_test.txt")
subcject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

#Reading training tables
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/Y_train.txt")
subcject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

#Reading features
features <- read.table('./data/UCI HAR Dataset/features.txt')

#Reading activity labels
activityLabels <- read.table('./data/UCI HAR Dataset/activity_labels.txt')

#Assigning columns names
colnames(x_train) <- features[,2]
colnames(y_train) <- "activityId"
colnames(subcject_train) <- "subjectId"

colnames(x_test) <- features[,2]
colnames(y_test) <- "activityId"
colnames(subcject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

#Mergind data in one dataset
merged_train <- cbind(y_train, subcject_train, x_train)
merged_test <- cbind(y_test, subcject_test, x_test)
DS <- rbind(merged_train, merged_test)

#STEP 2.
#Extracts only the measurements 
#on the mean and standard deviation for each measurement.

TidyDS <- DS %>% select(activityId, subjectId, contains("mean"), contains("std"))

#STEP 3
#Uses descriptive activity names to name the activities in the data set

TidyDS$activityId <- activityLabels[TidyDS$activityId, 2]

#STEP 4
#Appropriately labels the data set with descriptive variable names.
names(TidyDS)[1] = "activity"
names(TidyDS)[2] = "subject"
mames(TidyDS)<-gsub("Acc", "Accelerometer", names(TidyDS))
names(TidyDS)<-gsub("^t", "Time", names(TidyDS))
names(TidyDS)<-gsub("^f", "Frequency", names(TidyDS))
names(TidyDS)<-gsub("Gyro", "Gyroscope", names(TidyDS))
names(TidyDS)<-gsub("-mean()", "Mean", names(TidyDS), ignore.case = TRUE)
names(TidyDS)<-gsub("-std()", "STD", names(TidyDS), ignore.case = TRUE)


#STEP 5
#From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.

FinalDS <- TidyDS %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))

#Writing TidyDS in txt file
write.table(FinalDS, "FinalDS.txt", row.names=FALSE)


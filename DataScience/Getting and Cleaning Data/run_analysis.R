#******************************************************************
# Download and unzip the dataset ----
#******************************************************************

#Check if the file exists in the directory data
if (!file.exists("./data")) { dir.create("./data") }
#Url of the zip file
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download the file, and copy into Dataset.zip file
download.file(fileUrl, destfile = "./data/Dataset.zip")

# Unzip dataSet to /data directory
unzip(zipfile = "./data/Dataset.zip", exdir = "./data")

#******************************************************************
# Step1. Merge the training and the test sets to create one data set. ----
#******************************************************************

# Read trainings tables:
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

# Read testing tables:
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

# Read feature vector:
features <- read.table('./data/UCI HAR Dataset/features.txt')

# Read activity labels:
activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')

# Assign column names:
colnames(x_train) <- features[, 2]
colnames(y_train) <- "activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[, 2]
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId', 'activityType')

#Merge all data into one dataset
mrg_train <- cbind(y_train, subject_train, x_train)
mrg_test <- cbind(y_test, subject_test, x_test)
setAllInOne <- rbind(mrg_train, mrg_test)

#******************************************************************
# Step2. Extract only the measurements on the mean and standard deviation of each measurement ----
#******************************************************************

# Read column names
colNames <- colnames(setAllInOne)

#Create vector for defining ID, mean and standard deviation:
mean_std <- (grepl("activityId", colNames) |
                   grepl("subjectId", colNames) |
                   grepl("mean..", colNames) |
                   grepl("std..", colNames)
                   )
# Make nessesary subset from setAllInOne:
data_mean_sd <- setAllInOne[, mean_std == TRUE]

#******************************************************************
# Step3. Use descriptive activity names to name the activities in the data set ----
#******************************************************************

setActivityNames <- merge(data_mean_sd, activityLabels,
                              by = 'activityId',
                              all.x = TRUE)

#******************************************************************
# Step4. Appropriately labels the data set with descriptive variable names ----
#******************************************************************
#Done in previous steps

#******************************************************************
# Step5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject. ----
#******************************************************************

# Make a second tidy data set
TidySet <- aggregate(. ~ subjectId + activityId, setActivityNames, mean)
TidySet <- TidySet[order(TidySet$subjectId, TidySet$activityId),]

#Writing second tidy data set in txt file
write.table(TidySet, "TidySet.txt", row.name = FALSE)
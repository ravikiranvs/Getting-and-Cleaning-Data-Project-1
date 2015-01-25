#Read features txt file, contains the column names
featuresData <- read.table("UCI HAR Dataset/features.txt")

#Read the test data and combine with it's subject and activity data
testData_X <- read.table("UCI HAR Dataset/test/X_test.txt")
names(testData_X) <- featuresData$V2
testData_subject <- read.table("UCI HAR Dataset/test/subject_test.txt")
testData_X$Subject <- testData_subject$V1
testData_Y <- read.table("UCI HAR Dataset/test/Y_test.txt")
testData_X$Y <- testData_Y$V1

#Read the training data and combine with it's subject and activity data
trainData_X <- read.table("UCI HAR Dataset/train/X_train.txt")
names(trainData_X) <- featuresData$V2
trainData_subject <- read.table("UCI HAR Dataset/train/subject_train.txt")
trainData_X$Subject <- trainData_subject$V1
trainData_Y <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainData_X$Y <- trainData_Y$V1

#Merge the test and training data
AllData <- rbind(trainData_X, testData_X)

#Get the activity names and associate them with the data
activityLabs <- read.table("UCI HAR Dataset/activity_labels.txt")
names(activityLabs) <- c("Y", "Activity")
DataWithActivity <- merge(AllData, activityLabs, by.x = "Y", by.y = "Y")

#Function to select only columns with mean or std in their names
IsMeanOrStd <- function(ColName){
  if(grepl("-mean()", ColName) | grepl("-std()", ColName) | grepl("Subject", ColName) | grepl("Activity", ColName)){
    T;
  }
  else{
    F;
  }
}
IsMeanOrStd <- Vectorize(IsMeanOrStd)

#Select only the required columns from the data
cols <- names(DataWithActivity)
DataReqCols <- DataWithActivity[, cols[IsMeanOrStd(cols)]]

#Rename the columns to be more descriptive
cols <- names(DataReqCols)
cols <- gsub("[()]", "", cols)
cols <- gsub("-mean", "_Mean", cols)
cols <- gsub("-std", "_SD", cols)
cols <- gsub("Freq", "_Frequency", cols)
cols <- gsub("-X", "_X", cols)
cols <- gsub("-Y", "_Y", cols)
cols <- gsub("-Z", "_Z", cols)
cols <- gsub("tBodyAcc", "Time_Accelerometer_Body", cols)
cols <- gsub("tGravityAcc", "Time_Accelerometer_Gravity", cols)
cols <- gsub("tBodyGyro", "Time_Gyroscope_Gravity", cols)
cols <- gsub("fBodyAcc", "FFT_Accelerometer_Body", cols)
cols <- gsub("fGravityAcc", "FFT_Accelerometer_Gravity", cols)
cols <- gsub("fBodyGyro", "FFT_Gyroscope_Gravity", cols)
cols <- gsub("Jerk", "_Jerk", cols)
cols <- gsub("Mag", "_Manitude", cols)
cols <- gsub("fBodyBodyAcc", "FFT_Accelerometer_Body", cols)
cols <- gsub("fBodyBodyGyro", "FFT_Gyroscope_Body", cols)
names(DataReqCols) <- cols

#######
# DataReqCols is the final data table required for step 4

#Create the Tidy data for step 5
DataTidy <- DataReqCols
names(DataTidy) <- paste("Avg", cols, sep = "_")
DataTidy <- aggregate(DataTidy, by=list(Activity = DataTidy$Avg_Activity, Subject=DataTidy$Avg_Subject), mean)
DataTidy$Avg_Subject = NULL
DataTidy$Avg_Activity = NULL

#Write the tidy data set to a txt file.
write.table(DataTidy, "tidy_data.txt", sep = " ")

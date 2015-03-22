require(plyr)

#Read data
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE,col.names = c("ActivityId", "Activity"))
features <- read.table("UCI HAR Dataset/features.txt",colClasses = c("character"))

#Read Training set files
TrainSubj <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
TrainAct <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
TrainFeat <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

#Read Test set files
TestSubj <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
TestAct <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
TestFeat <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

#1.Merge Test and Train data sets

Subj <- rbind(TrainSubj, TestSubj)
Act <- rbind(TrainAct, TestAct)
Feat <- rbind(TrainFeat, TestFeat)
#Merge Columns: create full data table
FullTable <- cbind(Feat, Act, Subj)
# Label columns
sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(FullTable) <- sensor_labels

#2. Extract mean and std. deviation from the measurments

MeanTable <- FullTable[,grepl("mean|std|Subject|ActivityId", names(FullTable))]


#3. Uses descriptive activity names to name the activities in the data set

MeanTable <- join(MeanTable, activity_labels, by = "ActivityId", match = "first")
MeanTable <- MeanTable[,-1]

#4. Approprately label the data set with descriptive variable names.

#Remove punctuations, etc.
names(MeantTable) <- gsub('\\(|\\)',"",names(MeanTable, perl = TRUE)
#Change Acc to Acceleration using gsub
names(MeanTable)<-gsub("Acc", "Acceleration", names(MeanTable))
#Change Gyro to Gyroscope
names(MeanTable)<-gsub("Gyro", "Gyroscope", names(MeanTable))        
#Remove Body repetition and change label to Body
names(MeanTable)<-gsub("BodyBody", "Body", names(MeanTable))
#Clean up Mean and STD for better presentation
names(MeanTable)<-gsub('-mean()', "Mean", names(MeanTable), ignore.case = TRUE)
names(MeanTable)<-gsub("-std()", "StandardDeviation", names(MeanTable), ignore.case = TRUE)

#5. Create another independent data set w/ the average of each variable for each activity and subject using ddply

TidyTable = ddply(MeanTable, c("Subject","Activity"), numcolwise(mean))
write.table(MeanTable, file = "MeanTableTidy.txt", row.name = FALSE)






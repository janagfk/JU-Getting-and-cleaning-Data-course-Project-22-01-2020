
##Getting and Data cleaning Project.
rm(list=ls()) 


library("dplyr")
library("stringr")
library("quantmod")

path<-"G:/Jana/Other docs (training, induction, etc..)/R Training Coursera/Getting and Cleaning Data/Course Project/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/"

# 1. Merges the training and the test sets to create one data set.

##load data.

#load subjects.
subject_train<-read.table(paste0(path,"train/subject_train.txt"))
subject_test<-read.table(paste0(path,"test/subject_test.txt"))

#load activity labels
y_train <-read.table(paste0(path,"/train/y_train.txt"))
y_test <-read.table(paste0(path,"test/y_test.txt"))

#load data.
x_train <-read.table(paste0(path,"train/X_train.txt"))
x_test <-read.table(paste0(path,"test/X_test.txt"))


features<-read.table(paste0(path,"features.txt"))
activity_labels<-read.table(paste0(path,"activity_labels.txt"))

#merge train and test

#load subjects.
subject<-rbind(subject_train,subject_test)

#Load labels
Y<-rbind(y_train,y_test)
#load data
X<-rbind(x_train,x_test)

#combine all.
data<-cbind(subject,Y,X)

##check for duplicates.
sum(duplicated(features[,2],fromLast = TRUE)==duplicated(features[,2]))
grep("TRUE",duplicated(features[,2]))
grep("TRUE",duplicated(features[,2], fromLast = TRUE))

##name the colums using the features file
head(data)
names(data)[3:ncol(data)]<-as.character(features[,2])

names (data)
str (data)
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

sum(duplicated(names(data[,c(grep("mean|std",names(data)))])))
tidy.data<-data[,c(1,2,grep("mean|std",names(data)))]
str(tidy.data)

# 3. Uses descriptive activity names to name the activities in the data set
names (tidy.data)[1:2]<-c("Subject","Activity")
names(tidy.data)

# 4. Appropriately labels the data set with descriptive variable names.
names(activity_labels)[1]<-"Activity"
tidy.data<-merge(tidy.data,activity_labels, by="Activity")

##remove the index variable and reorder the labels one.
tidy.data<-tidy.data[,c(2,82,3:81)]
names(tidy.data)[2]<-"Activity"
str(tidy.data)

##clean up the variable names
names(tidy.data)<-gsub("Acc", "Accelerometer", names(tidy.data))
names(tidy.data)<-gsub("Gyro", "Gyroscope", names(tidy.data))
names(tidy.data)<-gsub("BodyBody", "Body", names(tidy.data))
names(tidy.data)<-gsub("Mag", "Magnitude", names(tidy.data))
names(tidy.data)<-gsub("^t", "Time", names(tidy.data))
names(tidy.data)<-gsub("^f", "Frequency", names(tidy.data))
names(tidy.data)<-gsub("tBody", "TimeBody", names(tidy.data))
names(tidy.data)<-gsub("-mean()", "Mean", names(tidy.data), ignore.case = TRUE)
names(tidy.data)<-gsub("-std()", "STD", names(tidy.data), ignore.case = TRUE)
names(tidy.data)<-gsub("-freq()", "Frequency", names(tidy.data), ignore.case = TRUE)
names(tidy.data)<-gsub("angle", "Angle", names(tidy.data))
names(tidy.data)<-gsub("gravity", "Gravity", names(tidy.data))

##check that it worked
str(tidy.data)


# 5. From the data set in step 4, creates a second, independent tidy data 
#   set with the average of each variable for each activity and each subject.


##create average summary
FinalData <- tidy.data %>%
    group_by(Subject, Activity) %>%
    summarise_all(funs(mean))

#write file.
write.table(FinalData, "FinalData.txt", row.name=FALSE)


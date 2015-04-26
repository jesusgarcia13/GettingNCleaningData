#
# Coursera "Getting and Cleaning Data" - class: getdata-013 (started Apr-06 2015)
# author: Jesus Garcia, jagel@msn.com
#

# Load Test files, "~/CP" is the Source Data folder

print ("Loading Test,Train and Label Files ...")
x_test <- read.table("CP/test/X_test.txt") # nrow(x_test): 2947 ; ncol(x_test): 561
y_test <- read.table("CP/test/y_test.txt") # nrow(y_test): 2947 ; ncol(y_test): 1
subject_test <- read.table("CP/test/subject_test.txt") # nrow(subject_test): 2947 ; ncol(subject_test): 1

# Load Train files
x_train <- read.table("CP/train/X_train.txt") # nrow(x_train): 7352 ; ncol(x_train): 561
y_train <- read.table("CP/train/y_train.txt") # nrow(y_train): 7352 ; ncol(y_train): 1
subject_train <- read.table("CP/train/subject_train.txt") # nrow(subject_train): 7352 ; ncol(subject_train): 1

# Load Activities and Features
activities <- read.table("CP/activity_labels.txt")
features <- read.table("CP/features.txt")

##
## 1) Merges the training and the test sets to create one data set.
##

print ("Merging datasets ...")
x_test <- rbind(x_test,x_train)
y_test <- rbind(y_test,y_train)
subject_test <- rbind(subject_test,subject_train)
 
##
## 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
##

# Identify "mean" & "std" columns by searching with grep all columns in Features$V2
mean_cols <- grep("mean",features$V2)
std_cols <- grep("std",features$V2)

##
## 3) Uses descriptive activity names to name the activities in the data set

print ("Creating tidy_set ...")

# Create Tidy Set, start with Subjects (col 1) and Activities (col 2)
tidy_set <- data.frame(subject_test$V1,y_test$V1)
 
# append "mean" columns to tidy set using cbind
for (i in 1:length(mean_cols))
     tidy_set <- cbind(tidy_set,x_test[,mean_cols[i]])
 
# append "std" columns to tidy set using cbind
for (i in 1:length(std_cols))
     tidy_set <- cbind(tidy_set,x_test[,std_cols[i]])

# (Optional) Add column names to tidy_set
colnames(tidy_set) <- c("Subject","Activity",as.character(features[c(mean_cols),2]), as.character(features[c(std_cols),2]))

# tidy_mean data frame will have the mean for each one of the 30 Subjects (1:30)
tidy_mean <- data.frame()

##
## 4 Appropriately labels the data set with descriptive variable names. 
##

# For each of the 30 Subjects
for (s in 1:30) # For each Subject
	for (a in 1:nrow(activities)) # For each 
	     tidy_mean <- rbind(tidy_mean,colMeans(tidy_set[tidy_set$Subject == s & tidy_set$Activity == a,],na.rm = TRUE))
		 
# Name the columns (verification purposes)
 colnames(tidy_mean) <- c("Subject","Activity",as.character(features[c(mean_cols),2]), as.character(features[c(std_cols),2]))

# Change Activity numbers to Strings/Factors

tidy_set$Activity <- as.character(tidy_set$Activity) 
tidy_mean$Activity <- as.character(tidy_mean$Activity) 

for( i in 1:nrow(activities) ) {
	tidy_set$Activity[tidy_set$Activity == i] <- as.character(activities$V2[i])
	tidy_mean$Activity[tidy_mean$Activity == i] <- as.character(activities$V2[i])
}

tidy_set$Activity <- as.factor(tidy_set$Activity) 
tidy_mean$Activity <- as.factor(tidy_mean$Activity) 
	
##
## 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##
write.table(tidy_mean, file = "tidy_mean.txt",row.names=FALSE, na="",col.names=FALSE, sep=",")
write.table(tidy_set, file = "tidy_set.txt",row.names=FALSE, na="",col.names=FALSE, sep=",")

print ("All Set")

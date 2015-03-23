# 1 Merges the training and the test sets to create one data set.
# 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3 Uses descriptive activity names to name the activities in the data set
# 4 Appropriately labels the data set with descriptive variable names. 
# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Load test & train files

x_test <- read.table("CP/test/X_test.txt")
y_test <- read.table("CP/test/y_test.txt")

x_train <- read.table("CP/train/X_train.txt")
y_train <- read.table("CP/train/y_train.txt")

#merge test & train files
x_set <-rbind(x_test,x_train)
y_set <-rbind(y_test,y_train)
 
#assign names 
colnames(x_set) <- features$V2
colnames(y_set) <- c("Subject")

# Activity Labels 1..6
activities <- read.table("CP/activity_labels.txt")
features <- read.table("CP/features.txt")

####################################

# Tody Set, start with y-set (subjects)
tidy_set <- data.frame(y_set)

# Identify mean, std and angle columns as vectors
mean_cols <- grep("mean",features$V2)
std_cols <- grep("std",features$V2)
angle_cols <- grep("angle",features$V2)
 
# append "mean" columns to tidy set 
for (i in 1:length(mean_cols)) { 
     tidy_set = cbind(tidy_set,x_set[,mean_cols[i]])
}
 
# append "std" columns to tidy set 
for (i in 1:length(std_cols)) { 
     tidy_set = cbind(tidy_set,x_set[,std_cols[i]])
}

# append "angle" columns to tidy set 
for (i in 1:length(angle_cols)) { 
     tidy_set = cbind(tidy_set,x_set[,angle_cols[i]])
}

#Add names to tody_set
colnames(tidy_set) <- c("Subject",as.character(features[c(mean_cols),2]), as.character(features[c(std_cols),2]),as.character(features[c(angle_cols),2]))

#save tidy-set to file
write.table(tidy_set,"tidy_set.txt",sep="\t",col.names='F')

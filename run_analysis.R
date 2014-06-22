#
#   TO RUN THIS PROGRAM MAKE SURE THAT
#   - The folder structure "UCI HAR Dataset" as created by unzipping "UCI HAR Dataset.zip"
#     downloaded from 
#       "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#     is in the working directory
#   - the packages "reshape2" and "sqldf" including dependents are installed

# read vectors of the subjects of the train/test observations and combine
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject <- rbind(subject_train, subject_test)

# read vectors of the activities of the train/test observations and combine
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
y <- rbind(y_train, y_test)

# read tables of the train/test observations and combine
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
X <-rbind(X_train,X_test)

# read "activity ordinal" (as used in y) -> "activity name" (as needed for tidy fata) 
# mapping table "activity_labels.txt"
activities <- read.table("UCI HAR Dataset/activity_labels.txt")

# replace activity ordinals in y with activity names in activities[,2]
for (i in 1:length(activities[,1])) {
    y[which(y$V1 == activities[i,1]),1] <- as.character(activities[i,2])
}
# convert y to factor
y$V1 <-as.factor(y$V1)

# read the table with the 561 feature ordinals and features names
features <-read.table("UCI HAR Dataset/features.txt")

# extract the "relevant" features, i.e. the means and standard deviations
# assumption: this is any feature containing either "mean()" or "std()" anywhere 
# in the feature name as listed in the feature table
relevant_features <- features[grepl ("mean\\()|std\\()", features[,2]),]

# The next two lines show alternative selections of relevant features which could also have been used:
# a) relevant_features <- features[grepl ("mean\\()$|std\\()$", features[,2]),]
#    any feature containing either "mean()" or "std()" at the end of the feature name
# b) relevant_features <- features[grepl ("[Mm]ean|[Ss]td", features[,2]),]
#    any feature containing "Mean", "mean", "Std", or "std" anywhere in the feature nakme

# make sure feature names are strings (not factors)
relevant_features[,2] <- as.character(relevant_features[,2])
# replace "funny" characters which might cause issues when used in a programming context:
# 1. "(" and ")" are deleted 
# 2. "-" and "," are replace by "_" 
relevant_features[,2] <- gsub("[\\(\\)]","",relevant_features[,2])
relevant_features[,2] <- gsub("[-,]","_",relevant_features[,2])

# Now in X, set column names of relevant features:
for (i in 1:length(relevant_features[,1])) {
    colnames(X)[relevant_features[i,1]] <- relevant_features[i,2]
}

# and keep only the columns with the relevant features
X <- X[,c(relevant_features[,1])]

# finally, add columns for subject and activity (vector y) to the left of table X
X <- cbind(subject,y,X)
# and lable them appropriately
colnames(X)[1:2] <- c("Subject","Activity")

# We now have a tidy albeit wide data set which we'll save to disk just incase...
write.table(X,"X_tidy_Means_and_Stds.csv", sep=",", row.names = FALSE)

# Now melt X into a skinny data set by making the observation (measure) type into a variable 
# Note that colnames(X)[1:2] are "Subject" and "Activity"
library(reshape2)
Xmelt <- melt (X, id=c("Subject","Activity"), measure.vars=colnames(X)[3:length(colnames(X))])

# Finally use sqldf to calculate the average of each variable per activity per subject
# and write the result to "X_tidy_Avg_of_Means_and_Stds.csv" in the current working directory
library(sqldf)
X2 <- sqldf("select Subject, Activity, Variable as Feature, avg(value) as Average 
            from Xmelt group by Subject, Activity, Variable")
# ... and write the required tidy data set to the working directory
write.table(X2,"X_tidy_Avg_of_Means_and_Stds.csv", sep=",", row.names = FALSE)

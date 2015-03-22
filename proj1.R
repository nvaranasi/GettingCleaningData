#Course Project for Getting and Cleaning Data

options(stringsAsFactors=FALSE)

#Libraries required
library(pastecs)
library(RODBC)
library(reshape2)
library(plyr)
library(stringr)

#File path
main.path <- "C:\\Users\\nvarana\\Desktop\\Training\\Getting_Cleaning_Data\\courseproject\\"
train.path <- "C:\\Users\\nvarana\\Desktop\\Training\\Getting_Cleaning_Data\\courseproject\\train\\"
test.path <- "C:\\Users\\nvarana\\Desktop\\Training\\Getting_Cleaning_Data\\courseproject\\test\\"

##Train Data
train.sub <- read.table(paste(train.path, "subject_train.txt", sep=""))
x.train <- read.table(paste(train.path, "X_train.txt", sep=""))
y.train <- read.table(paste(train.path, "y_train.txt", sep=""))

#Combine subject ids, activity ids and measurements
train.df <- cbind(train.sub, y.train, x.train)
names(train.df)[1] <- 'sub_id'
names(train.df)[2] <- 'act_id'

#Test Data
test.sub <- read.table(paste(test.path, "subject_test.txt", sep=""))
x.test <- read.table(paste(test.path, "X_test.txt", sep=""))
y.test <- read.table(paste(test.path, "y_test.txt", sep=""))

#Combine subject ids, activity ids and measurements
test.df <- cbind(test.sub, y.test, x.test)
names(test.df)[1] <- 'sub_id'
names(test.df)[2] <- 'act_id'

#1. Merge the training & test data sets to create one data set
df <- rbind(train.df, test.df)

#2. Extract only the measurements on the mean and standard deviation for each measurement
#Read column names text file 
features <- read.table(paste(main.path, "features.txt", sep=""))
measures.sub <- features[(grepl("mean()", features$V2)|grepl("std()", features$V2)),]
measures.sub$V1 <- paste("V", measures.sub$V1, sep="")
measures.sub$V2 <- gsub("-", "_", measures.sub$V2)
# measures.sub$V2 <- gsub("\\(\\)", "", measures.sub$V2)
i <- length(measures.sub$V1)
measures.sub[i+1,] <- c(names(df)[1]," ")
measures.sub[i+2,] <- c(names(df)[2], " ")  
df.sub <- df[,names(df) %in% measures.sub$V1]

#3. Uses descriptive activity names to name the activities in the data set
#Add the activity labels
activity.label <- read.table(paste(main.path, "activity_labels.txt", sep=""))
names(activity.label) <- c('act_id', 'act_desc')
df.sub <- merge(df.sub, activity.label, by="act_id", all.x=T)

#4. Appropriately lables the data set with descriptive label names
col_nums <- length(names(df.sub))-1 
for(i in 3:col_nums){
  names(df.sub)[i] <- measures.sub[i-2,"V2"]
}

#5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject
df.sub <- df.sub[order(df.sub$sub_id, df.sub$act_desc),]
df.sub$act_id <- NULL
tidy <- ddply(df.sub, .(sub_id, act_desc), numcolwise(mean))
write.table(tidy, paste(main.path,"tidy.txt", sep=""), row.names=F)




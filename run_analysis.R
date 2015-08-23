## Getting and Cleaning Data
## Course Project - August 2015

## File Description:
## 1. Merge the training and the test sets to create one data set.
## 2. Extract only the measurements on the mean and standard deviation 
## for each measurement. 
## 3. Use descriptive activity names to name the activities in the data set
## 4. Appropriately label the data set with descriptive activity names. 
## 5. Creates a second, independent tidy data set with the average of 
## each variable for each activity and each subject. 

## Dataset download location:
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## Question #1: Merges the training and the test sets to create one data set.

## Source data from files downloaded from the above address
Features    = read.table('./features.txt',header=FALSE); 
Activity    = read.table('./activity_labels.txt',header=FALSE);
Train       = read.table('./train/subject_train.txt',header=FALSE); 
xTrain      = read.table('./train/x_train.txt',header=FALSE); 
yTrain      = read.table('./train/y_train.txt',header=FALSE);
Test        = read.table('./test/subject_test.txt',header=FALSE);
xTest       = read.table('./test/x_test.txt',header=FALSE); 
yTest       = read.table('./test/y_test.txt',header=FALSE); 

## Create column names 
colnames(Activity)  = c('Activity_Id','Activity_Type');
colnames(Train)     = "Subject_Id";
colnames(xTrain)    = Features[,2]; 
colnames(yTrain)    = "Activity_Id";
colnames(Test)      = "Subject_Id";
colnames(xTest)     = Features[,2]; 
colnames(yTest)     = "Activity_Id";

## Merge
Train_Data   = cbind(yTrain,Train,xTrain);
Test_Data    = cbind(yTest,Test,xTest);
Complete_Set = rbind(Train_Data,Test_Data);

## Column vector creation
Column_Names  = colnames(Complete_Set);

## Question #2: Extracts only the measurements on the mean and 
## standard deviation for each measurement. 

Log_Vector = (grepl("activity..",Column_Names) | grepl("subject..",Column_Names) | 
                   grepl("-mean..",Column_Names) & !grepl("-meanFreq..",Column_Names) 
                 & !grepl("mean..-",Column_Names) | grepl("-std..",Column_Names) 
                 & !grepl("-std()..-",Column_Names));

Complete_Set = Complete_Set[Log_Vector==TRUE];

## Question #3: Uses descriptive activity names to name the activities in the data set.

Complete_Set = merge(Complete_Set,Activity,by ='Activity_Id',all.x=TRUE);

Column_Names  = colnames(Complete_Set); 

## Question #4: Appropriately labels the data set with descriptive variable names. 

for (i in 1:length(Column_Names)) 
{
  Column_Names[i] = gsub("\\()","",Column_Names[i])
  Column_Names[i] = gsub("-std$","Standard_Deviation",Column_Names[i])
  Column_Names[i] = gsub("-mean","Mean",Column_Names[i])
  Column_Names[i] = gsub("^(t)","Time",Column_Names[i])
  Column_Names[i] = gsub("^(f)","Frequency",Column_Names[i])
  Column_Names[i] = gsub("([Gg]ravity)","Gravity",Column_Names[i])
  Column_Names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",Column_Names[i])
  Column_Names[i] = gsub("[Gg]yro","Gyro",Column_Names[i])
  Column_Names[i] = gsub("AccMag","Acceleration_Magnitude",Column_Names[i])
  Column_Names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccel_JerkMagnitude",Column_Names[i])
  Column_Names[i] = gsub("JerkMag","Jerk_Magnitude",Column_Names[i])
  Column_Names[i] = gsub("GyroMag","Gyro_Magnitude",Column_Names[i])
};

Column_Names(Complete_Set) = Column_Names;

## Question #5: From the data set in step 4, creates a second, independent tidy data 
## set with the average of each variable for each activity and each subject.

NoActivityType  = Complete_Set[,names(Complete_Set) != 'Activity_Type'];

tidyData    = aggregate(NoActivityType[,names(NoActivityType) 
        != c('Activity_Id','Subject_Id')],by=list
        (Activity_Id=NoActivityType$Activity_Id,Subject_Id 
        = NoActivityType$Subject_Id),mean);

tidyData  = merge(tidyData,Activity,by='Activity_Id',all.x=TRUE);

write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');

## END

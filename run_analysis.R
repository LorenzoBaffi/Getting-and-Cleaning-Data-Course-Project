#library
library(dplyr)
library(tidyr)

#import features and rename variable labels
features <- read.table("./UCI HAR Dataset/features.txt")
names(features) <- c("ID_features", "features")

#import activity and rename variable labels
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
names(activity) <- c("ID_activity", "activity")

#function to import test or train dataframe. parameter must be "train" or "test"

import_df <- function (df) {

  #check correct input
  if (! df %in% c("test","train")){
    stop("parameter must be test or train")
  }
  
  
  #create directory for loading dataframe
  directory <- paste0("./UCI HAR Dataset/",df)
  
  #import measure dataframe
  df_meas <- read.table(paste0(directory,"/X_",df,".txt"))
  
  #rename variable names
  names(df_meas) <- features$features
  
  #import subject and rename variable label
  df_subject <- read.table(paste0(directory,"/subject_",df,".txt"))
  names(df_subject) <- "ID_subject"
  
  #import row labels, rename variable label and join activity df
  df_labels <- read.table(paste0(directory,"/y_",df,".txt"))
  names(df_labels) <- "ID_activity"
  
  df_labels <- df_labels %>%
    left_join(activity, by="ID_activity")
  
  #join data frame and select coloumn mean and standard deviation 
  df_meas_complete <- bind_cols(df_labels, df_subject, df_meas)
  
  df_meas_complete <- select(df_meas_complete, c(1:3, matches("(mean\\(\\)|std\\(\\))")))
  
  df_meas_complete <- gather(df_meas_complete, key="feature", value="measure", -(1:3))
   
  #otput function
  return(df_meas_complete)

  }

#import datset using previous function
df_test <- import_df("test")
df_train <- import_df("train")


#merge dataframe
df_complete <- bind_rows(df_test, df_train)

df_output <- df_complete %>%
  group_by(activity, ID_subject, feature)%>%
  summarise(avg=mean(measure))

write.table(df_output, file="df_output.txt",row.name=FALSE)

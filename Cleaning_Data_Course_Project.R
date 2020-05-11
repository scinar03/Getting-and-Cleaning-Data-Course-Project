#FInal Project
library(dplyr)
#####Loading the data#####
fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "./dt.zip", method = "curl")
#List all files and paths
as.character(unzip("./dt.zip", list = TRUE)$Name) 
#Saving all datasets
act_ref <- read.table(unzip("./dt.zip", files = "UCI HAR Dataset/activity_labels.txt", overwrite = TRUE))
features <- read.table(unzip("./dt.zip", files = "UCI HAR Dataset/features.txt", overwrite = TRUE))
#train sets:
tr_label <- read.table(unzip("./dt.zip", files = "UCI HAR Dataset/train/y_train.txt", overwrite = TRUE))
tr_set <- read.table(unzip("./dt.zip", files = "UCI HAR Dataset/train/X_train.txt", overwrite = TRUE))
tr_subject <- read.table(unzip("./dt.zip", files = "UCI HAR Dataset/train/subject_train.txt", overwrite = TRUE))
#test sets:
te_label <- read.table(unzip("./dt.zip", files = "UCI HAR Dataset/test/y_test.txt", overwrite = TRUE))
te_set <- read.table(unzip("./dt.zip", files = "UCI HAR Dataset/test/X_test.txt", overwrite = TRUE))
te_subject <- read.table(unzip("./dt.zip", files = "UCI HAR Dataset/test/subject_test.txt", overwrite = TRUE))

##########################################
# The instructions list 5 tasks that the script has to implement but they don't actually indicate
# that these steps has to be implemented in the provided order.So, my script does what is supposed to
# but in a bit different order compared to the instructions
###########################################

#####Label/feature Transformations#####

feature_headers<-as.vector(features[,2]) #extract featurs in to a vector
act_ref<-rename(act_ref, label=V2, label_key=V1) #Give the label column an actual name
tr_label<-rename(tr_label, label_key=V1) #Give the train labels the actual names
te_label<-rename(te_label, label_key=V1) #Give the test labels the actual names
tr_subject<-rename(tr_subject, subject=V1) #Label the train subject column
te_subject<-rename(te_subject, subject=V1) #Label the test subject column
#apply the actual feature headers to both sets:
names(tr_set)<-feature_headers
names(te_set)<-feature_headers

#####Filter and append the datasets#####

#filter both sets to mean and sd columns only (including meanFreq):
mean_sd_col<-feature_headers[grep("mean\\(|std\\(|meanFreq\\(", feature_headers)]
train<-subset(tr_set, select=mean_sd_col)
test<-subset(te_set, select=mean_sd_col)
#append labels and subjects to both sets
train<-cbind(train,tr_label,tr_subject)
test<-cbind(test,te_label,te_subject)
#apply the correct activity names to both sets using label keys
train<-merge(train,act_ref, by="label_key")
test<-merge(test,act_ref, by="label_key")
#test/train indicators just in case:
test$set<-"test"
train$set<-"train"
#Merge the train and test sets:
comb<-rbind(train, test)

#####Group Data#####

#Grouping: remove non-numericals->group by label->aggregate every group using mean:
comb_grouped<-comb%>%select(-c(label_key, set))%>%group_by(label,subject)%>%summarize_all(mean)

#####Write to a tidy dataset:#####

write.table(comb_grouped, "mean_results.txt", row.names = FALSE)

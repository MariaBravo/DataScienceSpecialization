## Results:
##========================================
## Activities names = 561
## Types of activities = 6
#1 WALKING
#2 WALKING_UPSTAIRS
#3 WALKING_DOWNSTAIRS
#4 SITTING
#5 STANDING
#6 LAYING

## 30 x 6 = 180 observations with 79 number of variables

## Training
## Train activities = 7352
## Corresponding activity code = 7352
## Corresponding subject code = 7352

## Test
## Test activities = 2947
## Corresponding activity code = 2947
## Corresponding subject code = 2947

## Final data = 180 rows, 81 columns
##========================================

library(data.table)

## Reading the activities names
read1 <- read.table("UCI HAR Dataset/features.txt", sep="", header=FALSE)
namesVarActivity <- as.data.table(read1)
char <- "\\()"

setnames(namesVarActivity, c("idActivity", "Activity"))
## I need to change the dash "-", comma "," and "\\()" characters
## inserted in the activities names
## for underscore "_" in order to operate later in the script
## with the fields names 
listNames <- c(as.character(namesVarActivity$Activity))
listNames <- gsub("-","_",listNames)
listNames <- gsub(",","_",listNames)
listNames <- gsub(char,"",listNames)


## Processing the training dataset
##-------------------------------------------------
##-------------------------------------------------
## Reading the activities measures at the training set
##
read2 <- read.table("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
datActivityTrain <- as.data.table(read2)

setnames(datActivityTrain, listNames)

## adding a column ID
colId <- c(1:nrow(datActivityTrain))
datActivityTrain <- cbind(colId, datActivityTrain)


## Reading the corresponding ID activity
##
read3 <- read.table("UCI HAR Dataset/train/y_train.txt", sep=" ", header=FALSE)
idActivityTrain <- as.data.table(read3)
## adding an ID
colId <- c(1:nrow(idActivityTrain))
idActivityTrain <- cbind(colId, idActivityTrain)
setnames(idActivityTrain, c("colId", "idActivity"))


## Reading the corresponding ID subject
##
read4 <- read.table("UCI HAR Dataset/train/subject_train.txt", sep=" ", header=FALSE)
idSubjectTrain <- as.data.table(read4)
colId <- c(1:nrow(idSubjectTrain))
idSubjectTrain <- cbind(colId, idSubjectTrain)
setnames(idSubjectTrain, c("colId", "idSubject"))

setkey(datActivityTrain, colId)
setkey(idActivityTrain, colId)
setkey(idSubjectTrain, colId)

datTrain <- datActivityTrain[idActivityTrain]
datTrain <- datTrain[idSubjectTrain]

## Processing the test dataset
##-------------------------------------------------
##-------------------------------------------------

## Reading the activities measures at the test set
##
read5 <- read.table("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
datActivityTest <- as.data.table(read5)

setnames(datActivityTest, listNames)

colId <- c(1:nrow(datActivityTest))
datActivityTest <- cbind(colId, datActivityTest)

## Reading the corresponding ID activity
##
read6 <- read.table("UCI HAR Dataset/test/y_test.txt", sep=" ", header=FALSE)
idActivityTest <- as.data.table(read6)
colId <- c(1:nrow(idActivityTest))
idActivityTest <- cbind(colId, idActivityTest)
setnames(idActivityTest, c("colId", "idActivity"))

## Reading the corresponding ID subject
##
read7 <- read.table("UCI HAR Dataset/test/subject_test.txt", sep=" ", header=FALSE)
idSubjectTest <- as.data.table(read7)
colId <- c(1:nrow(idSubjectTest))
idSubjectTest <- cbind(colId, idSubjectTest)
setnames(idSubjectTest, c("colId", "idSubject"))

## Setting data keys
setkey(datActivityTest, colId)
setkey(idActivityTest, colId)
setkey(idSubjectTest, colId)

datTest <- datActivityTest[idActivityTest]
datTest <- datTest[idSubjectTest]

##  Merging training and test datasets
## -----------------------------------
## -----------------------------------

datActivity <- rbind(datTrain, datTest)

##  Processing the merged data
## -----------------------------------
## -----------------------------------

listCol <- c("idSubject", "idActivity", grep("mean",listNames, value=TRUE), grep("std",listNames, value=TRUE))

datActivity <- datActivity[ ,listCol, with = FALSE]
setkeyv(datActivity, c("idSubject", "idActivity"))

DT <- data.table(idSubject=integer(0), idActivity=integer(0))
setkeyv(DT, c("idSubject", "idActivity"))

listColMean <-  c("idSubject", "idActivity")
for(i in 3:length(listCol))
{
    varMean <- paste("MeanOfVar", listCol[i], sep="_")
    listColMean <- c(listColMean, varMean)
    datActivity[,varMean := mean(get(listCol[i])), by=c("idSubject", "idActivity"), with = FALSE]       
}

datFinal <- unique(datActivity[,listColMean, with=FALSE])

## Formatting output
## -----------------
## -----------------

read8 <- read.table("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
lblActivity <- as.data.table(read8)

lblActivity$V2 <- tolower(lblActivity$V2)
substr(lblActivity$V2,1,1) <- toupper(substr(lblActivity$V2,1,1))
datFinal[idActivity==as.integer(lblActivity$V1), Activity:=lblActivity$V2]
lblActivity$V2 <- paste( toupper(substr(lblActivity$V2,1,1)), substr(lblActivity$V2,2,50), sep="")

datFinal[idActivity==lblActivity$V1, Activity:=lblActivity$V2 ]

listColTidy <- c("idSubject", "Activity")
for(i in 3:length(listCol))
{
    varTidy <- paste("MeanOfVar", listCol[i], sep="_")
    listColTidy <- c(listColTidy, varTidy)
}

datTidy <- datFinal[,listColTidy, with=FALSE]

## Writing  output
## -----------------
## -----------------

write.table(datTidy, file = "TidyData.txt", sep=",", row.names=FALSE)

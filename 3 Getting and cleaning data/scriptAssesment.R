
library(data.table)

read1 <- read.table("C:/Users/Luchy/Desktop/Coursera/Getting and cleaning data/UCI HAR Dataset/features.txt", sep="", header=FALSE)
namesVarActivity <- as.data.table(read1)
char <- "\\()"

setnames(namesVarActivity, c("idActivity", "Activity"))
listNames <- c(as.character(namesVarActivity$Activity))
listNames <- gsub("-","_",listNames)
listNames <- gsub(",","_",listNames)
listNames <- gsub(char,"",listNames)



##-------------------------------------------------
##-------------------------------------------------
read2 <- read.table("C:/Users/Luchy/Desktop/Coursera/Getting and cleaning data/UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
datActivityTrain <- as.data.table(read2)

setnames(datActivityTrain, listNames)

colId <- c(1:nrow(datActivityTrain))
datActivityTrain <- cbind(colId, datActivityTrain)

read3 <- read.table("C:/Users/Luchy/Desktop/Coursera/Getting and cleaning data/UCI HAR Dataset/train/y_train.txt", sep=" ", header=FALSE)
idActivityTrain <- as.data.table(read3)
colId <- c(1:nrow(idActivityTrain))
idActivityTrain <- cbind(colId, idActivityTrain)
setnames(idActivityTrain, c("colId", "idActivity"))

read4 <- read.table("C:/Users/Luchy/Desktop/Coursera/Getting and cleaning data/UCI HAR Dataset/train/subject_train.txt", sep=" ", header=FALSE)
idSubjectTrain <- as.data.table(read4)
colId <- c(1:nrow(idSubjectTrain))
idSubjectTrain <- cbind(colId, idSubjectTrain)
setnames(idSubjectTrain, c("colId", "idSubject"))


setkey(datActivityTrain, colId)
setkey(idActivityTrain, colId)
setkey(idSubjectTrain, colId)

datTrain <- datActivityTrain[idActivityTrain]
datTrain <- datTrain[idSubjectTrain]

##-------------------------------------------------
##-------------------------------------------------
##-------------------------------------------------
##-------------------------------------------------

read5 <- read.table("C:/Users/Luchy/Desktop/Coursera/Getting and cleaning data/UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
datActivityTest <- as.data.table(read5)

setnames(datActivityTest, listNames)

colId <- c(1:nrow(datActivityTest))
datActivityTest <- cbind(colId, datActivityTest)

read6 <- read.table("C:/Users/Luchy/Desktop/Coursera/Getting and cleaning data/UCI HAR Dataset/test/y_test.txt", sep=" ", header=FALSE)
idActivityTest <- as.data.table(read6)
colId <- c(1:nrow(idActivityTest))
idActivityTest <- cbind(colId, idActivityTest)
setnames(idActivityTest, c("colId", "idActivity"))

read7 <- read.table("C:/Users/Luchy/Desktop/Coursera/Getting and cleaning data/UCI HAR Dataset/test/subject_test.txt", sep=" ", header=FALSE)
idSubjectTest <- as.data.table(read7)
colId <- c(1:nrow(idSubjectTest))
idSubjectTest <- cbind(colId, idSubjectTest)
setnames(idSubjectTest, c("colId", "idSubject"))


setkey(datActivityTest, colId)
setkey(idActivityTest, colId)
setkey(idSubjectTest, colId)

datTest <- datActivityTest[idActivityTest]
datTest <- datTest[idSubjectTest]



datActivity <- rbind(datTrain, datTest)
    
listCol <- c("idSubject", "idActivity", grep("mean",listNames, value=TRUE), grep("std",listNames, value=TRUE))

datActivity <- datActivity[ ,listCol, with = FALSE]
setkeyv(datActivity, c("idSubject", "idActivity"))

##DT <- datActivity[c("idSubject", "idActivity"),by=c("idSubject", "idActivity")]
DT <- data.table(idSubject=integer(0), idActivity=integer(0))
setkeyv(DT, c("idSubject", "idActivity"))

listColMean <-  c("idSubject", "idActivity")
for(i in 3:length(listCol))
{
    varMean <- paste("MeanOf", listCol[i], sep="_")
    listColMean <- c(listColMean, varMean)
    datActivity[,varMean := mean(get(listCol[i])), by=c("idSubject", "idActivity"), with = FALSE]       
}

datFinal <- unique(datActivity[,listColMean, with=FALSE])




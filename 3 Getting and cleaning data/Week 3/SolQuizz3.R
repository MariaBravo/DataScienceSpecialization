

## Question 1
##***********************
ACR 1
Lot size
b .N/A (GQ/not a one-family house or mobile home)
1 .House on less than one acre
2 .House on one to less than ten acres
3 .House on ten or more acres

AGS 1
Sales of Agriculture Products
b .N/A (less than 1 acre/GQ/vacant/
            .2 or more units in structure)
1 .None
2 .$ 1 - $ 999
3 .$ 1000 - $ 2499
4 .$ 2500 - $ 4999
5 .$ 5000 - $ 9999
6 .$10000+
    

housingData <- read.table("housing.csv", sep=",", header=TRUE)
housingData <- data.frame(housingData)
housingData[c(1,2)] ## returns all rows for columns 1,2
housingData[c(1,2),] ## returns all columns rows 1,2
housingData[,c(1,2)] ## returns all columns rows 1,2

dt.tbl> DT[2]                      # 2nd row
x y v
1: a 3 2

dt.tbl> DT[,v]                     # v column (as vector)
[1] 1 2 3 4 5 6 7 8 9

dt.tbl> DT[,list(v)]               # v column (as data.table)
dataIris["Sepal.Length", "Sepal.Length"==5.9]


logicalVector <- housingData[,x:=((ACR==3) & (AGS==6))]
logicalVector <- logicalVector$x
which(logicalVector)


##*****************************************************************
## Question 2
##*****************************
library(jpeg)

z <- readJPEG("getdata_jeff.jpg", native=TRUE)
quantile(z, probs=c(0.3, 0.8), na.rm=TRUE)


##*****************************************************************
## Question 3
##*****************************

library(data.table)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileUrl, destfile="GDP", method="auto")

## "Reading GDP.R"


##GDPdata <- read.table("GDP.csv", sep=",",blank.lines.skip=TRUE, allowEscapes=TRUE, header=FALSE, strip.white=TRUE,  encoding="UTF-8", fill=TRUE, quote="", skip=5, as.is=TRUE, colClasses=c("character","character","character","character","character","character","character","character"), nrows=190)

##GDPdata <- data.table(GDPdata)

## To catch the V4 starting with a "
##bad <- !(substr(GDPdata$V4,1,1) %in% letters) & !(substr(GDPdata$V4,1,1) %in% LETTERS)
##GDPdata[bad, x:=1]

##substr(GDPdata$V4,start=2,stop=length(GDPdata$V4))

##
##GDPdata[,nchar(V3)] = 0
##.
##GDPdata[,nchar(V9)]
##GDPdata[,nchar(V10)]
##GDPdata[,nchar(V11)]
##GDPdata[,nchar(V12)]
##A <- scan("GDP.csv", skip=5, sep=",", blank.lines.skip=TRUE, allowEscapes=TRUE, what=list("","","","","","","","","","","",""), strip.white=c(TRUE), quote="", fill=TRUE)

"Reading EDU.R"

library(data.table)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileUrl, destfile="EDU.csv", method="auto")
EDUdata <- read.table("EDU.csv", sep=",",blank.lines.skip=TRUE, allowEscapes=TRUE, header=FALSE, strip.white=TRUE,  encoding="UTF-8", fill=TRUE, quote="", skip=11, as.is=TRUE, colClasses=c("character","character","character","character","character","character","character","character"))

dat1 <- merge(DTgdp, DTedu, by.x="V1", by.y="V1", all=FALSE)
library(plyr)
library(stringr)
dat2 <- dat1[,x:=as.numeric(str_trim(Rank))+0]
dat3 <- arrange(dat2, desc(x))

# Question 4
##----------------------------------------------------
dat4 <- arrange(dat3, V3)
dat4[,mean(x), by=V3]

##----------------------------------------------------

# Question 5
##----------------------------------------------------


brks <- with(dat4, quantile(x, probs = c(0, 0.20, 0.4, 0.6, 0.8, 1)))
dat4 <- within(dat4, quartile <- cut(x, breaks = brks, labels = 1:5, 
                include.lowest = TRUE))

table(dat4$quartile, dat4$V3)

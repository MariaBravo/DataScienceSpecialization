

fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
setInternet2(TRUE)
download.file(fileUrl, destfile="housing.csv", method="auto")
housingData <- read.table("housing.csv")






housingData <- read.table("housingW4.csv", sep=",", header=TRUE)
housingData <- data.frame(housingData)

splitdata <- strsplit(names(housingData), "wgtp")
splitdata[123]


"reading GDP week 4.R"
mean(as.numeric(list5))

countryNames <- list4
grep("*United",countryNames)
grep("^United",countryNames)



"reading EDU week 4.R"

dat1 <- merge(DTgdp, DTedu, by.x="V1", by.y="V1", all=FALSE)
vectorFound <- grep("^Fiscal year end: June",DTedu$V10)
dat1[vectorFound, V10]



library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn) 

DTamzn <- data.frame(amzn)
foundDates <- grep("^2012",row.names(DTamzn))
DT1 <- DTamzn[foundDates,]
listDates <- row.names(DT1)
listDates <- as.Date(listDates)
sum(weekdays(listDates)=="Monday")





write.csv(DTamzn , "zzz.txt")


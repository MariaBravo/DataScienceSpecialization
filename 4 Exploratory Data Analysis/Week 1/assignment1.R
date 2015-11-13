
## We will only be using data from the dates 2007-02-01 and 2007-02-02. 
## One alternative is to read the data from just those dates rather than 
## reading in the entire dataset and subsetting to those dates.


library(data.table)
library(lubridate)

dat1 <- read.table("household_power_consumption.txt",header=TRUE, sep=";", 
                   stringsAsFactors=FALSE,
                   colClasses=c("character","character","numeric", "numeric","numeric","numeric","numeric","numeric","numeric"),
                   na.strings="?")


dat1$NewDate <- dmy(dat1$Date)

vector <- dat1$NewDate == ymd("2007-02-01") | dat1$NewDate == ymd("2007-02-02")
dat2 <- dat1[vector,]

rm(vector)
rm(dat1)

png("plot1.png",width=480,height=480,units="px", bg="transparent", 
    type="cairo-png")

hist(dat2$Global_active_power, main="Global Active Power",
     col = "red", xlab="Global Active Power (kilowatts)")
dev.off()


##-------------------------------------------------------------


dat2$DateTime <- strptime(with( dat2, paste(NewDate, Time) ), "%Y-%m-%d %H:%M:%S")
Days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
dat2$Day_of_Week <- Days[day.of.week(month(dat2$DateTime),day(dat2$DateTime),year(dat2$DateTime)) + 1 ]

png("plot2.png",width=480,height=480,units="px", bg="transparent", 
    type="cairo-png")

with(dat2, plot(DateTime,Global_active_power, pch=NA, 
                ylab="Global Active Power (kilowatts)",
                xlab = ""))
lines(dat2$DateTime, dat2$Global_active_power, type="S")
dev.off()

##-------------------------------------

png("plot3.png",width=480,height=480,units="px", bg="transparent", 
    type="cairo-png")

with(dat2, plot(DateTime,Sub_metering_1, pch=NA, 
                ylab="Energy sub metering",
                xlab = ""))

lines(dat2$DateTime, dat2$Sub_metering_1, type="S", col="black")
lines(dat2$DateTime, dat2$Sub_metering_2, type="S", col="red")
lines(dat2$DateTime, dat2$Sub_metering_3, type="S", col="blue")
legend("topright", col=c("black", "red", "blue"), 
       legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       lty = c(1,1))

dev.off()
##-------------------------------------

png("plot4.png",width=480,height=480,units="px", 
    bg="transparent", type="cairo-png")

par(mfrow=c(2,2))
par(cex=0.7)

with(dat2, plot(DateTime,Global_active_power, pch=NA, 
                ylab="Global Active Power",
                xlab = ""))
lines(dat2$DateTime, dat2$Global_active_power, type="S")


##*****

par(xaxp=c(234,246,8))
with(dat2, plot(DateTime, Voltage, pch=NA, 
                ylab="Voltage",
                xlab = "datetime"))
lines(dat2$DateTime, dat2$Voltage, type="S")


##*****

with(dat2, plot(DateTime,Sub_metering_1, pch=NA, 
                ylab="Energy sub metering",
                xlab = ""))

lines(dat2$DateTime, dat2$Sub_metering_1, type="S", col="black")
lines(dat2$DateTime, dat2$Sub_metering_2, type="S", col="red")
lines(dat2$DateTime, dat2$Sub_metering_3, type="S", col="blue")
legend("topright", col=c("black", "red", "blue"), 
       legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       lty = c(1,1), bty="n")

##*****
##par(xaxp=c(0.0,0.5,0))
with(dat2, plot(DateTime, Global_reactive_power, pch=NA, 
                ylab="Global_reactive_power",
                xlab = "datetime"))
lines(dat2$DateTime, dat2$Global_reactive_power, type="S")


dev.off()

##-----------------------------------------------



##dat2 <- data.table(dat1)
##dat2[,DateTime:=paste(Date, Time)]

##sqldf("select * from dat1 where NewDate > 3")


##iris3 <- file("iris3.dat")
##sqldf("select * from iris3 where Sepal_Width > 3", file.format = list(sep = ";"))


##dat2[, DT:= strptime(dat2$DateTime, "%d/%m/%Y %H:%M:%S")]



##format(as.POSIXct(paste(D, Time)), "%d/%m/%Y %H:%M:%S") })

##as.POSIXct(strptime("2011-03-27 01:30:00", "%Y-%m-%d %H:%M:%S"))
##strptime(dat2$DateTime, "%d/%m/%Y %h:%m:%s")
##as.POSIXct(strptime(dat2$DateTime, "%d/%m/%Y %h:%m:%s"))


##strptime(dat2$DateTime, "%d/%m/%Y %H:%M:%S")



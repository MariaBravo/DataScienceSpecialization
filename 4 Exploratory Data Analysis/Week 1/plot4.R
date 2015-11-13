##------------------------------------------------------------------------------------------
## Goal: Examine how household energy usage varies over the period [2007-02-01, 2007-02-02]
## Task: Reconstruct Plot 4 using the base plotting system.
## Assumptions: 
##    1. The dataset "Electric power consumption" has been downloaded to the active 
##    working directory and has been saved as "household_power_consumption.txt".
##    2. Packages lubridate and chron has been installed.

## Notes:
##    1. We have been required to reconstruct Plot 4 saving it to a png file 
##    with a width of 480 pixels and a height of 480 pixels.
##    2. The dimensions of the png file provided as example are width = 504 pixels 
##    and height = 504 pixels.
##    3. This 32-bits depth example file has a transparent background that some
##    PNG viewers render as black.
##------------------------------------------------------------------------------------------    


library(lubridate)
library(chron)

# Reading the dataset coding the missing values as "?"
dat1 <- read.table("household_power_consumption.txt",header=TRUE, sep=";", 
                   stringsAsFactors=FALSE,
                   colClasses=c("character","character","numeric", "numeric","numeric","numeric","numeric","numeric","numeric"),
                   na.strings="?")

# Adding a valid date column
dat1$NewDate <- dmy(dat1$Date)

# Creating a logical vector in order to filter the data generated over the specified period
vector <- dat1$NewDate == ymd("2007-02-01") | dat1$NewDate == ymd("2007-02-02")
# Applying the filter
dat2 <- dat1[vector,]

# Removing objects from the workspace
rm(vector)
rm(dat1)

# Adding a valid dateTime column
dat2$DateTime <- strptime(with( dat2, paste(NewDate, Time) ), "%Y-%m-%d %H:%M:%S")
# Obtaining a Day_of_the_Week column
Days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
dat2$Day_of_Week <- Days[day.of.week(month(dat2$DateTime),day(dat2$DateTime),year(dat2$DateTime)) + 1 ]

# Launching the PNG graphics device
png("plot4.png",width=480,height=480,units="px", 
    bg="transparent", type="cairo-png")


# Setting par
par(mfrow=c(2,2))   ## 2 plots by row
par(cex=0.8)        ## reducing plotting text and symbols

# Creating the first plot 
with(dat2, plot(DateTime,Global_active_power, pch=NA, 
                ylab="Global Active Power",
                xlab = ""))
lines(dat2$DateTime, dat2$Global_active_power, type="S")



# Changing the coordinates of the extreme tick marks and the number of 
# intervals between tick-marks.
par(xaxp=c(234,246,8))

# Creating the second  plot 
with(dat2, plot(DateTime, Voltage, pch=NA, 
                ylab="Voltage",
                xlab = "datetime"))
lines(dat2$DateTime, dat2$Voltage, type="S")

# Creating the third  plot 
with(dat2, plot(DateTime,Sub_metering_1, pch=NA, 
                ylab="Energy sub metering",
                xlab = ""))
lines(dat2$DateTime, dat2$Sub_metering_1, type="S", col="black")
lines(dat2$DateTime, dat2$Sub_metering_2, type="S", col="red")
lines(dat2$DateTime, dat2$Sub_metering_3, type="S", col="blue")
legend("topright", col=c("black", "red", "blue"), 
       legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       lty = c(1,1), bty="n")

# Creating the fourth plot 
with(dat2, plot(DateTime, Global_reactive_power, pch=NA, 
                ylab="Global_reactive_power",
                xlab = "datetime"))
lines(dat2$DateTime, dat2$Global_reactive_power, type="S")

# Closing the PNG graphics device
dev.off()

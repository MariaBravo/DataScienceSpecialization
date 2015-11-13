
##  Q2
##  1. Have total emissions from PM2.5 decreased in the Baltimore City, 
##  Maryland (fips == 24510) from 1999 to 2008? 
##  2. Use the base plotting system to make a plot answering this question.
##  3. Upload a PNG file containing your plot addressing this question.


## Reading
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(data.table)
SCCdt <- data.table(SCC) 

NEIdt <- data.table(NEI) 
rm(NEI)

library(labeling)

## Total emissions by Baltimore + year
NEIyear24510 <- NEIdt[fips =="24510", ]
NEIyear24510 <- NEIyear24510[, z:=sum(Emissions),  by=year]
NEIyear24510$z <- round(NEIyear24510$z,3)
NEIyear24510 <- unique(NEIyear24510, by="year")

png("plot2.png",width=480,height=480,units="px", bg="transparent", 
    type="cairo-png")

bp <- barplot(NEIyear24510$z/1000, legend=NEIyear24510$year, 
              main = "PM2.5 emissions in Baltimore City from 1999 to 2008",
              ylab = "Emission (Thousand Tons)",
              ylim = c(0,3.5),
              cex.axis = 0.8,
              names.arg = NEIyear24510$year,        
              col = c("red3", "violet", "gold","green3"))

text(bp, NEIyear24510$z/1000+0.1, round(NEIyear24510$z/1000,3), cex=0.8)

dev.off()


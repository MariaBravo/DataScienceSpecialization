##  Q1
##  1. Have total emissions from PM2.5 decreased in the United States
##  from 1999 to 2008? 
##  2. Using the base plotting system, 
##  make a plot showing the total PM2.5 emission from all sources 
##  for each of the years 1999, 2002, 2005, and 2008.
##  3. Upload a PNG file containing your plot addressing this question.

## Reading
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(data.table)
SCCdt <- data.table(SCC) 

NEIdt <- data.table(NEI) 
rm(NEI)

library(labeling)

## Total emissions by year
NEIyear <- NEIdt[, z:=sum(Emissions),  by=year]
NEIyear <- unique(NEIyear, by="year")

png("plot1.png",width=480,height=480,units="px", bg="transparent", 
    type="cairo-png")

bp <- barplot(NEIyear$z/1000, legend=NEIyear$year, 
              main = "PM2.5 emissions in the United States from 1999 to 2008",
              ylab = "Emission (Thousand Tons)",
              ylim = c(0,8000),
              cex.axis = 0.8,
              names.arg = NEIyear$year,        
              col = c("red3", "violet", "gold","green3"))

text(bp, NEIyear$z/1000+130 , round(NEIyear$z/1000,3), cex=0.8)

dev.off()

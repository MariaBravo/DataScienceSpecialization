##  Q3
##  1. Of the four types of sources indicated by the type (point, nonpoint, 
##  onroad, nonroad) variable, which of these four sources have seen 
##  decreases in emissions from 1999-2008 for Baltimore City? 
##  2. Which have seen increases in emissions from 1999-2008? 
##  Use the ggplot2 plotting system to make a plot answer this question.
##  Upload a PNG file containing your plot addressing this question.


## Reading
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(data.table)
SCCdt <- data.table(SCC) 

NEIdt <- data.table(NEI) 
rm(NEI)

library(ggplot2)
library(labeling)

## Total emissions for Baltimore by type + year
NEItypeYear24510 <- NEIdt[fips =="24510", ]
NEItypeYear24510 <- NEItypeYear24510[, z:=sum(Emissions),  by=list(type,year)]
NEItypeYear24510 <- unique(NEItypeYear24510, by=c("type", "year"))

png("plot3.png",width=480,height=480,units="px", bg="transparent", 
    type="cairo-png")

ggplot(NEItypeYear24510, aes(as.factor(year), z, fill=year) ) + 
    geom_bar(stat="identity", binwidth = 0.7) + 
    facet_wrap(~ type) +
    labs(title = "Emissions from 1999-2008 for Baltimore City") + 
    labs(x = "Year", y = "Emission (Tons)") 
    
dev.off()

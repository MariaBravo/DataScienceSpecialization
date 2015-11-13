
##  Q5
##  emissions from motor vehicle sources
##  1. How have emissions from motor vehicle sources changed from 1999-2008 
##  in Baltimore City?
##  2.  Upload a PNG file containing your plot addressing this question.

## Reading
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(data.table)
SCCdt <- data.table(SCC) 

NEIdt <- data.table(NEI) 
rm(NEI)

library(ggplot2)
library(labeling)

## Filtering to rows with the value "Onroad" at column Category
SCConRoad <- SCCdt[Data.Category == "Onroad"]
list2 <- as.character(SCConRoad$SCC)
NEIonRoad <- NEIdt[SCC %in% list2,]

## Filtering to fips="Baltimore" and getting totals by year
NEIonRoad24510 <- NEIonRoad[fips =="24510", ]
NEIonRoadYear24510 <- NEIonRoad24510[, z:=sum(Emissions),  by="year"]
NEIonRoadYear24510 <- unique(NEIonRoadYear24510, by="year")

png("plot5.png",width=480,height=480,units="px", bg="transparent", 
    type="cairo-png")

g <- qplot(factor(year), z, data = NEIonRoadYear24510, 
           fill=factor(year))
g + geom_bar(stat = "identity", width = 0.7) +
    labs(title = "Emissions from motor vehicle sources 
         in Baltimore City (1999-2008)") + 
    labs(x = "Year", y = "Emission (Thousands Tons)") + 
    geom_text(aes(label = round(z,3), y=z+10, ymax=z), 
              size=4, position="dodge") +
    scale_fill_discrete("")

dev.off()

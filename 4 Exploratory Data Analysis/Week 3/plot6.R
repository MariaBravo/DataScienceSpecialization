##  Q6
##  emissions from motor vehicle sources in Baltimore City (fips == "24510")
##  Los Angeles County, California (fips == "06037") 

##  1. Compare emissions from motor vehicle sources in Baltimore City with emissions 
##  from motor vehicle sources in Los Angeles County, California (fips == 06037). 
##  2. Which city has seen greater changes over time in motor vehicle emissions?
##  3. Upload a PNG file containing your plot addressing this question.


## Reading
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(data.table)
SCCdt <- data.table(SCC) 

NEIdt <- data.table(NEI) 
rm(NEI)

library(ggplot2)
library(labeling)

## Filtering to fips="Baltimore" and fips="LA County" and getting totals by year
NEIonRoadcomp <- NEIonRoad[fips =="24510" | fips =="06037", ]
NEIonRoadYearcomp <- NEIonRoadcomp[, z:=sum(Emissions),  by=c("fips","year")]
NEIonRoadYearcomp <- unique(NEIonRoadYearcomp, by=c("fips","year"))

png("plot6.png",width=480,height=480,units="px", bg="transparent", 
    type="cairo-png")

ggplot(data = NEIonRoadYearcomp, 
       aes(x=factor(year), y=z, fill=fips)) + 
    geom_bar(stat="identity", position=position_dodge(), colour="black") +
    scale_fill_discrete("", labels=c("Baltimore", "LA County")) +    
    labs(title = "Emissions from motor vehicle sources in 
         Baltimore and Los Angeles County (1999-2008)") +
    labs(x = "Year", y = "Emission (Tons)") +
    geom_text(aes(label = round(z,3), x=as.factor(year), y=z+80, ymax=z), 
              size=4)

dev.off()

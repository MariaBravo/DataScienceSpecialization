
## Q4
## emissions from coal combustion-related sources
## SCCsector <- unique(SCCdt, by="EI.Sector")

##  1. Across the United States, how have emissions from coal combustion-related 
##  sources changed from 1999-2008?
##  2. Upload a PNG file containing your plot addressing this question.


## Reading
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(data.table)
SCCdt <- data.table(SCC) 

NEIdt <- data.table(NEI) 
rm(NEI)

library(ggplot2)
library(labeling)

## Total emissions from coal combustion-related
## Using the rows with the column Short.Name containing the word "coal"
v <- grep("coal", SCCdt$Short.Name, ignore.case = TRUE)
SCCcoal <- SCCdt[v]
SCCcoal$SCC <- as.character(SCCcoal$SCC)
list1 <- as.character(SCCcoal$SCC)
NEIcoal <- NEIdt[SCC %in% list1,]

## Getting totals by year
NEItypeYearCoal <- NEIcoal[, z:=sum(Emissions),  by=year]
NEItypeYearCoal <- unique(NEItypeYearCoal, by="year")


png("plot4.png",width=480,height=480,units="px", bg="transparent", 
    type="cairo-png")

g <- qplot(factor(year), z/1000, data = NEItypeYearCoal, 
           fill=factor(year))
g + geom_bar(stat = "identity", width = 0.7) +
    scale_fill_discrete("") +    
    labs(title = "Emissions from coal combustion-related sources 
         in United States (1999-2008)") + 
    labs(x = "Year", y = "Emission (Thousands Tons)") + 
    geom_text(aes(label = round(z/1000, 3), y=z/1000+10, ymax=z/1000+10), 
              size=4, position="dodge")

dev.off()


##Questions

##  You must address the following questions and tasks in your exploratory analysis. 
##  For each question/task you will need to make a single plot. 
##  Unless specified, you can use any plotting system in R to make your plot.

##  1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
##  Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
##  for each of the years 1999, 2002, 2005, and 2008.

##  2. Have total emissions from PM2.5 decreased in the Baltimore City, 
##  Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system 
##  to make a plot answering this question.

##  3. Of the four types of sources indicated by the type 
##  (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen 
##  decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases 
##  in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.

##  4. Across the United States, how have emissions from coal combustion-related sources 
##  changed from 1999-2008?

##  5. How have emissions from motor vehicle sources changed from 1999-2008 
##  in Baltimore City?

##  6. Compare emissions from motor vehicle sources in Baltimore City with emissions 
##  from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
##  Which city has seen greater changes over time in motor vehicle emissions?


## Reading
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(data.table)
SCCdt <- data.table(SCC) 

NEIdt <- data.table(NEI) 
rm(NEI)

library(ggplot2)
library(labeling)

##  options(scipen=999)

##  Q1
##  1. Have total emissions from PM2.5 decreased in the United States
##  from 1999 to 2008? 
##  2. Using the base plotting system, 
##  make a plot showing the total PM2.5 emission from all sources 
##  for each of the years 1999, 2002, 2005, and 2008.
##  3. Upload a PNG file containing your plot addressing this question.

NEIyear <- NEIdt[, z:=sum(Emissions),  by=year]
NEIyear <- unique(NEIyear, by="year")
##NEIyear$year <- as.numeric(NEIyear$year)

bp <- barplot(NEIyear$z/1000, legend=NEIyear$year, 
        main = "PM2.5 emissions in the United States from 1999 to 2008",
        ylab = "Emission (Thousand Tons)",
        ylim = c(0,8000),
        cex.axis = 0.8,
        names.arg = NEIyear$year,        
        col = c("red3", "violet", "gold","green3"))

text(bp, NEIyear$z/1000+130 , round(NEIyear$z/1000,3), cex=0.8)


##-------------------------------------------
##-------------------------------------------

##hist(NEIyear$Emissions, NEIyear$Year)
par(lab=c(8, 8, 10))
                    
##with(NEIyear, plot( year,  Emissions, type="b",pch=46, cex=8,
##                ylab="", lab=c(5, 5, 10),
##                xlab = NULL))

with(NEIyear, plot( year,  z/1000, type="b", 
                    xlim=c(1998,2010),
                    ylim=c(3000,7500),
                    ylab = "Emission (Thousand Tons)",
                    xlab = "Year",cex.lab = 0.8,
                    pch=46, cex=8,
                    axes = FALSE))

axis(side=1, at=NEIyear$year, cex.axis=0.8, las = 1)
axis(side=2, at=trunc(NEIyear$z/1000,0), cex.axis=0.7, las = 1)

##text(NEIyear$year-0.4, NEIyear$z/1000-170, paste(NEIyear$year), 
##     cex=0.7, adj=c(0,1))

text(NEIyear$year-0.4, NEIyear$z/1000-80, paste(round(NEIyear$z/1000,3)), 
     cex=0.7, adj=c(0,1))
##-------------------------------------------
##-------------------------------------------


##  Q2
##  1. Have total emissions from PM2.5 decreased in the Baltimore City, 
##  Maryland (fips == 24510) from 1999 to 2008? 
##  2. Use the base plotting system to make a plot answering this question.
##  3. Upload a PNG file containing your plot addressing this question.

NEIyear24510 <- NEIdt[fips =="24510", ]
NEIyear24510 <- NEIyear24510[, z:=sum(Emissions),  by=year]
NEIyear24510$z <- round(NEIyear24510$z,3)
NEIyear24510 <- unique(NEIyear24510, by="year")



bp <- barplot(NEIyear24510$z/1000, legend=NEIyear24510$year, 
              main = "PM2.5 emissions in Baltimore City from 1999 to 2008",
              ylab = "Emission (Thousand Tons)",
              ylim = c(0,3.5),
              cex.axis = 0.8,
              names.arg = NEIyear24510$year,        
              col = c("red3", "violet", "gold","green3"))

text(bp, NEIyear24510$z/1000+0.1, round(NEIyear24510$z/1000,3), cex=0.8)


##-------------------------------------------
##-------------------------------------------

with(NEIyear24510, plot( year,  z, type="b", 
                    xlim=c(1998,2010),
                    ylim=c(1500,3500),
                    ylab = "Emission (Tons)",
                    xlab = "Year",cex.lab = 0.8,
                    pch=46, cex=8,
                    axes = FALSE))

axis(side=1, at=NEIyear24510$year, cex.axis=0.8, las = 1)
axis(side=2, at=trunc(NEIyear24510$z,0), cex.axis=0.7, las = 1)

text(NEIyear24510$year, NEIyear24510$z-50, paste(round(NEIyear24510$z,3)), 
     cex=0.7, adj=c(0,1))

##-------------------------------------------
##-------------------------------------------

##  Q3
##  1. Of the four types of sources indicated by the type (point, nonpoint, 
##  onroad, nonroad) variable, which of these four sources have seen 
##  decreases in emissions from 1999-2008 for Baltimore City? 
##  2. Which have seen increases in emissions from 1999-2008? 
##  Use the ggplot2 plotting system to make a plot answer this question.
##  Upload a PNG file containing your plot addressing this question.


NEItypeYear24510 <- NEIdt[fips =="24510", ]
NEItypeYear24510 <- NEItypeYear24510[, z:=sum(Emissions),  by=list(type,year)]
NEItypeYear24510 <- unique(NEItypeYear24510, by=c("type", "year"))

g <- qplot(year, z, data = NEItypeYear24510,
      facets = .~ type)
g + geom_line() +   labs(title = "Emissions from 1999-2008 for Baltimore City") + 
                    labs(x = "Year", y = "Emission (Tons)")

## Q4
## emissions from coal combustion-related sources
## SCCsector <- unique(SCCdt, by="EI.Sector")

##  1. Across the United States, how have emissions from coal combustion-related 
##  sources changed from 1999-2008?
##  2. Upload a PNG file containing your plot addressing this question.

v <- grep("coal", SCCdt$Short.Name, ignore.case = TRUE)
SCCcoal <- SCCdt[v]
SCCcoal$SCC <- as.character(SCCcoal$SCC)
list1 <- as.character(SCCcoal$SCC)
NEIcoal <- NEIdt[SCC %in% list1,]

NEItypeYearCoal <- NEIcoal[, z:=sum(Emissions),  by=year]
NEItypeYearCoal <- unique(NEItypeYearCoal, by="year")


g <- qplot(factor(year), z/1000, data = NEItypeYearCoal, 
           fill=factor(year))
g + geom_bar(stat = "identity", width = 0.7) +
    labs(title = "Emissions from 1999-2008 for 
            coal combustion-related sources in United States") + 
    labs(x = "Year", y = "Emission (Thousands Tons)") + 
    geom_text(aes(label = round(z/1000, 3), y=z/1000+10, ymax=z/1000+10), 
              size=4, position="dodge")


##----------------------------------
##----------------------------------

g <- qplot(year, z, data = NEItypeYearCoal)
g + geom_line() +   labs(title = "Emissions from 1999-2008 for 
                    coal combustion-related sources in United States") + 
                    labs(x = "Year", y = "Emission (Tons)")
##----------------------------------
##----------------------------------


##  Q5
##  emissions from motor vehicle sources
##  1. How have emissions from motor vehicle sources changed from 1999-2008 
##  in Baltimore City?
##  2.  Upload a PNG file containing your plot addressing this question.

SCConRoad <- SCCdt[Data.Category == "Onroad"]
list2 <- as.character(SCConRoad$SCC)
NEIonRoad <- NEIdt[SCC %in% list2,]

NEIonRoad24510 <- NEIonRoad[fips =="24510", ]
NEIonRoadYear24510 <- NEIonRoad24510[, z:=sum(Emissions),  by="year"]
NEIonRoadYear24510 <- unique(NEIonRoadYear24510, by="year")


g <- qplot(factor(year), z, data = NEIonRoadYear24510, 
           fill=factor(year))
g + geom_bar(stat = "identity", width = 0.7) +
    labs(title = "Emissions from 1999-2008 for 
                motor vehicle sources in Baltimore City") + 
    labs(x = "Year", y = "Emission (Thousands Tons)") + 
    geom_text(aes(label = round(z,3), y=z+10, ymax=z), 
              size=4, position="dodge")


##-------------------------------------
##-------------------------------------

g <- qplot(year, z, data = NEIonRoadYear24510)
g + geom_line() +   labs(title = "Emissions from 1999-2008 for 
                         motor vehicle sources in Baltimore City") + 
                        labs(x = "Year", y = "Emission (Tons)")

##-------------------------------------
##-------------------------------------


##  Q6
##  emissions from motor vehicle sources in Baltimore City (fips == "24510")
##  Los Angeles County, California (fips == "06037") 

##  1. Compare emissions from motor vehicle sources in Baltimore City with emissions 
##  from motor vehicle sources in Los Angeles County, California (fips == 06037). 
##  2. Which city has seen greater changes over time in motor vehicle emissions?
##  3. Upload a PNG file containing your plot addressing this question.

NEIonRoadcomp <- NEIonRoad[fips =="24510" | fips =="06037", ]
NEIonRoadYearcomp <- NEIonRoadcomp[, z:=sum(Emissions),  by=c("fips","year")]
NEIonRoadYearcomp <- unique(NEIonRoadYearcomp, by=c("fips","year"))
##NEIonRoadYearcomp$year <- as.factor(NEIonRoadYearcomp$year)


ggplot(data = NEIonRoadYearcomp, 
    aes(x=factor(year), y=z, fill=fips)) + 
    geom_bar(stat="identity", position=position_dodge(), colour="black") +
    scale_fill_manual(values=c("lightblue1", "orangered"))  +
    labs(title = "Emissions from 1999-2008 comparative") + 
    labs(x = "Year", y = "Emission (Tons)") +
    geom_text(aes(label = round(z,3), x=as.factor(year), y=z+80, ymax=z), 
          size=4)


##-------------------------------------
##-------------------------------------

g <- qplot(year, z,
      data = NEIonRoadYearcomp,
      color = fips)
g + geom_line() +   labs(title = "Emissions from 1999-2008 comparative") + 
    labs(x = "Year", y = "Emission (Tons)")


##-------------------------------------
##-------------------------------------


##g <- qplot(year, z, data = NEIonRoadYearcomp,
##           facets = .~ fips)
##g + geom_line() +   labs(title = "Emissions from 1999-2008 comparative") + 
##    labs(x = "Year", y = "Emission (Tons)")


#with(NEIonRoadYear24510, 
#     xyplot(z ~ year ,  
#            lwd = 2, type="l",
#            pch = 46,
#            auto.key = TRUE,
#            xlab="Year", ylab = "Emission", 
#            col.line="red"))

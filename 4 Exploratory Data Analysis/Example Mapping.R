library(maps)
library(ggplot2)
library(data.table)

# Download data file and unzip it if the required files do not exist in the current working directory
if (! file.exists("summarySCC_PM25.rds") | ! file.exists("Source_Classification_Code.rds")) {
    if (! file.exists("exdata.zip")) {
        print("Downloading zip data file")
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "exdata.zip", method = "wget")
    }
    print("Unzipping the rds files")
    unzip("exdata.zip")
}

# Load Data
NEI <- as.data.table(readRDS("summarySCC_PM25.rds"))

# Subset for year 2008
NEI2008 <- NEI[NEI$year == "2008", ]

# sum per fips per year and prepare data frame with fips, years, emissions
dt <- NEI2008[,sum(Emissions), by = list(fips, year)]
setnames(dt, c("fips", "year", "Emissions"))
dt$Emissions <- round(dt$Emissions / 1000, digits = 2)
dt$fips <- as.integer(as.character(dt$fips))
dt$year <- as.integer(as.character(dt$year))
dt <- na.omit(dt)

# get fips to county data and create data table with
# with region (state) and sub region (county)
setkey(dt, fips)
data(county.fips)
county_fips <- data.table(county.fips)
setkey(county_fips, fips)
dt <- dt[county_fips]
dt$region <- sub(",.*", "", dt$polyname)
dt$subregion <- sub(".*,", "", dt$polyname)

# prepare a date frame with plotting data and emissions
# basic idea for the plot from
# http://stackoverflow.com/questions/23714052/ggplot-mapping-us-counties-problems-with-visualization-shapes-in-r
# http://stackoverflow.com/users/2985007/jlhoward
setkey(dt,"region", "subregion")
all_county <- data.table(map_data('county'))
setkey(all_county,region,subregion)
dtplot <- all_county[dt]

# plot the data
# basic idea from
# http://www.r-bloggers.com/us-state-maps-using-map_data/
png("choropleth_county.png", width = 1024, height = 1024)
p <- ggplot()
p <- p + geom_polygon(data=dtplot, aes(x=long, y=lat, group = group, fill = Emissions), colour="white") +
    scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar") +
    coord_map() +
    labs(fill = "Emissions in Kilo Ton ", title = "Emissions of " ~ PM[2.5] ~ "across US - 2008") +
    xlab("Longitude") +
    ylab("Latitude")
print(p)
dev.off()
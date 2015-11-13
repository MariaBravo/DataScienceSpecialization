pollutantmean <- function(directory, pollutant, id = 1:332) 
{
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
    totdat <- numeric()
    for(i in id)
    {        
        if (i < 10) 
        {
            fileName <- paste("00",i,".csv", sep="")
        }
 
        if (i < 100 & i >= 10) 
        {
            fileName <- paste("0",i,".csv", sep="")
        }
        
        if (i >= 100) 
        {
            fileName <- paste(i,".csv", sep="")
        }
        fileName <- paste(directory,"/",fileName,sep="")
        pollutantData <- read.table(fileName, sep=",", header=TRUE)
        dat <- as.data.table(pollutantData)
        coldat <- dat[[pollutant]]
        bad <- is.na(coldat)
        coldat <- coldat[!bad]
        totdat <- c(totdat, coldat)
    }

    meandat <- mean(totdat, na.rm = TRUE)        
    print(meandat)
    
    
}
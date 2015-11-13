complete <- function(directory, pid = 1:332) 
{
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    a_data_frame <- data.frame()
    for(id in pid)
    {        
        if (id < 10) 
        {
            fileName <- paste("00",id,".csv", sep="")
        }
        
        if (id < 100 & id >= 10) 
        {
            fileName <- paste("0",id,".csv", sep="")
        }
        
        if (id >= 100) 
        {
            fileName <- paste(id,".csv", sep="")
        }
        fileName <- paste(directory,"/",fileName,sep="")
        pollutantData <- read.table(fileName, sep=",", header=TRUE)
        dat <- as.data.table(pollutantData)
        
        nobs <- sum(!(is.na(dat$nitrate) | is.na(dat$sulfate)))
        a_data_frame <- rbind(a_data_frame, c(id, nobs))
        
        
    }   
    colnames(a_data_frame) <- c("id","nobs")
    return(a_data_frame)
}
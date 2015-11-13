corr <- function(directory, threshold = 0) 
{
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations

    res <- numeric()
    listFiles <- Sys.glob(paste(directory,"/*.csv",sep=""))
    a_data_frame <- data.frame()
    for(id in listFiles)
    {        
        pollutantData <- read.table(id, sep=",", header=TRUE)
        dat <- as.data.table(pollutantData)
        
        nobs <- sum(!(is.na(dat$nitrate) | is.na(dat$sulfate)))
        if (nobs > threshold)
        {
            cordat <- dat[!(is.na(dat$nitrate) | is.na(dat$sulfate))]
            
            x <- cor(cordat$sulfate,cordat$nitrate)
            res <- c(res, x)
        }
    }   
    return(res)
}
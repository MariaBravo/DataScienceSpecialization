
best <- function(state, outcome) 
{
    
    ## Read outcome data
    outcomeDT <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    outcomeDT[, 11] <- as.numeric(outcomeDT[, 11])
    outcomeDT[, 17] <- as.numeric(outcomeDT[, 17])
    outcomeDT[, 23] <- as.numeric(outcomeDT[, 23])
    
    outcomeList <- c("heart attack", 
                     "heart failure", 
                      "pneumonia")

    rowNumber <- 0
    ## Check that state and outcome are valid
    stateList <- unique(outcomeDT[,7])
    if (!(state %in% stateList))
    {
        stop("invalid state")   
    }
    else
    {
        if (!(outcome %in% outcomeList))
        {
            stop("invalid outcome")               
        }        
        else
        {
            if (outcome == outcomeList[1])
            {
                outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", "Heart.Attack", sep="")
                rowNumber <- 11
            }
            else
            {
                if (outcome == outcomeList[2])
                {
                    outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", "Heart.Failure", sep="")
                    rowNumber <- 17
                    
                }
                else
                {
                    outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", "Pneumonia", sep="")                 
                    rowNumber <- 23
                    
                }
            }
            ## Return hospital name in that state with lowest 30-day death
            ## rate
            d1 <- outcomeDT[outcomeDT$State== state,]
            
            bad <- is.na(with(d1, get(outcome) ))
            
            d2 <- d1[!bad,]
            sortVector1 <- sort(d2[,2],na.last=NA, index.return=TRUE)                         
            d3 <- d2[sortVector1$ix,]
            
            sortVector2 <- sort(d3[,rowNumber],na.last=NA, index.return=TRUE)             
            d4 <- head(d3[sortVector2$ix,],1)
            ##print(head(d3,3))
            return(d4[[2]])
        }

    }
}


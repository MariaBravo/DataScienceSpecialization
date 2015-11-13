rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    library(reshape2)
    library(data.table)    
    ## Read outcome data
    outcomeDT <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    outcomeDT[, 11] <- as.numeric(outcomeDT[, 11])
    outcomeDT[, 17] <- as.numeric(outcomeDT[, 17])
    outcomeDT[, 23] <- as.numeric(outcomeDT[, 23])
    
    outcomeList <- c("heart attack", 
                     "heart failure", 
                     "pneumonia")
    
    numList <- c("best", "worst")
    
    rowNumber <- 0
    if (!(outcome %in% outcomeList))
    {
        stop("invalid outcome")               
    }        
    
    if (!(num %in% numList) & !is.numeric(num) )
    {
        stop("invalid num")               
    }        
    
    
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
    
    
    ## Return hospital name by state with the lowest 30-day death
    ## rate
    d1 <- outcomeDT
    bad <- is.na(with(d1, get(outcome) ))
    
    d2 <- d1[!bad,]
    
    if (num == "best") {num <- 1}
    if (num == "worst") {num <- -1}
    
    if (num < 0)
    {
        d4 <- d2[order(d2$State, -(with(d2, get(colnames(d2)[rowNumber]) )), with(d2, get(colnames(d2)[2]))),]              
        
    }
    else
    {
        d4 <- d2[order(d2$State,with(d2, get(colnames(d2)[rowNumber]) ), with(d2, get(colnames(d2)[2]) )),]    
    }
    
    
    d5 <- split(d4 , d4$State)  ## list of data frames

    if (num == -1) {num <- 1}

    ## d5[1] <- data frame # 1
    ## d5[2] <- data frame # 2

    ## d5[[1]][1] <- data frame # 1, col # 1
    ## d5[[1]][2] <- data frame # 1, col # 2

    list1 <- character()
    list2 <- character()
    list3 <- numeric()

    for(i in 1:length(d5))
    {
        ## each element is a data frame
        ## names(d5)[1] State # 1
        ## d5[[1]][1]   Col # 1 List of
        ## d5[[1]][2]   Col # 2 List of Hospital Name        
        ## d5[[1]][[2]][1] Col # 2 Hospital Name # 1
        if (length(d5[[i]][[1]]) < num)
        {
            list1 <- c(list1, names(d5)[i])
            list2 <- c(list2, "NA")
            list3 <- c(list3, "NA")             
        }
        else
        {
            list1 <- c(list1, names(d5)[i])
            list2 <- c(list2, d5[[i]][[2]][num])
            list3 <- c(list3, d5[[i]][[rowNumber]][num])        
        }
        
    }

    d6 <- data.table(hospital=list2, state=list1)
    return(d6)
    
}



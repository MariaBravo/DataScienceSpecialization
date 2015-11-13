

zz <- file("EDU.csv", "rb")
readChar(zz,c(668))

endLine <- "\n"
line <- vector("character", 235)
for(i in 1:235)
{
    readC <- " "
    while (readC != endLine)
    {
        readC <- readChar(zz,c(1))
        if ((readC == '\\') | (readC == '\r'))
        {
            ##print("B")
        }
        else
        {
            line[i] <- paste(line[i], readC, sep="")
        }
    }
    ##print(line[i])
}

for(i in 1:235)
{
    cat(c(i, line[i]), file="texto1.txt" , append=TRUE)
}



listF <- list()
listTot <- list()

j <- 1
for(i in 2:235)
{
    j <- 1
    for (k in 1:31)
    {
        field <- ""
        readC <- substring(line[i],j,j)
        
##        while ((readC != ",") & (readC != '\n'))
##        {
##            readC <- substring(line[i],j,j)
##            if (readC != ",")
##            {
##                field <- paste(field, readC, sep="")
##            }
##            j <- j + 1
##        }
        
        
        if (readC == '\"')
        {
            j <- j + 1
            readC <- " "    
            while (readC != '\"')
            {
                readC <- substring(line[i],j,j)                            
                if (readC != '\"')
                {
                    field <- paste(field, readC, sep="")
                }
                j <- j + 1
            }
            j <- j + 1
           ## print(field)
            
        }    
        else
        {    
            readC <- " "    
            while ((readC != ",") & (readC != '\n'))
            {
                readC <- substring(line[i],j,j)
                
                if (readC == '\"') print("error")                
                if (readC != ",")
                {
                    field <- paste(field, readC, sep="")
                }
                j <- j + 1
            }
        }
            
        listF[k] <- field
    }
    
    listTot[[i]] <- listF
   ## DT <- rbind(DT, listF)
##print(i)
##print(DT)
    ##if (i==35) return(0)
}

DTedu <- data.table()
for(i in 1:234)
{
    listF <- character()
    for(j in 1:31)
    {
        listF <- c(listF, listTot[[i+1]][j])
    }
    
    DTedu <- rbind(DTedu,listF)
}

##DTedu <- DTedu[c(2:235),]
setkey(DTedu, "V1")

return(0)














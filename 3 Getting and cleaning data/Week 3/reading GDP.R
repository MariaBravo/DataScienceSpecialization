

zz <- file("GDP.csv", "rb")
readChar(zz,c(119))

endLine <- "\n"
line <- vector("character", 190)
for(i in 1:190)
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

## Getting Field 1 (Length = 3, character)

list1 <- character()
list2 <- character()
list3 <- character()
list4 <- character()
list5 <- character()

for(i in 1:190)
{
    field <- character()
    readC <- " "
    j <- 1
    while (readC != ",")
    {
        readC <- substring(line[i],j,j)
        if (readC != ",")
        {
            field <- paste(field, readC, sep="")
        }
        j <- j + 1
    }
    list1[i] <- field
    
    ##print("aaaa")
    
    field <- character()
    readC <- " "    
    while (readC != ",")
    {
        readC <- substring(line[i],j,j)
        if (readC != ",")
        {
            field <- paste(field, readC, sep="")
        }
        j <- j + 1
    }
    list2[i] <- field

    ##print("bbbbb")
    
    
    field <- character()    
    readC <- " "    
    while (readC != ",")
    {
        readC <- substring(line[i],j,j)
        if (readC != ",")
        {
            field <- paste(field, readC, sep="")
        }
        j <- j + 1
    }
    list3[i] <- field
    
    ##print("ccccc")

    
    field <- character()    
    readC <- substring(line[i],j,j)
    ## special case
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
    
    }    
    else
    {    
        readC <- " "    
        while (readC != ",")
        {
            readC <- substring(line[i],j,j)
            
                
            if (readC != ",")
            {
                field <- paste(field, readC, sep="")
            }
            j <- j + 1
        }
    }

    list4[i] <- field
    
    ##print("ccccc")
    j <- j + 1
    
    field <- character()    
    readC <- " "    
    readC1 <- " "
    readC2 <- " "
    while ( (readC != '\"') & (readC != ',,') )
    {
        ##print("111")
        readC2 <- substring(line[i],j,j)
        readC <- paste(readC1, readC2, sep="")
        print(readC2)
        print(is.integer(readC2))
        if (!is.na(as.integer(readC2)))
        {
            field <- paste(field, readC2, sep="")
        }
        j <- j + 1
        readC1 <- readC2
        ##print("222")
    }
    list5[i] <- field    
    ##print("dddd")
    
    
}

DTgdp <- data.table("V1"=list1,"Rank"=list2,"Country"=list4,"Income"=list5)
setkey(DTgdp, "V1")

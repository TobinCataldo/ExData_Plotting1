getProject1Data <- function(useExistingFile=T, useExistingVar=T) {
    
    # URL https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
    murl <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    mzfile <- "exdata-data-household_power_consumption.zip"
    mzfilepath <- paste(getwd(), mzfile, sep="/")
    
    #txt name household_power_consumption.txt
    mtfile <- "household_power_consumption.txt"
    
    # check for existence
    t<-grep(paste("^",mtfile,"$", sep=""),
               rownames(file.info(dir())), perl=T)
    
    z<-grep(paste("^",mzfile,"$", sep=""), 
            rownames(file.info(dir())), perl=T)
    
    
    if (length(t) > 0 && useExistingFile) {      
        # text fle exists  
        message("using existing txt file")
        # useExistingVar should be false, why did i download the file then?
        loadProject1Data(mtfile, useExistingVar)
    }
    else if (length(z) > 0 && useExistingFile) {
        # zip file already exists in current working directory
        # need to unzip to get txt file
        
        message("using existing zip file")
        
        unzip(mzfile)
        # load the data and push int global env
        # useExistingVar should be false, why did i download the file then?
        loadProject1Data(mtfile, useExistingVar=F)
    } 
    else {    
        
        message("downloading zip")
        
        download.file(murl, mzfile, method="curl", quiet = FALSE, mode = "wb",
                  cacheOK = TRUE)        
        
        unzip(mzfile)
        
        # load the data and push int global env
        loadProject1Data(mtfile, useExistingVar=F)

    }   
}


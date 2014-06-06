## functions in plot1.R
## plot1(printToScreen (bool), useExistingFile(bool), useExistingVar(bool)
##      printToScreen - print to screen or file
##      useExistingFile - use existing course file, if it exists or
##                    refetch from URL
##      useExistingVar - use existing global env var if it exists or
##                    repopulate it
## getProject1Data( useExistingFile(bool), useExistingVar(bool))
##      useExistingFile - use existing course file, if it exists or
##                    refetch from URL
##      useExistingVar - use existing global env var if it exists or
##                    repopulate it
## loadProject1Data(inputdatafile(string), useExistingVar(bool))
##      inputdatafile - the file to load the data from
##      useExistingVar - use existing global env var if it exists or
##                    repopulate it

plot1 <- function(printToScreen=F, useExistingFile=T,useExistingVar=T) {

    # processes input for project data.
    # creates a global env variable called :
    # household_power_consumption
    
    ## This function should reside in a separate R file
    ## but am including it below
    ## I am leaving the function dscrete in order
    ## to maintain readability 
    getProject1Data(useExistingFile,useExistingVar)
    
    
    # subset the the large file
    mdat<-subset(household_power_consumption, Date=="1/2/2007" | Date=="2/2/2007")    

    
    
    par(family="serif", cex=.75)
    
    if (!printToScreen) {
        # graphics device
        png("plot1.png", 
            width=480, 
            height=480, 
            units="px", 
            bg="white",
            type="cairo-png")
    }
    
    
    # generate the histogram
    hist(mdat$Global_active_power, 
         main="Global Active Power", 
         xlab="Global Active Power (kilowatts)", 
         ylab="Frequency", 
         col=c("red"))
    
    if (!printToScreen) {
        # finish writing
        val <- dev.off()
    }

}

## getProject1Data.R
## checks for existence of file in working directory
## downloads and unzips course data to working directory
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

## loadProject1Data.R
## loads a global env data set that can be reused
loadProject1Data <- function(inputdatafile, useExistingVar=T) {
    
    if (useExistingVar) {
        message("useExistingVar is true")
        # use existing is true but it doesn't exist :(
        if (!exists("household_power_consumption")) {
            message("But the globalenv var doesn't exist")
            # create the data frame
            household_power_consumption <- read.csv(inputdatafile, 
                                                    na.strings=c("?"),  
                                                    sep=";")
            # push into globalenv
            assign("household_power_consumption", household_power_consumption, 
                   pos = globalenv())
        }    
    } else {
        message("creating var")
        # create the data frame
        household_power_consumption <- read.csv(inputdatafile, 
                                                na.strings=c("?"),  
                                                sep=";")
        # push into globalenv
        assign("household_power_consumption", household_power_consumption, 
               pos = globalenv())
    }
}


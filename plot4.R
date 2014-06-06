## functions in plot4.R
## plot4(printToScreen (bool), useExistingFile(bool), useExistingVar(bool)
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


plot4 <- function(printToScreen=F, useExistingFile=T,useExistingVar=T) {
    
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
 
    
    # add DateTime column and a factorable Day column
    mdat <-cbind(mdat, strptime(c(paste(mdat$Date, mdat$Time, 
                                        sep=" ")), 
                                "%d/%m/%Y %H:%M:%S"))
    colnames(mdat)[10]="dt"
    
    mdat <- cbind(mdat, format(mdat$dt, "%a"))
    colnames(mdat)[11]="day"
    
    if (!printToScreen) {
        # graphics device
        png("plot4.png", 
            width=480, 
            height=480, 
            units="px", 
            bg="white",
            type="cairo-png")
    }
    
     
    par(mfrow=c(2,2))
    par(family="serif", cex=.8)  
    
    # we know a two days are in the data set
    # how many rows does Thursday have, I could assume half (1440)
    nuday <- nrow(subset(mdat, day==format(min(mdat$dt),"%a")))
    
    # the x axis labels
    lab<-c("Thu","Fri","Sat")
    
    
    ##########################
    # PLOT 1,1
    
    plot(mdat$Global_active_power, 
         main=NA, 
         ylab="Global Active Power", 
         col="black", 
         type="l", 
         xlab=NA, 
         xaxt='n')
    
    # add the x axis labels
    axis(1, at=c(1,nuday+1,2880),labels=lab)
    
     # draw the borderplot
    box(col="black")
    
    #############################
    # PLOT 1,2
    
    plot(mdat$Voltage, 
         main=NA, 
         ylab="Voltage", 
         col="black", 
         type="l", 
         xlab="datetime", 
         xaxt='n')
    
    # add the x axis labels
    axis(1, at=c(1,nuday+1,2880),labels=lab)    
    
    # draw the border
    box(col="black")
   
    #############################    
    # Plot 2,1
    
    # build the main plot
    plot(mdat$Sub_metering_1,
         main=NA, 
         ylab="Energy sub metering", 
         col="black", 
         type="l", 
         xlab=NA, 
         xaxt='n')
    
    # add sub metering lines
    lines(mdat$Sub_metering_2, col="red")    
    lines(mdat$Sub_metering_3, col="blue")

    # add the x axis labels
    axis(1, at=c(1,nuday+1,2880),labels=lab)
    
    # draw the border
    box(col="black")
        
    # add the legend
    legend("topright", 
           c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
           lty=c(1,1,1), 
           col=c("black","red","blue"), bty="n")
    
    ##########################
    # Plot 2,2
    
    plot(mdat$Global_reactive_power, 
         main=NA, 
         ylab="Global_reactive_power", 
         col="black", 
         type="l", 
         xlab="datetime", 
         xaxt='n')
    
    # add the x axis labels
    axis(1, at=c(1,nuday+1,2880),labels=lab)
    
    # draw the border
    box(col="black")
    
    if (!printToScreen) {
        # finish writing
        val <- dev.off()
    }
    
    par(mfrow=c(1,1))
 
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

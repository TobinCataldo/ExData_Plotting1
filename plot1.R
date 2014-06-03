plot1 <- function(inputdatafile) {
    ## input parameter should be the location of the data file
    ## no checking, just assuming the passed data file is the unzipped
    ## txt file from the assignment household_power_consumption.txt
    
    # read the entire csv
    household_power_consumption <- read.csv(inputdatafile, 
                                            na.strings=c("?"),  
                                            sep=";")
    
    # subset the the large file
    mdat<-subset(household_power_consumption, Date=="1/2/2007" | Date=="2/2/2007")    
    
    #drop the original (should make for quicker analysis)
    rm(household_power_consumption)

    # graphics device
    png("plot1.png", 
        width=480, 
        height=480, 
        units="px", 
        bg="transparent",
        type="cairo")
    
    # generate the histogram
    hist(mdat$Global_active_power, 
         main="Global Active Power", 
         xlab="Global Active Power (kilowatts)", 
         ylab="Frequency", 
         col=c("red"))
    
    # finish writing
    val <- dev.off()
 
}

plot1Local <- function() {
    ## this one is for me
    plot1("C:/Users/tcataldo/Work/build/R/coursera/ExploratoryDataAnalysis/household_power_consumption.txt")
}
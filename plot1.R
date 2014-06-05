plot1 <- function(printToScreen=F, useExistingFile=T,useExistingVar=T) {

    # processes input for project data.
    # creates a global env variable called :
    # household_power_consumption
    getProject1Data(useExistingFile,useExistingVar)
    
    
    # subset the the large file
    mdat<-subset(household_power_consumption, Date=="1/2/2007" | Date=="2/2/2007")    

    
    op <- par()
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
    
    par<-op
}


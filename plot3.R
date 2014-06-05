plot3 <- function(printToScreen=F, useExistingFile=T,useExistingVar=T) {
    
    # processes input for project data.
    # creates a global env variable called :
    # household_power_consumption
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
    
    op <- par()
    par(family="serif", cex=.8)
    
    
    if (!printToScreen) {
        # graphics device
        png("plot3.png", 
            width=480, 
            height=480, 
            units="px", 
            bg="white",
            type="cairo-png")
    }
    
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
    
 
    
    # we know a two days are in the data set
    # how many rows does Thursday have, I could assume half (1440)
    nuday <- nrow(subset(mdat, day==format(min(mdat$dt),"%a")))
    
    # add the x axis labels
    lab<-c("Thu","Fri","Sat")
    axis(1, at=c(1,nuday+1,2880),labels=lab)
    
    # draw the border
    box(col="black")
    
    
    # add the legend
    legend("topright", 
           c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
           lty=c(1,1,1), 
           col=c("black","red","blue"))
    
    if (!printToScreen) {
        # finish writing
        val <- dev.off()
    }
    par<-op
    
}


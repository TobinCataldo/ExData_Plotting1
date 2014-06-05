plot4 <- function(printToScreen=F, useExistingFile=T,useExistingVar=T) {
    
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
    
    if (!printToScreen) {
        # graphics device
        png("plot4.png", 
            width=480, 
            height=480, 
            units="px", 
            bg="white",
            type="cairo-png")
    }
    
    op <- par()
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
    
    par<-op
 
}


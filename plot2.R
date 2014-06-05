plot2 <- function(printToScreen=F, useExistingFile=T,useExistingVar=T) {
    
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
    par(family="serif", cex=.75)
    
    if (!printToScreen) {
        # graphics device
        png("plot2.png", 
            width=480, 
            height=480, 
            units="px", 
            bg="white",
            type="cairo-png")
    }
    
    plot(mdat$Global_active_power, 
             main=NA, 
             ylab="Global Active Power (kilowatts)", 
             col="black", 
             type="l", 
             xlab=NA, 
             xaxt="n")
    
    # we know a two days are in the data set
    # how many rows does Thursday have, I could assume half (1440)
    nuday <- nrow(subset(mdat, day==format(min(mdat$dt),"%a")))
    
    # add the x axis labels
    lab<-c("Thu","Fri","Sat")
    axis(1, at=c(1,nuday+1,2880),labels=lab)

    # draw the border
    box(col="black")
    
    if (!printToScreen) {
        # finish writing
        val <- dev.off()
    }
    
    par<-op
}

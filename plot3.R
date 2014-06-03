plot3 <- function(inputdatafile) {
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
    
    # add DateTime column and a factorable Day column
    mdat <-cbind(mdat, strptime(c(paste(mdat$Date, mdat$Time, 
                                        sep=" ")), 
                                "%d/%m/%Y %H:%M:%S"))
    colnames(mdat)[10]="dt"
    
    mdat <- cbind(mdat, format(mdat$dt, "%a"))
    colnames(mdat)[11]="day"
    
    # graphics device
    png("plot3.png", 
        width=480, 
        height=480, 
        units="px", 
        bg="white",
        type="cairo-png")
    
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
           col=c("black","blue","red"))
    
    # finish writing
    val <- dev.off()
  
    
}

plot3Local <- function() {
    ## this one is for me
    plot3("C:/Users/tcataldo/Work/build/R/coursera/ExploratoryDataAnalysis/household_power_consumption.txt")
}
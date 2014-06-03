plot2 <- function(inputdatafile) {
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
    png("plot2.png", 
        width=480, 
        height=480, 
        units="px", 
        bg="transparent",
        type="cairo")
    
    plot(mdat$Global_active_power, 
             main=NA, 
             ylab="Global Active Power (kilowatts)", 
             col="black", 
             type="l", 
             xlab=NA, 
             xaxt='n')
    
    # we know a two days are in the data set
    # how many rows does Thursday have, I could assume half (1440)
    nuday <- nrow(subset(mdat, day==format(min(mdat$dt),"%a")))
    
    # add the x axis labels
    lab<-c("Thu","Fri","Sat")
    axis(1, at=c(1,nuday+1,2880),labels=lab)

    # draw the border
    box(col="black")
    
    # finish writing
    val <- dev.off()
    
}

plot2Local <- function() {
 ## this one is for me
 plot2("C:/Users/tcataldo/Work/build/R/coursera/ExploratoryDataAnalysis/household_power_consumption.txt")
}
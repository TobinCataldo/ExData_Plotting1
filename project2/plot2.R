##
## The Quesion
## Have total emissions from PM2.5 decreased in the Baltimore City, 
## Maryland (fips == "24510") from 1999 to 2008? Use the base plotting 
## system to make a plot answering this question.
##
## Output a PNG

plot2 <- function(printToScreen=F, useExistingFile=T,useExistingVar=T) {
    
    # uSe BASE plot
    
    # processes input for project data.
    # creates a global env variables NEI, SCC :    
    getProject2Data(useExistingFile,useExistingVar)
  
    
    # aggregate the data by year (simple sum)
    # trim the dataset
    trimmed <- subset(NEI, fips=="24510")
    aggdata <- aggregate(trimmed$Emissions, by=list(trimmed$year), FUN=sum)
    
    if (!printToScreen) {
        # graphics device
        png("plot2.png", 
            width=480, 
            height=480, 
            units="px", 
            bg="white",
            type="cairo-png")
    } 
    
    # plot x,y with y scaled to parts per thousand
    plot(aggdata[[1]], round(aggdata[[2]]/1000,2), 
         main="Total PM2.5 Emissions by Year for Baltimore City, Maryland", 
         ylab="Total Emissions (parts per thousand)", 
         col="black", 
         type="o", 
         xlab="Year", 
         xaxt="n",
         ylim=c(0,5) # could do calc but this is easier     
    )
    
    axis(1, at=aggdata[[1]], labels=aggdata[[1]]) # only want 4 ticks
    # add scaled point values to chart
    text(aggdata[[1]], round(aggdata[[2]]/1000,2), 
         labels=round(aggdata[[2]]/1000,2), 
         pos=3, # Values of 1,2,3,4,(below,left,above,right)
         cex=0.8)
    
   
    if (!printToScreen) {
        val<-dev.off()
    }
}
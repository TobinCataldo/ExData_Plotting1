##
## The Question
## Have total emissions from PM2.5 decreased in the United States from 
## 1999 to 2008? Using the base plotting system, make a plot showing 
## the total PM2.5 emission from all sources for each of the years 
## 1999, 2002, 2005, and 2008.
##
## Output a PNG

plot1 <- function(printToScreen=F, useExistingFile=T,useExistingVar=T) {
    
    # uSe BASE plot
    
    # processes input for project data.
    # creates a global env variables NEI, SCC :    
    getProject2Data(useExistingFile,useExistingVar)

    
    # aggregate the data by year (simple sum)
    aggdata <- aggregate(NEI$Emissions, by=list(NEI$year), FUN=sum);

    if (!printToScreen) {
        # graphics device
        png("plot1.png", 
            width=480, 
            height=480, 
            units="px", 
            bg="white",
            type="cairo-png")
    } 
    
    # plot x,y with y scaled to parts per million
    plot(aggdata[[1]], round(aggdata[[2]]/1000000,2), 
         main="Total PM2.5 Emissions by Year", 
         ylab="Total Emissions (parts per million)", 
         col="black", 
         type="o", 
         xlab="Year", 
         xaxt="n",
         ylim=c(0,10) # could do calc but this is easier     
    )
    
    axis(1, at=aggdata[[1]], labels=aggdata[[1]]) # only want 4 ticks
    # add scaled point values to chart
    text(aggdata[[1]], round(aggdata[[2]]/1000000,2), 
         labels=round(aggdata[[2]]/1000000,2), 
         pos=3, # Values of 1,2,3,4,(below,left,above,right)
         cex=0.8)
    
   
    if (!printToScreen) {
        val<-dev.off()
    }
}
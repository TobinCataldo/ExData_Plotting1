##
## The Question
## How have emissions from motor vehicle sources changed from 1999–2008 
## in Baltimore City?
##
## Output a PNG

plot5 <- function(printToScreen=F, useExistingFile=T,useExistingVar=T) {
    
    # meet local package requirements
    require(ggplot2)
    require(grid)
    # processes input for project data.
    # creates a global env variables NEI, SCC :    
    getProject2Data(useExistingFile,useExistingVar)
    
    # aggregate the data by year (simple sum)
    trimmed <- subset(NEI, fips=="24510")
    
    # find the motor vehicles
    # EI.Sector starting with Mobile looks pretty good
    motorVehicle <- SCC[grepl("^Mobile.*?",SCC$EI.Sector),]
    
    # unique(motorVehicle$EI.Sector)
    # [1] Mobile - On-Road Gasoline Light Duty Vehicles
    # [2] Mobile - On-Road Gasoline Heavy Duty Vehicles
    # [3] Mobile - On-Road Diesel Light Duty Vehicles  
    # [4] Mobile - On-Road Diesel Heavy Duty Vehicles  
    # [5] Mobile - Non-Road Equipment - Gasoline       
    # [6] Mobile - Non-Road Equipment - Other          
    # [7] Mobile - Non-Road Equipment - Diesel         
    # [8] Mobile - Aircraft                            
    # [9] Mobile - Commercial Marine Vessels           
    # [10] Mobile - Locomotives   
    
    # grab an intersect (investigate joins bette)
    trimmed <- trimmed[ trimmed$SCC %in% intersect(motorVehicle$SCC, trimmed$SCC),]
    
    # just want the EI.Sector
    trimmed <- merge(trimmed, motorVehicle, by.x = "SCC", by.y = "SCC")
    aggdata <- aggregate(trimmed$Emissions, by=list(trimmed$fips, trimmed$EI.Sector, trimmed$year), FUN=sum)
  
    # local variables in AES fail
    # http://stackoverflow.com/questions/10659133/local-variables-within-aes    
    .e <- environment()
    mainplot <- ggplot(data=aggdata, 
                 aes(x=factor(aggdata[[3]]),          # X axis data, factors become tick marks
                     y=round(aggdata[[4]],2),         # scaled Y axis data
                     label=round(aggdata[[4]],2),
                     color=aggdata[[2]]
                 ),
                 environment =.e) +  # adds point value to graph
      
        geom_point() +     
        geom_line(size=.5, aes(group=aggdata[[2]])) +          # connect the points
        #geom_text(size=3, hjust = 1, vjust = 1.25) +     # writes the labels    
        ylab("PM2.5 Emissions") + 
        xlab("Year") + 
        labs(color = "\n\n\n\n\n\n\n\n\n\n\n\n\nSource Type") +        
        ggtitle("PM2.5 Emissions From Motor Vehicles By Source\nBaltimore City, Maryland") +       
        theme(plot.title = element_text(size=10, face="bold")) +
        theme(legend.position="right") + 
        theme(legend.direction="vertical") +
        theme(legend.title = element_text(size=8, face="bold")) +
        theme(legend.text = element_text(size=9)) +
        theme(axis.title = element_text(size=9)) +
        theme(axis.text = element_text(size=8)) 
        #stat_summary(aes(group=factor(aggdata[[1]]),label="sum"), fun.y=sum, geom="line", linetype="dotted", size=1.3, alpha=.23, color="black") 
    
    
        subplot <- ggplot(data=aggdata, 
                          aes(x=factor(aggdata[[3]]),          # X axis data, factors become tick marks
                              y=round(aggdata[[4]],2),         # scaled Y axis data
                              label=round(aggdata[[4]],2)
                              ),
                              
                              environment =.e) +  # adds point value to graph
        
        ylab("") + 
        xlab("") + 
        ggtitle("Total Motor Vehicle Emissions\nBaltimore City, Maryland") +       
        theme(plot.title = element_text(size=8, face="bold")) +
 
        theme(panel.background = element_rect(fill = "transparent", 
                                              color = "transparent", 
                                              size=0)) +
        theme(panel.grid.minor = element_blank()) +
        theme(panel.grid.major = element_blank()) +
        theme(plot.background = element_rect(fill = "transparent",
                                             color = "transparent", 
                                             size=0)) +

        theme(axis.title = element_text(size=7)) +
        theme(axis.text = element_text(size=7)) +
        stat_summary(aes(group=factor(aggdata[[1]]),label="sum"), fun.y=sum, geom="line", linetype="dotted", size=1.1, alpha=1, color="black") 
    

        vp <- viewport(width = 0.30, height = 0.35,
                       x=.85, y=.60,  
                       just = c("right","bottom"))


    if (printToScreen) {
        print(mainplot)
        print(subplot, vp = vp)
 
    } else{
       
      #  ggsave("plot5.png", pl, width=8, height=6, dpi=100)
    
        png("plot5.png", 
            width=520, 
            height=400, 
            units="px", 
            bg="white",
            type="cairo-png")
        print(mainplot)
        print(subplot, vp = vp)
 
        va<-dev.off()
    }
}


##
## The Question
## Compare emissions from motor vehicle sources in Baltimore City with 
## emissions from motor vehicle sources in Los Angeles County, 
## California (fips == "06037"). Which city has seen greater changes 
## over time in motor vehicle emissions?
##
## Output a PNG

plot6 <- function(printToScreen=F, useExistingFile=T,useExistingVar=T) {
    

        # meet local package requirements
        require(ggplot2)
        require(grid)
        # processes input for project data.
        # creates a global env variables NEI, SCC :    
        getProject2Data(useExistingFile,useExistingVar)
        
        # aggregate the data by year (simple sum)
        trimmed <- subset(NEI, fips=="24510" | fips == "06037" )
        
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
        aggdata[1] <- as.factor(aggdata[[1]])
        aggdata[2] <- as.factor(aggdata[[2]])
        aggdata[3] <- as.factor(aggdata[[3]])
        
        levels(aggdata[[1]]) <- c("Los Angeles County, California", "Baltimore City, Maryland")
        
        # local variables in AES fail
        # http://stackoverflow.com/questions/10659133/local-variables-within-aes    
        .e <- environment()
        mainplot <- ggplot(data=aggdata, 
                           aes(x=Group.3, 
                               y=x, 
                               color=Group.2)) + 
            geom_point() + 
            geom_line(size=.6,aes(group=Group.2))+ 
            facet_grid(Group.1~., scales="free_y") +
            ylab("PM2.5 Emissions") + 
            xlab("Year") + 
            labs(color = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nSource Type") + 
            ggtitle("PM2.5 Emissions From Motor Vehicles By Source") +
            theme(plot.title = element_text(size=10, face="bold")) +
            theme(legend.position="right") + 
            theme(legend.direction="vertical") +
            theme(legend.title = element_text(size=8, face="bold")) +
            theme(legend.text = element_text(size=9)) +
            theme(axis.title = element_text(size=9)) +
            theme(axis.text = element_text(size=8)) +
            theme(strip.text.y = element_text(size = 9, face="bold"))
        
        # build the subplot, but don't use stat_summary 
        # just build new aggregation table
        aggdata2 <- aggregate(aggdata$x, by=list(aggdata$Group.1, aggdata$Group.3), FUN=sum)
        
        subplot <- ggplot(data=aggdata2, 
                          aes(x=Group.2,   
                              y=x,         
                              color=Group.1
                          ),                          
                          environment =.e) +              
            ylab("") + 
            xlab("") +
            labs(color="") +
            ggtitle("Total Motor Vehicle Emissions") +       
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
            theme(legend.position=c(.95,.5)) + 
            theme(legend.background = element_rect(fill = "transparent", 
                                                  color = "transparent", 
                                                  size=0)) +
            theme(legend.title = element_text(size=8, face="bold")) +
            theme(legend.text = element_text(size=9)) +
            geom_point() +
            geom_line(aes(group=Group.1)) +
            scale_color_manual(values=c("#CC6666", "#9999CC"))
            #stat_summary(aes(group=Group.1), fun.y=sum, geom="line", linetype="solid", size=1.1, alpha=1, color="black") 
        
        # pin these together
        vp <- viewport(width = 0.40, height = 0.38,
                       x=.88, y=.60,  
                       just = c("right","bottom"))
        
        
        if (printToScreen) {
           print(mainplot)
           print(subplot, vp = vp)      
            
        } else{
            
            #  ggsave("plot5.png", pl, width=8, height=6, dpi=100)
            
            png("plot6.png", 
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
    
    


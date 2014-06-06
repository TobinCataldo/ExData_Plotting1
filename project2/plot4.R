##
## The Question
## Across the United States, how have emissions from coal 
## combustion-related sources changed from 1999–2008?
##
## Output a PNG

plot4 <- function(printToScreen=F, useExistingFile=T,useExistingVar=T) {
    
    # meet local package requirements
    require(ggplot2)
    require(grid)
    # processes input for project data.
    # creates a global env variables NEI, SCC :    
    getProject2Data(useExistingFile,useExistingVar)
    
    # find the coal combusters
    # EI.Sector ending with Coal looks pretty good
    coalsources <- SCC[grepl(".*? Coal$",SCC$EI.Sector),]
    
    # unique(coalsources$EI.Sector)
    # [1] Fuel Comb - Electric Generation - Coal     
    # [2] Fuel Comb - Industrial Boilers, ICEs - Coal
    # [3] Fuel Comb - Comm/Institutional - Coal    
    
    # grab an intersect (investigate joins bette)
    trimmed <- NEI[ NEI$SCC %in% intersect(coalsources$SCC, NEI$SCC),]
    
    # just want the EI.Sector
    trimmed <- merge(trimmed, coalsources, by.x = "SCC", by.y = "SCC")
    aggdata <- aggregate(trimmed$Emissions, by=list(trimmed$EI.Sector, trimmed$year), FUN=sum)
    aggdata[1] <- as.factor(aggdata[[1]])
    aggdata[2] <- as.factor(aggdata[[2]])
    
    
    # local variables in AES fail
    # http://stackoverflow.com/questions/10659133/local-variables-within-aes    
    .e <- environment()
    mainplot <- ggplot(data=aggdata, 
                       aes(x=Group.2,
                           y=round((x/1000),2),                               
                           color=Group.1
                       ),
                       environment =.e) +          
        geom_point() +     
        geom_line(size=.5, aes(group=Group.1)) +          # connect the points
        #geom_text(size=3, hjust = 1, vjust = 1.25) +     # writes the labels    
        ylab("PM2.5 Emission (parts per thousand)") + 
        xlab("Year") + 
        labs(color = "\n\n\n\n\n\n\n\n\n\n\n\n\nSource") +        
        ggtitle("PM2.5 Emission From Coal Combustion\nBy Source") +       
        theme(plot.title = element_text(size=10, face="bold")) +
        theme(legend.position="right") + 
        theme(legend.direction="vertical") +
        theme(legend.title = element_text(size=9, face="bold")) +
        theme(legend.text = element_text(size=9)) +
        theme(axis.title = element_text(size=9)) +
        theme(axis.text = element_text(size=9)) 
    #stat_summary(aes(group=factor(aggdata[[1]]),label="sum"), fun.y=sum, geom="line", linetype="dotted", size=1.3, alpha=.23, color="black") 
    
    
    subplot <- ggplot(data=aggdata, 
                      aes(x=Group.2,
                          y=x),                              
                      environment =.e) +     
        ylab("") + 
        xlab("") + 
        ggtitle("Total PM2.5 Emission\nFrom Coal Combustion") +       
        theme(plot.title = element_text(size=9, face="bold")) +
        theme(panel.background = element_rect(fill = "transparent", 
                                              color = "transparent", 
                                              size=0)) +
        theme(panel.grid.minor = element_blank()) +
        theme(panel.grid.major = element_blank()) +
        theme(plot.background = element_rect(fill = "transparent",
                                             color = "transparent", 
                                             size=0)) +
        
        theme(axis.title = element_text(size=8)) +
        theme(axis.text = element_text(size=8)) +
        stat_summary(aes(group=NA), fun.y=sum, geom="line", linetype="solid", size=1.1, alpha=1, color="black") 
    
    
    vp <- viewport(width = 0.40, height = 0.40,
                   x=.93, y=.52,  
                   just = c("right","bottom"))
    
    
    if (printToScreen) {
        print(mainplot)
        print(subplot, vp = vp)        
        
    } else{
        
        #  ggsave("plot5.png", pl, width=8, height=6, dpi=100)
        
        png("plot4.png", 
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


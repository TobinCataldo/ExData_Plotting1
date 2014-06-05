##
## The Quesion
## Of the four types of sources indicated by the type (point, nonpoint, 
## onroad, nonroad) variable, which of these four sources have seen 
## decreases in emissions from 1999–2008 for Baltimore City? Which have 
## seen increases in emissions from 1999–2008? Use the ggplot2 plotting 
## system to make a plot answer this question.
##
## Output a PNG

plot3 <- function(printToScreen=F, useExistingFile=T,useExistingVar=T) {
    
    # meet local package requirements
    require(ggplot2)
    
    # processes input for project data.
    # creates a global env variables NEI, SCC :    
    getProject2Data(useExistingFile,useExistingVar)
            
    # aggregate the data by year (simple sum)
    trimmed <- subset(NEI, fips=="24510")
    aggdata <- aggregate(trimmed$Emissions, by=list(trimmed$type, trimmed$year), FUN=sum)

    
    # local variables in AES fail
    # http://stackoverflow.com/questions/10659133/local-variables-within-aes    
    .e <- environment()
    pl <- ggplot(data=aggdata, 
                 aes(x=factor(aggdata[[2]]),          # X axis data, factors become tick marks
                     y=round(aggdata[[3]],2),         # scaled Y axis data
                     label=round(aggdata[[3]],2),
                     group=aggdata[[1]],
                     color=aggdata[[1]]
                     ),
                 environment =.e) +  # adds point value to graph
        geom_point() +     
        geom_line(size=1.1) +          # connect the points
        geom_text(size=3, hjust = 1, vjust = 1.25) +     # writes the labels    
        ylab("Total PM2.5 Emissions") + 
        xlab("Year") + 
        labs(color = "Source Type") +        
        ggtitle("PM2.5 Emissions By Year and Type for Baltimore City, Maryland") +       
        theme(plot.title = element_text(size=10, face="bold")) +
        theme(legend.position=c(.88,.84)) +    
        theme(legend.title = element_text(size=7, face="bold")) +
        theme(legend.text = element_text(size=6)) +
        theme(axis.title = element_text(size=8)) +
        theme(axis.text = element_text(size=7)) 

      
    if (printToScreen) {
        print(pl)
    } else{
        # produces 500x500px
        ggsave("plot3.png", pl, width=5, height=5, dpi=100)
    }
}
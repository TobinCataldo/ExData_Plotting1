loadProject2Data <- function(inputdatavector, useExistingVar=T) {
    
    if (useExistingVar) {
        message("useExistingVar is true")
        # use existing is true but it doesn't exist :(
        if (!exists("NEI")) {
            message("But the globalenv NEI var doesn't exist")
            # create the data frame
            NEI <- readRDS(inputdatavector[1])
            # push into globalenv
            assign("NEI", NEI, pos = globalenv()) 
        }    
        if (!exists("SCC")) {
            message("But the globalenv SCC var doesn't exist")
            # create the data frame
            SCC <- readRDS(inputdatavector[2])
            # push into globalenv
            assign("SCC", SCC, pos = globalenv()) 
        }
        
    } else {
        message("creating var NEI")
        # create the data frame
        NEI <- readRDS(inputdatavector[1])
        # push into globalenv
        assign("NEI", NEI, pos = globalenv())
         
        message("creating var SCC")
        # create the data frame
        SCC <- readRDS(inputdatavector[2])
        # push into globalenv
        assign("SCC", SCC, pos = globalenv()) 
    }
}
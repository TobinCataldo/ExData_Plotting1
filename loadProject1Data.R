loadProject1Data <- function(inputdatafile, useExistingVar=T) {
    
    if (useExistingVar) {
        message("useExistingVar is true")
        # use existing is true but it doesn't exist :(
        if (!exists("household_power_consumption")) {
            message("But the globalenv var doesn't exist")
            # create the data frame
            household_power_consumption <- read.csv(inputdatafile, 
                                            na.strings=c("?"),  
                                            sep=";")
            # push into globalenv
            assign("household_power_consumption", household_power_consumption, 
                pos = globalenv())
        }    
    } else {
        message("creating var")
        # create the data frame
        household_power_consumption <- read.csv(inputdatafile, 
                                                na.strings=c("?"),  
                                                sep=";")
        # push into globalenv
        assign("household_power_consumption", household_power_consumption, 
               pos = globalenv())
    }
}
getProject2Data <- function(useExistingFile=T, useExistingVar=T) {
    
    # URL https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
    murl <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    mzfile <- "exdata-data-NEI_data.zip"
    mzfilepath <- paste(getwd(), mzfile, sep="/")
    
    #txt name Source_Classification_Code.rds summarySCC_PM25.rds
    mtfileNEI <- "summarySCC_PM25.rds"
    mtfileSCC <- "Source_Classification_Code.rds"
    
    # check for existence
    t<-grep(paste("^",mtfileNEI,"$", sep=""),
            rownames(file.info(dir())), perl=T)
    u<-grep(paste("^",mtfileSCC,"$", sep=""),
            rownames(file.info(dir())), perl=T)
    
    z<-grep(paste("^",mzfile,"$", sep=""), 
            rownames(file.info(dir())), perl=T)
    
    
    if (length(t) > 0 && length(u) > 0 &&  useExistingFile) {      
        # text files exists  
        message("using existing txt file")
        # useExistingVar should be false, why did i download the file then?
        loadProject2Data(c(mtfileNEI,mtfileSCC), useExistingVar)
    }
    else if (length(z) > 0 && useExistingFile) {
        # zip file already exists in current working directory
        # need to unzip to get txt file
        
        message("using existing zip file")
        
        unzip(mzfile)
        # load the data and push int global env
        # useExistingVar should be false, why did i download the file then?
        loadProject2Data(c(mtfileNEI,mtfileSCC), useExistingVar=F)
    } 
    else {    
        
        message("downloading zip")
        
        download.file(murl, mzfile, method="curl", quiet = FALSE, mode = "wb",
                      cacheOK = TRUE)        
        
        unzip(mzfile)
        
        # load the data and push int global env
        loadProject2Data(c(mtfileNEI,mtfileSCC), useExistingVar=F)
        
    }   
}

complete <- function(directory, id = 1:332) {
        #################################
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        #################################
        
        # get the file name number 
        filename <- vector(mode="character", length=length(id))
        #form the file name 
        for(i in seq_along(id)) {
                ## form the file name by adding leading zeros
                filenum <- sprintf("%03d", id[i])
                # Create the file path 
                zz <- paste(filenum, ".csv", sep="") 
                # Create a vector of the file names 
                filename[i] <- zz      

        }

        #set the data frame for the id number of entries  
        nobs <- vector(mode="integer", length=length(id)) 
        
        for(i in seq(filename)) {
                #read in the data
                thedata <- read.csv(filename[i])
                #determine the number of good number of rows
                completerows <- sum(complete.cases(thedata))
                #save the row count in the data frame
                nobs[i] <- completerows

        }
        result = data.frame(id, nobs)
        result
}


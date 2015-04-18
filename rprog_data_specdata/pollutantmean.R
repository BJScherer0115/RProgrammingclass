
pollutantmean <- function(directory, pollutant, id = 1:332) {
        ############################
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ############################
        
        ############################
        ## Define storage areas
        ############################
        
        thesum <- 0
        total <- 0 
       
        ############################
        ## Define coding area
        ############################
        
        filename <- vector(mode="character", length=length(id))
        for(i in seq_along(id)) {
                ## form the file name by adding leading zeros
                filenum <- sprintf("%03d", id[i])
                # Create the file path 
                zz <- paste(filenum, ".csv", sep="") 
                # Create a vector of the file names 
                filename[i] <- zz      
                #debugging print statement
                #print(filename)
        }
         # for all the file names get the data
         for(i in filename) {
                # read in the data 
                thedata <- read.csv(i)
                # get the data that is good 
                gooddata <- complete.cases(thedata[pollutant])   
                thedata <- thedata[gooddata, ]
                # get the total number of rows that are good
                total <- total + nrow(thedata)
                # sum the non NA data
                thesum <- thesum + sum(thedata[[pollutant]], na.rm = TRUE)
        }
        #determine the mean and round it to 3 decimal places 
        themean <- round(thesum/total, 3)
        print(total)
        # return the rounded mean
        return(themean)
       
}
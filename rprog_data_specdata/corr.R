corr <- function(directory, threshold = 0) {
  ##############################
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ##############################
  
  #set file name numbers for specdata
  id = 1:332
  
  filename <- vector(mode="character", length=length(id))
  for(i in seq_along(id)) {
    ## form the file name by adding leading zeros
    filenum <- sprintf("%03d", id[i])
    # Create the file path 
    zz <- paste(filenum, ".csv", sep="") 
    # Create a vector of the file names 
    filename[i] <- zz      
  }
  
  
  #empty vector
  result <-vector(mode="numeric", length=0)
  
  for(i in seq(filename)) {
    #read in the data
    thedata <- read.csv(filename[i])
    #get the complete cases
    good <- complete.cases(thedata)
    thedata <- thedata[good, ]
    #determine the threshold
    if (nrow(thedata) > threshold) {
      #calculate the correlation using complete. 
      correlation <- cor(thedata[["sulfate"]], thedata[["nitrate"]], use = "complete.obs")
      #append the result to the vector 
      result <- append(result, correlation)

    }
  }
  #return the result
  result
}
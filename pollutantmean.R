pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## author: Keith Thoresz
  ## user id: 903673
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  if (!is.character(directory) || directory == '') {
    return ("Error: must provide a valid directory")
  }

  if (!is.character(pollutant) || pollutant == '') {
    return ("Error: must provide a valid pollutant")
  }

  if (!is.numeric(id) || length(id) < 1 || any(id < 1) || any(id > 332)) {
    return ("Error: monitor ids must be in the range [1,332]")
  }
  
  filenames = dir(directory)
  pollutant_data = read.csv(file.path(directory,filenames[id[1]]))[pollutant]
  
  if (length(id) > 1) {
    for (fileid in id[2:length(id)]) {
      new_data = read.csv(file.path(directory,filenames[fileid]))[pollutant]
      pollutant_data<-rbind(pollutant_data,new_data)
    }
  }
  
  return (mean(pollutant_data[[pollutant]],na.rm=TRUE))
}
complete <- function(directory, id = 1:332) {
  ## author: Keith Thoresz
  ## user id: 903673

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
  
  if (!is.character(directory) || directory == '') {
    return ("Error: must provide a valid directory")
  }
    
  if (!is.numeric(id) || length(id) < 1 || any(id < 1) || any(id > 332)) {
    return ("Error: monitor ids must be in the range [1,332]")
  }
  
  filenames = dir(directory)
  data = read.csv(file.path(directory,filenames[id[1]]))
  good_obs = data.frame(id=id[1], nobs=length(which(complete.cases(data))))
  
  if (length(id) > 1) {
    for (fileid in id[2:length(id)]) {
      data = read.csv(file.path(directory,filenames[fileid]))
      row = c(fileid,length(which(complete.cases(data))))
      good_obs<-rbind(good_obs,row)
    }
  }
  
  print (good_obs)
}
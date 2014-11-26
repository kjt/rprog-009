corr <- function(directory, threshold = 0) {
  ## author: Keith Thoresz
  ## user id: 903673

  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  if (!is.character(directory) || directory == '') {
    return ("Error: must provide a valid directory")
  }
  
  if (!is.numeric(threshold) || length(threshold) > 1) {
    return ("Error: threshold must be a number")
  }
  
  filenames = dir(directory)
  correlations = numeric()
  
  for (file in filenames) {
    data = read.csv(file.path(directory,file))
    cases = which(complete.cases(data))

    if (length(cases) > threshold) {
      data = data[cases,]
      correlations = c(correlations,cor(data$sulfate,data$nitrate))
    }
  }
   
  return (correlations)
}
corr <- function(directory, threshold = 0) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  require(data.table)
  
  cors <- c()
  id <- 1:332
  for (i in 1:length(id)) {
    dt <- fread(paste0(directory,sprintf("/%03d",id[i]),'.csv'))
    if (sum(complete.cases(dt)) > threshold){
      cors <- c (cors,dt [,cor(nitrate,sulfate,use='pairwise.complete.obs')])
    }
  }
  return (cors)
}
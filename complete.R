complete <- function(directory, id = 1:332) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  require(data.table)
  
  all_dt <- data.table(id=1,nobs=1)
  all_dt <- all_dt[0]
  
  for (i in 1:length(id)) {
    dt <- fread(paste0(directory,sprintf("/%03d",id[i]),'.csv'))
    c.cases <- data.table(id=id[i],nobs=sum(complete.cases(dt)))
    all_dt <-  rbindlist (list(all_dt,c.cases), use.names = T,fill = F)
  }
  df <- as.data.frame(all_dt)
  return (df)
  
}
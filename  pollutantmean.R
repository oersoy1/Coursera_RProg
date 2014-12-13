pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  require(data.table)
  
  all_dt <- data.table(Date='2003-01-01',sulfate=NA,nitrate=NA,ID=1)
  all_dt <- all_dt[0]
  
  for (i in 1:length(id)) {
    dt <- fread(paste0(directory,sprintf("%03d",id[i]),'.csv'))
    all_dt <- rbindlist (list(all_dt,dt), use.names = T,fill = F)
  }
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  if (pollutant == 'nitrate') {
    m_pol <- all_dt [,mean(nitrate,na.rm=T)]
  }
  else {
    m_pol <- all_dt [,mean(sulfate,na.rm=T)]
  }
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  return (round(m_pol,3))
}
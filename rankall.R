rankall <- function(outcome, num = "best") {
  
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  if (!outcome %in% outcomes) {
    stop ('invalid outcome')
  }
  
  require(data.table)
  dt <- fread("outcome-of-care-measures.csv")
  
  setnames(dt,'Hospital 30-Day Death (Mortality) Rates from Heart Attack','Mortality.Heart.Attack')
  setnames(dt,'Hospital 30-Day Death (Mortality) Rates from Heart Failure','Mortality.Heart.Failure')
  setnames(dt,'Hospital 30-Day Death (Mortality) Rates from Pneumonia','Mortality.Pneumonia')
  setnames(dt,'Hospital Name', 'Hospital.Name')
  
  suppressWarnings(dt <- dt[,Mortality.Heart.Attack:=as.numeric(Mortality.Heart.Attack)])
  suppressWarnings(dt <- dt[,Mortality.Heart.Failure:=as.numeric(Mortality.Heart.Failure)])
  suppressWarnings(dt <- dt[,Mortality.Pneumonia:=as.numeric(Mortality.Pneumonia)])
  
  
  if (outcome =='heart attack') {
    setkeyv(dt,c('State','Mortality.Heart.Attack','Hospital.Name'))
    dt <- dt[!is.na(Mortality.Heart.Attack)]
  }else if (outcome =='heart failure'){
    setkeyv(dt,c('State','Mortality.Heart.Failure','Hospital.Name'))
    dt <- dt[!is.na(Mortality.Heart.Failure)]
  }else if (outcome =='pneumonia'){
    setkeyv(dt,c('State', 'Mortality.Pneumonia', 'Hospital.Name'))
    dt <- dt[!is.na(Mortality.Pneumonia)]
  }
  setnames(dt,c('Hospital.Name','State'),c('hospital','state'))
  
  if (num=='best') {
    dt[J(unique(state)),mult="first"][,list(hospital,state)]
  }else if (num=='worst'){
    dt[J(unique(state)),mult="last"][,list(hospital,state)]
  }else {
    dt[,.SD[num],by=state][,list(hospital,state)]
  }
}
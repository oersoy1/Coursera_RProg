rankhospital <- function(state, outcome, num = "best") {
  
  states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
              "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
              "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM",
              "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD",
              "TN", "TX", "UT", "VT", "VI", "VA", "WA", "WV", "WI", "WY", "GU")
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  
  if (!state %in% states) {
    stop ('invalid state')
  }
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
  
  if (num=='best') {
    dt[state,Hospital.Name,mult='first'] 
  }else if (num=='worst'){
    dt[state,Hospital.Name,mult='last'] 
  }else {
    rank.names <- dt[state,Hospital.Name]
    rank.names[num]
  }
  
  
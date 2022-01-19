best <- function(state, outcome) {
  ## Read outcome data
  outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hrtAtk <- as.numeric(outcome[,11]) #Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  hrtFail <- as.numeric(outcome[,17])#Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  pnu <- #Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  stateCode <- outcome[,7]
  outcomes <- c("HEART ATTACK", "HEART FAILURE", "PNEUMONIA")
  
  #as.numeric(outcome[,23])
  ## Check that state and outcome are valid
  vali <- function(st, oc){
    library("dplyr")
    `%notin%` <- Negate('%in%')
    st <- toupper(st)
    oc <- toupper(oc)
    if(st %notin% stateCode){
      stop("invalid outcome")
    } else if(oc %notin% outcomes) {
      stop("invalid outcome")
    } else{
      #c(st,oc)
      #outcome %>% filter(stateCode == st, min(pnu, na.rm = TRUE))
      
      }
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}

#test


select(outcome, Hospital.Name, State, starts_with("Hospital.30.Day.Death"))
colnames(outcome)




best <- function(stateCode, disease) {
  ## Read outcome data
  outcome <-read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  ## create new data frame and clean data
  outcomeDF<-select(outcome, Hospital.Name, State, starts_with("Hospital.30.Day.Death"))
  colnames(outcomeDF) <-c("HOSPITAL","STATE","HEART ATTACK","HEART FAILURE","PNEUMONIA")
  outcomeDF[outcomeDF == "Not Available"] <- NA
  outcomeDF[,3:5] <- outcomeDF[,3:5] %>% mutate_if(is.character,as.numeric)
  vali(stateCode, disease)
  ## Check that state and outcome are valid
  vali <- function(st, oc){
    library("dplyr")
    `%notin%` <- Negate('%in%')
    st <- toupper(st)
    oc <- toupper(oc)
    if(st %notin% outcomeDF$STATE){
      stop("invalid outcome")
    } else if(oc %notin% colnames(outcomeDF)) {
      stop("invalid outcome")
    } else 
      calc(st,oc)
  }
  calc <- function(st,oc){
    filter(outcomeDF, outcomeDF$STATE == st) %>% 
    na.omit(outcomeDF)  
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}









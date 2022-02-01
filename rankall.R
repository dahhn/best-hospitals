rankall <- function(outcome, num = "best") {
  require("tidyverse")
  library(data.table)
  `%notin%` <- Negate(`%in%`)
  outcome <- toupper(outcome)
  ## Read outcome data to OC, name columns, change char to numeric
  oc <- read.csv("outcome-of-care-measures.csv") %>%
    select(., Hospital.Name, State, starts_with("Hospital.30.Day.Death"))
  colnames(oc) <- c("HOSPITAL","STATE","HEART ATTACK","HEART FAILURE","PNEUMONIA") 
  oc[oc == "Not Available"] <- NA
  oc[,3:5] <- oc[,3:5] %>% mutate_if(is.character,as.numeric)
  
  ## validate outcome
  if(outcome %notin% colnames(oc)) {stop("invalid outcome")}
  
  ## Create dataframe to analyze based on state
  filtered <- na.omit(oc) %>% 
    select(., HOSPITAL, STATE, `HEART ATTACK`, `HEART FAILURE`, PNEUMONIA) %>% 
    group_by(`STATE`)
  if(outcome == "HEART ATTACK"){arrange(filtered, `HEART ATTACK`, .by_group = T)}
  else if(outcome == "HEART FAILURE"){arrange(filtered, `HEART FAILURE`,.by_group = T)}
  else if(outcome == "PNEUMONIA"){arrange(filtered, PNEUMONIA,.by_group = T)}
}
rankall("heart attack",1)
rankall("heart failure","worst")
rankall("pneumonia", "worst")
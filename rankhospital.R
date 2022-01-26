rankhospital <- function(st, outcome, num = "best") {
  require("tidyverse")
  `%notin%` <- Negate(`%in%`)
  ## Read outcome data to OC, name columns, change char to numeric
  oc <- read.csv("outcome-of-care-measures.csv") %>%
        select(., Hospital.Name, State, starts_with("Hospital.30.Day.Death"))
  colnames(oc) <- c("HOSPITAL","STATE","HEART ATTACK","HEART FAILURE","PNEUMONIA") 
  oc[oc == "Not Available"] <- NA
  oc[,3:5] <- oc[,3:5] %>% mutate_if(is.character,as.numeric)
  ## Check that state and outcome are valid
  st <-toupper(st) 
  outcome <- toupper(outcome)
  if(st %notin% oc$STATE) {stop("invalid state")}
  if(outcome %notin% colnames(oc)){stop("invalid outcome")}
  # Return hospital name in that state with the given rank 30-day death rate
  filtered <- filter(oc, oc$STATE == st) %>%
    na.omit(filtered) %>%
    select(., HOSPITAL, {{outcome}})
  #order(filtered,filtered[1], filtered$HOSPITAL)
  returnDF <- filtered[order(filtered[2]),filtered[1]] %>% 
  
head(returnDF)
}


rankhospital("UT", "Heart Attack", 4)

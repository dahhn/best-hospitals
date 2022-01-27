rankall <- function(outcome, num = "best") {
  require("tidyverse")
  `%notin%` <- Negate(`%in%`)
  outcome <- toupper(outcome)
  if(outcome %notin% colnames(oc)) {stop("invalid outcome")}   ## validate outcome
  
  ## Read outcome data to OC, name columns, change char to numeric
  oc <- read.csv("outcome-of-care-measures.csv") %>%
    select(., Hospital.Name, State, starts_with("Hospital.30.Day.Death"))
  colnames(oc) <- c("HOSPITAL","STATE","HEART ATTACK","HEART FAILURE","PNEUMONIA") 
  oc[oc == "Not Available"] <- NA
  oc[,3:5] <- oc[,3:5] %>% mutate_if(is.character,as.numeric)
  
  #################### Function for for returning a given rank for every state
  ranker <- function(outcome, n = "best"){
    #filter by outcome
    filtered <- na.omit(oc) %>%
      #select(., HOSPITAL, STATE, {{outcome}})
      select(., HOSPITAL, STATE, `HEART ATTACK`) # in return val, remove outcome
    #search for rank by state
    returnDF <- filtered[order(filtered[3],filtered[1]), ] %>% group_by(STATE) 
  head(returnDF)
  
    
    }
  
  
  
  
  ####################
  ## Create dataframe to analyze based on state
  filtered <- na.omit(oc) %>%
    select(., HOSPITAL, STATE, {{outcome}})
  returnDF <- filtered[order(filtered[3],filtered[1]), ]
  
 
  
  
   ## validate 'n' argument and assign value if a character
  if(is.character(n) == TRUE){
    n <- toupper(n)
    if(n == "BEST"){returnDF <- slice(which.min({{outcome}}))}
    else if(n == "WORST"){returnDF <- slice(which.max({{outcome}}))}
  }
  # Return hospital name in that state with the given rank 30-day death rate
  returnDF
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}
head(rankall("heart attack",1))
head(rankall("heart attack","worst"))

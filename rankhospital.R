rankhospital <- function(st, outcome, n = "BEST") {
  require("tidyverse")
  `%notin%` <- Negate(`%in%`)
  st <-toupper(st)
  outcome <- toupper(outcome)
  
  ## Check that state and outcome are valid 
  if(st %notin% oc$STATE) {stop("invalid state")}
  if(outcome %notin% colnames(oc)) {stop("invalid outcome")}

  ## Read outcome data to OC, name columns, change char to numeric
  oc <- read.csv("outcome-of-care-measures.csv") %>%
          select(., Hospital.Name, State, starts_with("Hospital.30.Day.Death"))
  colnames(oc) <- c("HOSPITAL","STATE","HEART ATTACK","HEART FAILURE","PNEUMONIA") 
  oc[oc == "Not Available"] <- NA
  oc[,3:5] <- oc[,3:5] %>% mutate_if(is.character,as.numeric)
  
  ## Create dataframe to analyze based on state
  filtered <- filter(oc, oc$STATE == st) %>%
          na.omit(filtered) %>%
          select(., HOSPITAL, {{outcome}})
  returnDF <- filtered[order(filtered[2],filtered[1]), ]
  ## validate 'n' argument and assign value if a character
  if(is.character(n) == TRUE){
          n <- toupper(n)
          if(n == "BEST"){n <- 1}
          else if(n == "WORST"){n <- length(returnDF$HOSPITAL)}
  }
  # Return hospital name in that state with the given rank 30-day death rate
  returnDF[n,1]
}
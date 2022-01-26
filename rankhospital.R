rankhospital <- function(st, outcome, n = "BEST") {
  
  require("tidyverse")
  require(stats)
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
  if(is.character(n) == TRUE){
      toupper(n)  
      if(n != "BEST" || n!= "WORST" ){{stop("invalid rank")}}
      else if(n == "BEST"){n <- 1}
      else if(n == "WORST"){n <- length(oc$STATE)}
  } else{}
  if(st %notin% oc$STATE) {stop("invalid state")}
  if(outcome %notin% colnames(oc)){stop("invalid outcome")}
  
  # Return hospital name in that state with the given rank 30-day death rate
  filtered <- filter(oc, oc$STATE == st) %>%
    na.omit(filtered) %>%
    select(., HOSPITAL, {{outcome}})
  
  #order(filtered,filtered[1], filtered$HOSPITAL)
  returnDF <- filtered[order(filtered[2],filtered[1]), ]
  #rankCol <- c(1:length(returnDF[1]))
  #rbind(returnDF,rankCol)
  test <- rank(returnDF[,2]) %>% rbind(returnDF, .)
  returnDF[n,1]
}


rankhospital("UT", "Heart Attack", "WORST")

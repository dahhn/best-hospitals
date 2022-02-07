rankall <- function(outcome, num = "best") {
  require("tidyverse")
  library(data.table)
  `%notin%` <- Negate(`%in%`)
  outcome <- toupper(outcome)
  
  # Read outcome data to OC, name columns, change char to numeric
  oc <<- read.csv("outcome-of-care-measures.csv") %>%
    select(., Hospital.Name, State, starts_with("Hospital.30.Day.Death"))
  colnames(oc) <- c("HOSPITAL","STATE","HEART ATTACK","HEART FAILURE","PNEUMONIA")
  oc[oc == "Not Available"] <<- NA
  oc[,3:5] <- oc[,3:5] %>% mutate_if(is.character,as.numeric)
  oc <<-tibble(oc) %>% group_by(`STATE`)
  # validate outcome
  if(outcome %notin% colnames(oc)) {stop("invalid outcome")}
  
  # function to evaluate arguements 
    x<-data.frame()
    stateList <- unique(oc$STATE)
    if(outcome == "HEART ATTACK"){
      selMin <- function(df)return(df[which.min(df$`HEART ATTACK`),])
      selMax <- function(df)return(df[which.max(df$`HEART ATTACK`),] )
    }
    else if(outcome == "HEART FAILURE"){
      selMin <- function(df)return(df[which.min(df$`HEART FAILURE`),])
      selMax <- function(df)return(df[which.max(df$`HEART FAILURE`),] )
    }
    else if(outcome == "PNEUMONIA"){
      selMin <- function(df)return(df[which.min(df$`PNEUMONIA`),])
      selMax <- function(df)return(df[which.max(df$`PNEUMONIA`),]) 
    }

    if(is.character(num) == T){
      num <- toupper(num)
      stateList <- unique(oc$STATE)
      if(num == "BEST"){
        for (i in 1:54){
          df <- filter(oc, STATE == stateList[i])
          row1 <- selMin(df)
          x<-rbind(row1,x)
        }
        x
        }
      else if(num == "WORST"){
        for (i in 1:54){
          df <- filter(oc, STATE == stateList[i])
          row1 <- selMax(df)
          x<-rbind(row1,x)
        }
        
        }
      }
    else {
      for (i in 1:54){
        df <- filter(oc, STATE == stateList[i])
        row1 <- slice(df,n = num)
        x<-rbind(row1,x)
      }
    } 
    select(x, HOSPITAL, STATE) %>% arrange(STATE)    
}

rankall("pneumonia", "best")
tail(rankall("pneumonia", "worst"), 3)
rankall("heart attack",1)
rankall("heart attack","best")
rankall("heart attack","worst")
rankall("heart failure","best")






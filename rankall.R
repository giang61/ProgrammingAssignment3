#outcome <- read.csv("outcome-of-care-measures.csv",colClasses="character")
source("rankhospital.R")
rankall <- function(type, num){
  # check inputs
  if (!any(type == c("heart attack", "heart failure", "pneumonia"))){
    stop("Invalid Outcome")
  }
  
  hospitals <- sapply(state.abb, rankhospital, type, num)
  
  return(t(hospitals))
}
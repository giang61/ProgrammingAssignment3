#outcome <- read.csv("outcome-of-care-measures.csv",colClasses="character")
by_state <- split(outcome,outcome$State)
best <- function(state, type){
  # check inputs
  if (!any(state == state.abb)){
    stop("Invalid State")
  }
  if (!any(type == c("heart attack", "heart failure", "pneumonia"))){
    stop("Invalid Outcome")
  }
  
  # subset by state
  my_state <- by_state[[state]]
  
  # find min death rates
  if(type=="heart attack") column <- 11
  else if(type=="heart failure") column <- 17
  else column <- 23
  
  type_list <- sapply(my_state[,column], function(x) as.numeric(as.character(x)))
  hosp_list <- my_state[,2]
  new_table <- data.frame(hosp_list,type_list)
  
  min_rows <- which(new_table[,2]==min(new_table[,2],na.rm=TRUE))
  
  # find corresponding hospital(s)
  best_hosp <- new_table[min_rows,c(1,2)]
  
  return (best_hosp)
 
}
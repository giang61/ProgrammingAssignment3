#outcome <- read.csv("outcome-of-care-measures.csv",colClasses="character")
by_state <- split(outcome,outcome$State)
rankhospital <- function(state, type, num){
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
  
  non_na <- which(new_table[,2]!=is.na(new_table[,2]))
  retained_hosp <- new_table[non_na,c(1,2)]
  sorted_rows <- order(retained_hosp[,2],retained_hosp[,1])
  sorted_hosp <- retained_hosp[sorted_rows,c(1,2)]
  
  if(num=="worst") return(tail(sorted_hosp,n=1))
  if (num=="best") num <- 1
  return(sorted_hosp[num,1])
}
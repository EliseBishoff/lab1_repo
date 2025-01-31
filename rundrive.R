rundrive <- function(state, team) {

  state <- state + rnorm(1, 55, 15)
  team <- team * -1                
  list(state = state, team = team) 
}

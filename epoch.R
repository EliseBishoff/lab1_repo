
source("rundrive.R")
epoch <- function() {
  state <- rnorm(1, 50, 15) 
  team <- 1                 
  if(state>100&&state<=110){
    score=7*team
  }
  if(state>110&&state<=120){
    score=3*team
  }

  while (state <= 100) {
    result <- rundrive(state, team) 
    state <- result$state          
    team <- result$team             
  }
  

  c(score)
}

epoch()
score

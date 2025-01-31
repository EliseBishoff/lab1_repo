epoch <- function(down, ytg, fp) {
  team <- 1
  total_score <- 0
  
  for (time in 1:10) {  
    result <- drive(team, down, ytg, fp)
    team <- result$team
    down <- result$down
    ytg <- result$ytg
    fp <- result$fp
    total_score <- total_score + result$score
  }
  
  return(total_score)
}
epoch <- function(down, ytg, fp) {
  team <- 1
  total_score <- 0
  time=1
  
 while(total_score==0&&time<11) {  
    result <- drive(team, down, ytg, fp)
    team <- result$team
    down <- result$down
    ytg <- result$ytg
    fp <- result$fp
    total_score <- total_score + result$score
    time=time+1
  }
  
  return(total_score)
}

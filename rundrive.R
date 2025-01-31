drive <- function(team, down, ytg, fp) {
  score <- 0
  
  while (fp<100&&down <= 4) {
    gain <- max(0,rnorm(1,2,1))
    fp <- fp + gain
    ytg <- ytg - gain
    
    if (ytg <= 0) {  
      down <- 1
      ytg <- 10
    } else {
      down <- down + 1
    }
    
    if (fp >= 100&&fp<110) {  
      
      score <- score + 7 * team  
      break  
    }
    if (fp<=120&&fp>110){
      score<-score+3*team
      break
    }
    
  }
  
  
  
  fp <- 100-fp
  ytg <- 10  
  down <- 1  
  team <- -team  
  
  return(list(team = team, down = down, ytg = ytg, fp = fp, score = score))
}

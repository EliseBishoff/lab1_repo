top <- function(down, ytg, fp, n = 1000) {
  scores <- numeric(n) 
  
  for (i in 1:n) {
    scores[i] <- epoch(down, ytg, fp)  
  }
  
  return(mean(scores))  
}
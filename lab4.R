
top <- function(down, ytg, fp, n = 1000) {
  scores <- numeric(n)
  
  for (i in 1:n) {
    scores[i] <- epoch(down, ytg, fp)  
  }
  
  return(mean(scores))  
}


drive <- function(team, down, ytg, fp) {
  score <- 0
  while (fp < 100 && down <= 4) {
    # Try field goal if it's relatively close to the end and it is the 4th down
    if (down == 4 && fp > 60 && fp < 90) {
      probabilities <- c(0.2, 0.8)  # success with 0.2 probability
      success <- sample(1:2, 1, prob = probabilities)
      
      if (success == 1) {
        score <- score + 3 * team
        fp <- -15
        ytg <- 10
        down <- 1
        team <- -team
        return(list(team = team, down = down, ytg = ytg, fp = fp, score = score))
      }
      
      if (success == 2) {
        probabilities <- c(0.5, 0.5)
        fp <- sample(c(25, 1 - fp), 1, prob = probabilities)
        ytg <- 10
        down <- 1
        team <- -team
        return(list(team = team, down = down, ytg = ytg, fp = fp, score = score))
      }
    }
    
    # Execute punt if the field play is close to own end zone
    if (down == 4 && fp < 55) {
      fp <- rnorm(1, 30, 10)
      ytg <- 10
      down <- 1
      team <- -team
      return(list(team = team, down = down, ytg = ytg, fp = fp, score = score))
    }
    
    # Execute "go for it" logic
    gain <- max(0, rnorm(1, 2, 1))
    fp <- fp + gain
    ytg <- ytg - gain
    
    if (ytg <= 0) {
      down <- 1
      ytg <- 10
    } else {
      down <- down + 1
    }
    
    if (fp >= 100 && fp < 110) {
      score <- score + 7 * team
      break
    }
    
    if (fp <= 120 && fp > 110) {
      score <- score + 3 * team
      break
    }
  }
  
  fp <- 100 - fp
  ytg <- 10  
  down <- 1  
  team <- -team  
  
  return(list(team = team, down = down, ytg = ytg, fp = fp, score = score))
}


epoch <- function(down, ytg, fp) {
  team <- 1
  total_score <- 0
  time <- 1
  
  while (total_score == 0 && time < 11) {  
    result <- drive(team, down, ytg, fp)
    team <- result$team
    down <- result$down
    ytg <- result$ytg
    fp <- result$fp
    total_score <- total_score + result$score
    time <- time + 1
  }
  
  return(total_score)
}

# Initialize parameters
down <- 1    
ytg <- 10    
fp <- 50   
n <- 1000    

# Run epoch function
epoch_score <- epoch(down, ytg, fp)
average_score <- top(down, ytg, fp, n)
print(average_score)

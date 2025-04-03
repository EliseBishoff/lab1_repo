prob_table <- data.frame(
  yard_bin = c("[0,10)", "[10,20)", "[20,30)", "[30,40)", "[40,50)", 
               "[50,60)", "[60,70)", "[70,80)", "[80,90)", "[90,100]"),
  FG_prob = c(0, 0, 0, 0, 0, 0.0105, 0.438, 0.763, 0.791, 0.637),
  Punt_prob = c(0.93, 0.928, 0.921, 0.862, 0.812, 0.678, 0.171, 0.000962, 0, 0)
)

drive <- function(team, down, ytg, fp) {
  score <- 0
  while (fp < 100 && down <= 4) {
    if (down == 4) {
      bin <- cut(fp, breaks = seq(0, 100, by = 10), include.lowest = TRUE, right = FALSE)
      if (is.na(bin)) {
        bin <- "[90,100]"
      }
      prob_row <- prob_table[prob_table$yard_bin == as.character(bin), ]
      if (nrow(prob_row) == 0) {
        fg_prob <- 0
        punt_prob <- 1
      } else {
        fg_prob <- prob_row$FG_prob
        punt_prob <- prob_row$Punt_prob
      }
      go_prob <- max(0, 1 - fg_prob - punt_prob)
      total_prob <- fg_prob + punt_prob + go_prob
      fg_prob <- fg_prob / total_prob
      punt_prob <- punt_prob / total_prob
      go_prob <- go_prob / total_prob
      decision <- sample(c("FG", "Punt", "GoForIt"), size = 1, 
                         prob = c(fg_prob, punt_prob, go_prob))
      if (decision == "FG") {
        fg_success <- sample(1:2, 1, prob = c(0.2, 0.8))
        if (fg_success == 1) {
          score <- score + 3 * team
          fp <- -15
          ytg <- 10
          down <- 1
          team <- -team
          return(list(team = team, down = down, ytg = ytg, fp = fp, score = score))
        } else {
          fp <- sample(c(25, 1 - fp), 1, prob = c(0.5, 0.5))
          ytg <- 10
          down <- 1
          team <- -team
          return(list(team = team, down = down, ytg = ytg, fp = fp, score = score))
        }
      } else if (decision == "Punt") {
        fp <- rnorm(1, mean = 30, sd = 10)
        ytg <- 10
        down <- 1
        team <- -team
        return(list(team = team, down = down, ytg = ytg, fp = fp, score = score))
      } else if (decision == "GoForIt") {
        gain <- max(0, rnorm(1, mean = 2, sd = 1))
        fp <- fp + gain
        ytg <- ytg - gain
        if (ytg <= 0) {
          down <- 1
          ytg <- 10
        } else {
          down <- down + 1
        }
      }
    } else {
      gain <- max(0, rnorm(1, mean = 2, sd = 1))
      fp <- fp + gain
      ytg <- ytg - gain
      if (ytg <= 0) {
        down <- 1
        ytg <- 10
      } else {
        down <- down + 1
      }
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

top <- function(down, ytg, fp, n = 1000) {
  scores <- numeric(n)
  for (i in 1:n) {
    scores[i] <- epoch(down, ytg, fp)
  }
  return(mean(scores))
}

down <- 1
ytg <- 10
fp <- 50
n <- 1000

epoch_score <- epoch(down, ytg, fp)
average_score <- top(down, ytg, fp, n)
print(average_score)




down <- 1   
ytg <- 10    
fp <- 50     
n <- 1000    


epoch_score <- epoch(down, ytg, fp)
average_score <- top(down, ytg, fp, n)
print(average_score)

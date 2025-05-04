
library(nnet)

predict_FG_prob <- function(fp) {
  c0 <- -6.361913
  c1 <- 0.106210
  logit <- c0 + c1 * fp
  return(exp(logit) / (1 + exp(logit))) 
}
predict_decision_probs <- function(fp, ydstogo) {
  a0 <- -1.373438
  a1 <- 0.0854324
  a2 <- -0.19078054
  b0 <- -6.125479
  b1 <- 0.1747688
  b2 <- -0.04108412
  
  log_go <- a0 + a1 * fp + a2 * ydstogo
  log_punt <- b0 + b1 * fp + b2 * ydstogo
  
  exp_go <- exp(log_go)
  exp_punt <- exp(log_punt)
  exp_fg <- 1
  
  total <- exp_fg + exp_go + exp_punt
  return(c(FG = exp_fg / total, Punt = exp_punt / total, GoForIt = exp_go / total))
}

drive <- function(team, down, ytg, fp) {
  score <- 0
  while (fp < 100 && down <= 4) {
    if (down == 4) {
      fg_prob <- predict_FG_prob(fp)
      decision_probs <- predict_decision_probs(fp, ytg)
      go_prob <- decision_probs["GoForIt"]
      punt_prob <- decision_probs["Punt"]
      total_prob <- fg_prob + punt_prob + go_prob
      fg_prob <- fg_prob / total_prob
      punt_prob <- punt_prob / total_prob
      go_prob <- go_prob / total_prob
      
      decision <- sample(c("FG", "Punt", "GoForIt"), size = 1, 
                         prob = c(fg_prob, punt_prob, go_prob))
      
      if (decision == "FG") {
        fg_success <- sample(c(1, 0), 1, prob = c(fg_prob, 1 - fg_prob))
        if (fg_success == 1) {
          score <- score + 3 * team
          return(list(team = -team, down = 1, ytg = 10, fp = -15, score = score))
        } else {
          fp <- sample(c(25, 1 - fp), 1, prob = c(0.5, 0.5))
          return(list(team = -team, down = 1, ytg = 10, fp = fp, score = score))
        }
      } else if (decision == "Punt") {
        fp <- rnorm(1, mean = 30, sd = 10)
        return(list(team = -team, down = 1, ytg = 10, fp = fp, score = score))
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
    if (fp >= 100) {
      score <- score + 7 * team
      break
    }
  }
  return(list(team = -team, down = 1, ytg = 10, fp = 100 - fp, score = score))
}


down <- 1
ytg <- 10
fp <- 50
n <- 1000

epoch_score <- epoch(down, ytg, fp)
average_score <- top(down, ytg, fp, n)
print(average_score)

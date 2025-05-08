drive <- function(team, down, ytg, fp, fg_value = 3) {
  score <- 0
  
  while (fp < 100 && down <= 4) {
    if (down == 4) {
      # Bin field position for 4th down decision
      bin <- cut(fp, breaks = seq(0, 100, by = 10), include.lowest = TRUE, right = FALSE)
      prob_row <- prob_table[prob_table$yard_bin == as.character(bin), ]
      
      # Set fallback if bin missing
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
      
      decision <- sample(c("FG", "Punt", "GoForIt"), 1, prob = c(fg_prob, punt_prob, go_prob))
      
      if (decision == "FG") {
        fg_success <- sample(1:2, 1, prob = c(0.2, 0.8))  # Can plug in model here
        if (fg_success == 1) {
          score <- score + fg_value * team
          return(list(team = -team, down = 1, ytg = 10, fp = 25, score = score))
        } else {
          fp <- sample(c(25, 100 - fp), 1, prob = c(0.5, 0.5))
          return(list(team = -team, down = 1, ytg = 10, fp = fp, score = score))
        }
      } else if (decision == "Punt") {
        fp <- rnorm(1, mean = 30, sd = 10)
        return(list(team = -team, down = 1, ytg = 10, fp = fp, score = score))
      } else {
        gain_obj <- simulate_yards_gained(fp)
        fp <- fp + gain_obj$yards_gained
        ytg <- ytg - gain_obj$yards_gained
        if (gain_obj$turnover) {
          return(list(team = -team, down = 1, ytg = 10, fp = 100 - fp, score = score))
        }
        if (ytg <= 0) {
          down <- 1
          ytg <- 10
        } else {
          down <- down + 1
        }
      }
    } else {
      gain_obj <- simulate_yards_gained(fp)
      fp <- fp + gain_obj$yards_gained
      ytg <- ytg - gain_obj$yards_gained
      if (gain_obj$turnover) {
        return(list(team = -team, down = 1, ytg = 10, fp = 100 - fp, score = score))
      }
      if (ytg <= 0) {
        down <- 1
        ytg <- 10
      } else {
        down <- down + 1
      }
    }
    
    # Touchdown or end-of-field conditions
    if (fp >= 100 && fp < 110) {
      score <- score + 7 * team
      break
    }
    if (fp >= 110 && fp <= 120) {
      score <- score + fg_value * team
      break
    }
  }

  return(list(team = -team, down = 1, ytg = 10, fp = 100 - fp, score = score))
}

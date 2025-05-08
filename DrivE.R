drive <- function(team, down, ytg, fp,
                  rule_config,
                  probabilities, rush_mixtures, pass_mixtures, prob_table) {
  score   <- 0
  outcome <- NA_character_
  
  if (any(is.na(c(team, down, ytg, fp)))) {
    return(list(team=-team, down=1, ytg=rule_config$yards_to_gain,
                fp=25, score=0, outcome="BadInput"))
  }
  
  while (fp < 100 && down <= rule_config$downs_allowed) {
    ## 4TH DOWN DECISION
    if (down == rule_config$downs_allowed) {
      fg_p   <- predict_FG_prob(fp)
      dec_ps <- predict_decision_probs(fp, ytg)
      p_vec  <- c(FG=fg_p, Punt=dec_ps["Punt"], GoForIt=dec_ps["GoForIt"])
      if (any(is.na(p_vec)) || sum(p_vec)==0) break
      p_vec <- p_vec / sum(p_vec)
      decision <- sample(names(p_vec), 1, prob=p_vec)
      
      if (decision == "FG") {
        if (runif(1) < fg_p) {
          score   <- rule_config$fg_points * team
          outcome <- "FG"
        } else {
          outcome <- "MissFG"
        }
        return(list(
          team    = -team,
          down    = 1,
          ytg     = rule_config$yards_to_gain,
          fp      = 100 - fp,
          score   = score,
          outcome = outcome
        ))
      }
      
      if (decision == "Punt") {
        punt_yd <- rnorm(1, mean=40, sd=10)
        new_fp  <- max(1, 100 - (fp + punt_yd))
        outcome <- "Punt"
        return(list(
          team    = -team,
          down    = 1,
          ytg     = rule_config$yards_to_gain,
          fp      = new_fp,
          score   = 0,
          outcome = outcome
        ))
      }
    }
    
    ## TO
    play <- simulate_yards_gained(fp, probabilities, rush_mixtures, pass_mixtures)
    if (play$turnover) {
      outcome <- "Turnover"
      return(list(
        team    = -team,
        down    = 1,
        ytg     = rule_config$yards_to_gain,
        fp      = 100 - fp,
        score   = 0,
        outcome = outcome
      ))
    }
    
    gain <- ifelse(is.na(play$yards_gained), 0, play$yards_gained)
    fp   <- fp + gain
    ytg  <- ytg - gain
    
    if (ytg <= 0) {
      down <- 1
      ytg  <- rule_config$yards_to_gain
    } else {
      down <- down + 1
    }
    
    ## TD
    if (fp >= 100) {
      score   <- rule_config$td_points * team
      outcome <- "TD"
      return(list(
        team    = -team,
        down    = 1,
        ytg     = rule_config$yards_to_gain,
        fp      = 25,
        score   = score,
        outcome = outcome
      ))
    }
  }
  
  ## NO SCORE OR TO
  outcome <- "TurnoverOnDowns"
  return(list(
    team    = -team,
    down    = 1,
    ytg     = rule_config$yards_to_gain,
    fp      = 100 - fp,
    score   = score,
    outcome = outcome
  ))
}

top <- function(n = 1000,
                rule_config,
                probabilities,
                rush_mixtures,
                pass_mixtures,
                prob_table) {
  
  mat <- replicate(n, epoch(
    start_fp     = 25,
    ytg          = rule_config$yards_to_gain,
    rule_config  = rule_config,
    probabilities= probabilities,
    rush_mixtures= rush_mixtures,
    pass_mixtures= pass_mixtures,
    prob_table   = prob_table
  ))
  
  df <- as.data.frame(t(mat))
  names(df) <- c("Team1","Team2","Total")
  df
}

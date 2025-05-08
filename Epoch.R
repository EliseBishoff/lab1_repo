epoch <- function(start_fp      = 25,
                  rule_config,
                  probabilities,
                  rush_mixtures,
                  pass_mixtures,
                  prob_table,
                  n              = 2000) {
  
  scores <- numeric(n)
  events <- character(n)
  
  for (i in seq_len(n)) {
    d <- drive(
      team         = 1,
      down         = 1,
      ytg          = rule_config$yards_to_gain,
      fp           = start_fp,
      rule_config  = rule_config,
      probabilities= probabilities,
      rush_mixtures= rush_mixtures,
      pass_mixtures= pass_mixtures,
      prob_table   = prob_table
    )
    scores[i] <- d$score
    events[i] <- d$outcome
  }
  
  ## FINDING EP and PROBS
  ep    <- mean(scores)
  p_td  <- mean(events == "TD")
  p_fg  <- mean(events == "FG")
  p_mfg <- mean(events == "MissFG")
  p_pt  <- mean(events == "Punt")
  p_to  <- mean(events == "Turnover" | events == "TurnoverOnDowns")
  
  data.frame(
    start_fp = start_fp,
    EP        = ep,
    P_TD      = p_td,
    P_FG      = p_fg,
    P_MissFG  = p_mfg,
    P_Punt    = p_pt,
    P_TO      = p_to,
    row.names = NULL
  )
}

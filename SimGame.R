simulate_game <- function(start_fp = 25,
                          ytg = 10,
                          down = 1,
                          rule_config,
                          probabilities,
                          rush_mixtures,
                          pass_mixtures,
                          prob_table) {
  score1 <- 0; score2 <- 0; team <- 1; fp <- start_fp
  
  for (i in seq_len(rule_config$max_drives)) {
    down <- ifelse(is.na(down)||down<1,1,down)
    fp   <- ifelse(is.na(fp),25,fp)
    ytg  <- ifelse(is.na(ytg),rule_config$yards_to_gain,ytg)
    
    res <- drive(team, down, ytg, fp,
                 rule_config, probabilities, rush_mixtures, pass_mixtures, prob_table)
    
    if (team==1) score1 <- score1 + res$score else score2 <- score2 + res$score
    
    team <- res$team; down <- res$down; ytg <- res$ytg; fp <- res$fp
  }
  
  c(team1 = score1, team2 = score2, total = score1 + score2)
}

simulate_game <- function(start_fp = 25, max_drives = 20, fg_value = 3) {
  down <- 1
  ytg <- 10
  fp <- start_fp
  team <- 1  # Team 1 starts
  score_team1 <- 0
  score_team2 <- 0
  
  for (i in 1:max_drives) {
    result <- drive(team, down, ytg, fp, fg_value)
    
    if (team == 1) {
      score_team1 <- score_team1 + result$score
    } else {
      score_team2 <- score_team2 + result$score
    }
    
    # Update state for next drive
    team <- result$team
    down <- result$down
    ytg <- result$ytg
    fp <- result$fp
  }
  
  return(list(team1 = score_team1, team2 = score_team2, total = score_team1 + score_team2))
}

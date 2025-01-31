# This is just an initial file for part 1 of lab 3; very much subject to change
library(dplyr)

game_data <- readRDS("data.rds")

# Get list of unique teams
teams <- unique(c(game_data$Visiting_Team, game_data$Home_Team))
n_teams <- length(teams)

# Create an empty transition matrix
transition_matrix <- matrix(0, nrow = n_teams, ncol = n_teams, dimnames = list(teams, teams))

# Populate the matrix
for (i in 1:nrow(game_data)) {
  visitor <- game_data$Visiting_Team[i]
  home <- game_data$Home_Team[i]
  
  if (game_data$Visiting_Score[i] < game_data$Home_Score[i]) {
    transition_matrix[visitor, home] <- transition_matrix[visitor, home] + 1
  } else {
    transition_matrix[home, visitor] <- transition_matrix[home, visitor] + 1
  }
}

# Normalize each row to sum to 1 (Markov property)
transition_matrix <- sweep(transition_matrix, 1, rowSums(transition_matrix), FUN = "/")
transition_matrix[is.na(transition_matrix)] <- 0  # Handle NaN cases (teams with no losses)

# Initialize probability vector (uniform distribution)
p <- rep(1 / n_teams, n_teams)

# Run Markov chain until convergence
for (i in 1:10000) {
  p <- p %*% transition_matrix
}

# Convert to named vector 
rankings <- setNames(as.numeric(p), teams)
rankings <- sort(rankings, decreasing = TRUE)

rankings

# Just a list of the rankings (teams in order)
cat("#", paste(names(rankings), collapse = ", "), "\n")

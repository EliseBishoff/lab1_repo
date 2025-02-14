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


## part 2

## remaking list of unique team
teams <- unique(c(game_data$Visiting_Team, game_data$Home_Team))
n_teams <- length(teams)
##sets weight on interleague games, will be djusted to compare rakingws at different a values.
alpha_values <- seq(0, 1, by = 0.2)


steady_states <- list()
  # Create an empty transition matrix with teams as row and column name, # Loop over each game to populate the transition matrix,     
# Determine whether the game is interleague or intraleague.
    # If the leagues are the same, weight is 1; otherwise, weight is alpha.
for(alpha in alpha_values) {
  transition_matrix <- matrix(0, nrow = n_teams, ncol = n_teams, 
                              dimnames = list(teams, teams))
  for (i in 1:nrow(game_data)) {
    visitor <- game_data$Visiting_Team[i]
    home <- game_data$Home_Team[i]
    if (game_data$Visiting_League[i] == game_data$Home_League[i]) {
      weight <- 1
    } else {
      weight <- alpha
    }

 ##normalizing each row   
  row_sums <- rowSums(transition_matrix)
  transition_matrix <- sweep(transition_matrix, 1, row_sums, FUN = "/")
  transition_matrix[is.na(transition_matrix)] <- 0
 
##creating/assigning prob vector   
p <- rep(1 / n_teams, n_teams)
 
##making steady state
for (j in 1:10000) {
    p <- p %*% transition_matrix
  }

##saving ranking for current vector    
steady_states[[as.character(alpha)]] <- setNames(as.numeric(p), teams)
}

##computes and generates ranking
  
for(alpha in names(steady_states)) {
  cat("Alpha =", alpha, "\n")
  ranking <- sort(steady_states[[alpha]], decreasing = TRUE)
  cat("Ranking order:", paste(names(ranking), collapse = ", "), "\n")
  cat("Steady state probabilities:", paste(round(ranking, 4), collapse = ", "), "\n\n")
}


## Generating output for comparison

library(ggplot2)
library(reshape2)

## making data frame

lab3_df <- data.frame(Team = teams)
for (alpha in names(steady_states)) {
  alpha_vector <- steady_states[[alpha]]
  df_alpha <- data.frame(Team = names(alpha_vector), Probability = as.numeric(alpha_vector))
  steady_df <- merge(lab3_df, df_alpha, by = "Team", all.x = TRUE, suffixes = c("", paste0("_alpha", alpha)))
}

## plotting sample steady state probs
selected_teams <- c("TeamA", "TeamB", "TeamC")  # replace with actual team names from your data
plot_data <- subset(steady_long, Team %in% selected_teams)




## GG plot
  ggplot(plot_data, aes(x = Alpha, y = Probability, group = Team, color = Team)) +
  geom_line() +
  geom_point() +
  labs(title = "Steady State Probs vs. Interleague Weight (Alpha)",
       x = "Alpha (Interleague Weight)",
       y = "Steady State Probs")


  


library(dplyr)
simulate_yards_gained <- function(yardline, probabilities, rush_mixtures, pass_mixtures) {
  yard_group_chr <- as.character(
    cut(yardline, breaks = seq(0, 100, by = 10), right = FALSE)
  )
  probs <- probabilities %>%
    filter(as.character(yard_group) == yard_group_chr)
  
  if (nrow(probs) != 1 || probs$total_plays <= 0 || is.na(probs$total_plays)) {
    return(list(yards_gained = 0, turnover = TRUE))
  }
  
  p_rush <- probs$total_rush / probs$total_plays
  p_pass <- probs$total_pass / probs$total_plays
  
  play_type <- sample(c("rush", "pass"), 1, prob = c(p_rush, p_pass))
  
  if (play_type == "rush") {
    if (runif(1) < probs$fumble_rush_prob) {
      return(list(yards_gained = 0, turnover = TRUE))
    }
    model_row <- rush_mixtures %>%
      filter(as.character(yard_group) == yard_group_chr)
    if (nrow(model_row) != 1) {
      return(list(yards_gained = 0, turnover = FALSE))
    }
    model <- model_row$mixture_model[[1]]
    comp  <- sample(seq_along(model$weights), 1, prob = model$weights)
    μ     <- model$means[comp]
    σ     <- sqrt(model$variances[comp])
    if (is.na(μ)||is.na(σ)) {
      return(list(yards_gained = 0, turnover = FALSE))
    }
    gain <- rnorm(1, μ, σ)
    return(list(yards_gained = round(pmin(gain, yardline)), turnover = FALSE))
    
  } else {
    if (runif(1) < probs$interception_pass_prob ||
        runif(1) < probs$fumble_pass_prob) {
      return(list(yards_gained = 0, turnover = TRUE))
    }
    if (runif(1) < probs$incomplete_pass_prob) {
      return(list(yards_gained = 0, turnover = FALSE))
    }
    model_row <- pass_mixtures %>%
      filter(as.character(yard_group) == yard_group_chr)
    if (nrow(model_row) != 1) {
      return(list(yards_gained = 0, turnover = FALSE))
    }
    model <- model_row$mixture_model[[1]]
    comp  <- sample(seq_along(model$weights), 1, prob = model$weights)
    μ     <- model$means[comp]
    σ     <- sqrt(model$variances[comp])
    if (is.na(μ)||is.na(σ)) {
      return(list(yards_gained = 0, turnover = FALSE))
    }
    gain <- rnorm(1, μ, σ)
    return(list(yards_gained = round(pmin(gain, yardline)), turnover = FALSE))
  }
}


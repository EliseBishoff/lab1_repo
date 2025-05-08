library(dplyr)
library(mclust)

data <- data %>%
  mutate(
    yardline_100 = 100 - yardline_100,  
    yard_group = cut(yardline_100, breaks = seq(0, 100, by = 10), right = FALSE)
  )


probabilities <- data %>%
  group_by(yard_group) %>%
  summarise(
    total_plays = n(),
    total_rush = sum(rush_attempt == 1),
    total_pass = sum(pass_attempt == 1),
    
    
    fumble_rush = sum(fumble == 1 & rush_attempt == 1),
    fumble_rush_prob = ifelse(total_rush > 0, fumble_rush / total_rush, 0),
    
    
    interception_pass = sum(interception == 1 & pass_attempt == 1),
    interception_pass_prob = ifelse(total_pass > 0, interception_pass / total_pass, 0),
    
    fumble_pass = sum(fumble == 1 & pass_attempt == 1),
    fumble_pass_prob = ifelse(total_pass > 0, fumble_pass / total_pass, 0),
    
    incomplete_pass = sum(pass_attempt == 1 & yards_gained == 0 & interception == 0 & fumble == 0),
    incomplete_pass_prob = ifelse(total_pass > 0, incomplete_pass / total_pass, 0),
    
    completion_pass_prob = ifelse(total_pass > 0, 1 - interception_pass_prob - fumble_pass_prob - incomplete_pass_prob, 0),
    
    avg_yards_rush = ifelse(total_rush - fumble_rush > 0,
                            mean(yards_gained[rush_attempt == 1 & fumble == 0], na.rm = TRUE),
                            NA),
    avg_yards_pass_completion = ifelse(total_pass - (interception_pass + fumble_pass + incomplete_pass) > 0,
                                       mean(yards_gained[pass_attempt == 1 & interception == 0 & fumble == 0 & yards_gained != 0], na.rm = TRUE),
                                       NA)
  )

fit_mixture <- function(data, k = 2) {
  if (length(data) < k) return(list(means = mean(data), variances = var(data), weights = 1))
  model <- Mclust(data, G = k, verbose = FALSE)
  if (is.null(model)) return(list(means = mean(data), variances = var(data), weights = 1))
  list(
    means = model$parameters$mean,
    variances = model$parameters$variance$sigmasq,
    weights = model$parameters$pro
  )
}


rush_mixtures <- data %>%
  filter(rush_attempt == 1, fumble == 0) %>%
  group_by(yard_group) %>%
  summarise(
    mixture_model = list(fit_mixture(yards_gained))
  )


pass_completion_mixtures <- data %>%
  filter(pass_attempt == 1, interception == 0, fumble == 0, yards_gained != 0) %>%
  group_by(yard_group) %>%
  summarise(
    mixture_model = list(fit_mixture(yards_gained))
  )
simulate_yards_gained <- function(yardline) {
  yard_group <- cut(yardline, breaks = seq(0, 100, by = 10), right = FALSE)
  probs <- probabilities[probabilities$yard_group == yard_group, ]
  
  play_type <- sample(c("rush", "pass"), 1, prob = c(probs$total_rush / probs$total_plays, probs$total_pass / probs$total_plays))
  
  if (play_type == "rush") {
    if (runif(1) < probs$fumble_rush_prob) {
      return(list(yards_gained = 0, turnover = TRUE))
    } else {
      model <- rush_mixtures$mixture_model[rush_mixtures$yard_group == yard_group][[1]]
      if (is.null(model)) {
        yards <- probs$avg_yards_rush
      } else {
        component <- sample(seq_along(model$weights), 1, prob = model$weights)
        yards <- rnorm(1, model$means[component], sqrt(model$variances[component]))
      }
      yards <- min(yards, yardline)
      return(list(yards_gained = round(yards), turnover = FALSE))
    }
  } else {
    if (runif(1) < probs$interception_pass_prob) {
      return(list(yards_gained = 0, turnover = TRUE))
    }
    if (runif(1) < probs$fumble_pass_prob) {
      return(list(yards_gained = 0, turnover = TRUE))
    }
    if (runif(1) < probs$incomplete_pass_prob) {
      return(list(yards_gained = 0, turnover = FALSE))
    }
    model <- pass_completion_mixtures$mixture_model[pass_completion_mixtures$yard_group == yard_group][[1]]
    if (is.null(model)) {
      yards <- probs$avg_yards_pass_completion
    } else {
      component <- sample(seq_along(model$weights), 1, prob = model$weights)
      yards <- rnorm(1, model$means[component], sqrt(model$variances[component]))
    }
    yards <- min(yards, yardline)
    return(list(yards_gained = round(yards), turnover = FALSE))
  }
}

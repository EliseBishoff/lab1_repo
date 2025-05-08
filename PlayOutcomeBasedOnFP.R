
simulate_yards_gained <- function(yardline) {
  yard_group <- cut(yardline, breaks = seq(0, 100, by = 10), right = FALSE)
  probs <- probabilities[probabilities$yard_group == yard_group, ]
  
  play_type <- sample(c("rush", "pass"), 1,
                      prob = c(probs$total_rush / probs$total_plays,
                               probs$total_pass / probs$total_plays))
  
  if (play_type == "rush") {
    if (runif(1) < probs$fumble_rush_prob) {
      return(list(yards_gained = 0, turnover = TRUE))
    } else {
      model <- rush_mixtures$mixture_model[rush_mixtures$yard_group == yard_group][[1]]
      component <- sample(seq_along(model$weights), 1, prob = model$weights)
      yards <- rnorm(1, model$means[component], sqrt(model$variances[component]))
      yards <- min(yards, yardline)
      return(list(yards_gained = round(yards), turnover = FALSE))
    }
  } else {
    if (runif(1) < probs$interception_pass_prob ||
        runif(1) < probs$fumble_pass_prob) {
      return(list(yards_gained = 0, turnover = TRUE))
    }
    if (runif(1) < probs$incomplete_pass_prob) {
      return(list(yards_gained = 0, turnover = FALSE))
    }
    model <- pass_completion_mixtures$mixture_model[pass_completion_mixtures$yard_group == yard_group][[1]]
    component <- sample(seq_along(model$weights), 1, prob = model$weights)
    yards <- rnorm(1, model$means[component], sqrt(model$variances[component]))
    yards <- min(yards, yardline)
    return(list(yards_gained = round(yards), turnover = FALSE))
  }
}

## Necessary Packages
library(dplyr)
library(mgcv)

## Getting field goal data from set
pbp_fg <- pbp %>%
  filter(play_type == "field_goal") %>%
  mutate(
    kick_distance = yardline_100 + 17,  # Actual kick distance
    made = ifelse(field_goal_result == "made", 1, 0)  # Binary success variable
  )

## Using logistic model (could be changed)
fg_model <- gam(made ~ s(kick_distance), data = pbp_fg, family = binomial)

## Generating success probability
fg_success_prob <- function(kick_distance) {
  predict(fg_model, newdata = data.frame(kick_distance = kick_distance), type = "response")
}

## FG Function
attempt_field_goal <- function(yardline, fg_value = 3, fg_prob_func = fg_success_prob) {
  kick_distance <- yardline + 17
  prob_make <- fg_prob_func(kick_distance)
  kick_result <- rbinom(1, 1, prob_make)
  
  if (kick_result == 1) {
    return(list(
      points = fg_value,
      outcome = "FG Made",
      field_position = 75  # kickoff touchback start
    ))
  } else {
    miss_spot_from_own_endzone <- 100 - (yardline + 7)
    return(list(
      points = 0,
      outcome = "FG Missed",
      field_position = miss_spot_from_own_endzone
    ))
  }
}



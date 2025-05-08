## NECESSARY PACKAGES
library(dplyr)
library(mgcv)

## TRIMMING FG DATA
pbp_fg <- pbp %>%
  filter(play_type == "field_goal") %>%
  mutate(
    kick_distance = yardline_100 + 17,  # Actual kick distance
    made = ifelse(field_goal_result == "made", 1, 0)  # Binary success variable
  )

## LOG REGRESSION
fg_model <- gam(made ~ s(kick_distance), data = pbp_fg, family = binomial)

## SUCCESS PROB
fg_success_prob <- function(kick_distance) {
  predict(fg_model, newdata = data.frame(kick_distance = kick_distance), type = "response")
}

## FG FUNCTION
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

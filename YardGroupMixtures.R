pbp <- readRDS("pbp2014-2024 (1).rds")
library(mclust)

rush_mixtures <- pbp %>%
  filter(play_type == "run", fumble_lost == 0) %>%
  group_by(yard_group) %>%
  summarise(
    mixture_model = list(fit_mixture(yards_gained))
  )

pass_completion_mixtures <- pbp %>%
  filter(play_type == "pass", interception == 0, fumble_lost == 0, complete_pass == 1) %>%
  group_by(yard_group) %>%
  summarise(
    mixture_model = list(fit_mixture(yards_gained))
  )

pbp <- readRDS("FINALPROJECTDATA.rds")
library(mclust)
if (!"yard_group" %in% names(pbp)) {
  pbp <- pbp %>%
    filter(!is.na(yardline_100)) %>%
    mutate(
      yardline_100 = 100 - yardline_100,
      yard_group = cut(yardline_100, breaks = seq(0, 100, by = 10), right = FALSE)
    )
}

# 2. Define the Gaussian mixture fitting function
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

# 3. Fit mixture models for rush and pass completions
rush_mixtures <- pbp %>%
  filter(play_type == "run", fumble_lost == 0) %>%
  group_by(yard_group) %>%
  summarise(
    mixture_model = list(fit_mixture(yards_gained)),
    .groups = "drop"
  )

pass_mixtures <- pbp %>%
  filter(play_type == "pass", interception == 0, fumble_lost == 0, complete_pass == 1) %>%
  group_by(yard_group) %>%
  summarise(
    mixture_model = list(fit_mixture(yards_gained)),
    .groups = "drop"
  )

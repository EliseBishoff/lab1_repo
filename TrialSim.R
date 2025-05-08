## LOADING PACKAGES
library(dplyr)
library(mgcv)
library(mclust)
library(nnet)

## REPORDUCIBILITY
set.seed(111)

## LOADING DATA
pbp <- readRDS("FINALPROJECTDATA.rds") %>%
  filter(!is.na(yardline_100)) %>%
  mutate(
    yardline_100 = 100 - yardline_100,
    yard_group   = cut(yardline_100, breaks = seq(0, 100, by = 10), right = FALSE)
  )

## BUILDING/LOADING MOD INPUTS
if (file.exists("model_inputs.RData")) {
  load("model_inputs.RData")
  message("Loaded saved model inputs.")
} else {
  message("Generating model inputs from pbp data...")
  
  probabilities <- pbp %>%
    group_by(yard_group) %>%
    summarise(
      total_plays = n(),
      total_rush  = sum(rush_attempt == 1, na.rm = TRUE),
      total_pass  = sum(pass_attempt == 1, na.rm = TRUE),
      fumble_rush = sum(fumble == 1 & rush_attempt == 1, na.rm = TRUE),
      fumble_rush_prob = ifelse(total_rush > 0, fumble_rush / total_rush, 0),
      interception_pass = sum(interception == 1 & pass_attempt == 1, na.rm = TRUE),
      interception_pass_prob = ifelse(total_pass > 0, interception_pass / total_pass, 0),
      fumble_pass = sum(fumble == 1 & pass_attempt == 1, na.rm = TRUE),
      fumble_pass_prob = ifelse(total_pass > 0, fumble_pass / total_pass, 0),
      incomplete_pass = sum(pass_attempt == 1 & yards_gained == 0 & interception == 0 & fumble == 0, na.rm = TRUE),
      incomplete_pass_prob = ifelse(total_pass > 0, incomplete_pass / total_pass, 0),
      avg_yards_rush = mean(yards_gained[rush_attempt == 1 & fumble == 0], na.rm = TRUE),
      avg_yards_pass_completion = mean(yards_gained[pass_attempt == 1 & interception == 0 & fumble == 0 & yards_gained > 0], na.rm = TRUE)
    )
  
  rush_mixtures <- pbp %>%
    filter(rush_attempt == 1, fumble == 0) %>%
    group_by(yard_group) %>%
    summarise(mixture_model = list(fit_mixture(yards_gained)))
  
  pass_mixtures <- pbp %>%
    filter(pass_attempt == 1, interception == 0, fumble == 0, yards_gained > 0) %>%
    group_by(yard_group) %>%
    summarise(mixture_model = list(fit_mixture(yards_gained)))
  
  save(probabilities, rush_mixtures, pass_mixtures, file = "model_inputs.RData")
  message("Model inputs saved to model_inputs.RData")
}

## SOURCING ALL COMPONENT FILES
source("RuleConfig.R")
source("FGFunction.R")
source("ProbTable4.R")
source("YardGroupMixtures.R")
source("SimulateYardsGained.R")
source("DrivE.R")
source("Epoch.R")

## EP CURVE
library(dplyr)
yard_bins <- seq(5, 95, by = 10)
ep_curve <- bind_rows(lapply(yard_bins, function(fp) {
  epoch(
    start_fp      = fp,
    rule_config   = rule_config,
    probabilities = probabilities,
    rush_mixtures = rush_mixtures,
    pass_mixtures = pass_mixtures,
    prob_table    = prob_table,
    n             = 2000
  )
}))

## OUTPUT
print(ep_curve)
cat("\nMean EP across all bins:", mean(ep_curve$EP), "\n")




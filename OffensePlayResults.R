pbp <- readRDS("pbp2014-2024 (1).rds")
probabilities <- pbp %>%
  filter(play_type %in% c("run", "pass")) %>%
  group_by(yard_group) %>%
  summarise(
    total_plays = n(),
    total_rush = sum(play_type == "run", na.rm = TRUE),
    total_pass = sum(play_type == "pass", na.rm = TRUE),
    
    fumble_rush = sum(fumble_lost == 1 & play_type == "run", na.rm = TRUE),
    fumble_rush_prob = ifelse(total_rush > 0, fumble_rush / total_rush, 0),
    
    interception_pass = sum(interception == 1 & play_type == "pass", na.rm = TRUE),
    interception_pass_prob = ifelse(total_pass > 0, interception_pass / total_pass, 0),
    
    fumble_pass = sum(fumble_lost == 1 & play_type == "pass", na.rm = TRUE),
    fumble_pass_prob = ifelse(total_pass > 0, fumble_pass / total_pass, 0),
    
    incomplete_pass = sum(play_type == "pass" & complete_pass == 0 &
                            interception == 0 & fumble_lost == 0, na.rm = TRUE),
    incomplete_pass_prob = ifelse(total_pass > 0, incomplete_pass / total_pass, 0),
    
    completion_pass_prob = ifelse(total_pass > 0,
                                   1 - interception_pass_prob - fumble_pass_prob - incomplete_pass_prob, 0),
    
    avg_yards_rush = mean(yards_gained[play_type == "run" & fumble_lost == 0], na.rm = TRUE),
    avg_yards_pass_completion = mean(yards_gained[play_type == "pass" & interception == 0 &
                                                   fumble_lost == 0 & complete_pass == 1], na.rm = TRUE)
  )

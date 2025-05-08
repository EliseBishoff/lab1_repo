pbp <- readRDS("pbp2014-2024 (1).rds")
pbp <- pbp %>%
  filter(!is.na(yardline_100)) %>%
  mutate(
    yardline = 100 - yardline_100,  # Convert to field position from own endzone
    yard_group = cut(yardline, breaks = seq(0, 100, by = 10), right = FALSE)
  )

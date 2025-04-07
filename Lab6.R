
data<-read.csv("nhl_pbp20162017.csv")
data<-na.omit(data)
data$point_diff<-data$Home_Score-data$Away_Players
data$home_shot<-data$Ev_Team==data$Home_Team



data$time_group <- cut(data$Seconds_Elapsed,
                       breaks = seq(0, 1200, by = 300),  
                       include.lowest = TRUE,
                       labels = c("Group1", "Group2", "Group3", "Group4"))
data$Event_numeric <- ifelse(data$Event == "SHOT", 1, 0)

model<-glm(Event_numeric~Period+time_group+home_shot+point_diff,family=poisson(),data=data)
summary(model)


data$region_x <- cut(data$xC, 
                     breaks = c(-100, -33.33, 33.33, 100), 
                     include.lowest = TRUE, 
                     labels = c("Left", "Center", "Right"))


data$region_y <- cut(data$yC, 
                     breaks = c(-45, 0, 45), 
                     include.lowest = TRUE, 
                     labels = c("Bottom", "Top"))


data$region <- interaction(data$region_x, data$region_y)

model1<-glm(Event_numeric~home_shot+point_diff+region,family =poisson(),data=data)

data <- data[data$Event %in% c("SHOT", "MISS"), ]

data$shot_success <- ifelse(data$Event == "SHOT", 1, 0)

model_success <- glm(shot_success ~ region + home_shot + point_diff,
                     family = binomial(), data = data)
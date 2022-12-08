library(tidyverse)
library(lubridate)
source("joining.R")
week_num = 1

run = function() {
  weeks = load_weeks()
  plays = load_plays()
  weeks_summarized = create_weekly_play_summary(weeks)
  weeks_summarized = join_summarized_weeks_to_position(weeks_summarized)
  weeks_summarized = pivot_by_role(weeks_summarized)
  weeks_summarized = join_time_to_throw(weeks_summarized)
  week_data_play_direction = week_data %>% 
    select(gameId, playId, playDirection) %>%
    distinct(gameId, playId, playDirection, .keep_all = TRUE)
  weeks_summarized_with_play_direction = weeks_summarized %>% left_join(
    week_data_play_direction, by = c('gameId', 'playId')
  )
  weeks_summarized_with_play_direction = weeks_summarized_with_play_direction %>%
    mutate(playDirection = as.factor(playDirection))
  return(weeks_summarized)
}

read_plays = function() {
  plays = read_csv("plays.csv")
  return(plays)
}

read_week = function(week_num) {
  week = read_csv(paste0("week", week_num, ".csv"))
  return(week)
}


load_plays = function() {
  plays = read_plays()
  plays_conditioned = condition_plays(plays)
  return(plays_conditioned)
}

load_weeks = function() {
  weeks = read_week(1)
  weeks = condition_week_data(weeks)
  for (i in 2:8) {
    new_week = read_week(i)
    new_week_conditioned = condition_week_data(new_week)
    weeks = rbind(weeks, new_week_conditioned)
  }
  create_time_to_play_dictionary(weeks)
  return(weeks)
}

create_time_to_play_dictionary = function(weeks) {
  time_of_throw = weeks %>%
    mutate(is_sack = str_detect(event, 'sack')) %>%
    filter(is_throw == TRUE | is_sack == TRUE) %>%
    group_by(gameId, playId) %>%
    mutate(action_time = min(time_since_play_start, na.rm = TRUE)) %>%
    select(gameId, playId, action_time, event) %>%
    distinct(gameId, playId, .keep_all = TRUE) %>%
    filter(action_time > 0, action_time < 60 * 5) # point of second part is to remove Inf value action_time plays (i.e. plays where likely snap time is unknown)
  time_of_throw %>% write_csv("time_of_throw_or_sack.csv")
  return(time_of_throw)
}

condition_plays = function(plays){
  plays_passing = plays %>% 
    filter(passResult != 'R') %>%
    filter(passResult != ' ')
  plays_passing = plays_passing %>%
    filter(!(foulName1 %in% c('Offensive Holding', 'Defensive Offside', 'Illegal Formation',
                              'Illegal Formation', 'Illegal Block Above the Waist', 'Face Mask (15 Yards)',
                              'Disqualification', 'Illegal Blindside Block', 'Chop Block',
                              'Low Block', 'Illegal Shift', 'Tripping', 'Illegal Use of Hands',
                              'Illegal Substitution', 'Illegal Motion', 'Clipping')))
  return(plays_passing)
}

condition_week_data = function(week) {
  
  week_data = week %>%
    mutate(is_throw = ifelse(str_detect(event, "pass") & !str_detect(event, 'outcome') & !str_detect(event, 'interrupted'), TRUE, FALSE)) %>%
    mutate(time_of_throw = ifelse(str_detect(event, "pass") & !str_detect(event, 'outcome'), time, NA)) %>%
    arrange(gameId, playId, nflId, time, event, is_throw) %>%
    group_by(gameId, playId, nflId, time) %>%
    mutate(across(c(x, y, s, a, o), ~mean(., na.rm = TRUE))) %>%
    arrange(gameId, playId, nflId, time, event) %>%
    distinct(gameId, playId, nflId, time, .keep_all = TRUE) %>%
    ungroup()
    
  
  week_data = week_data %>%
    ungroup() %>%
    mutate(play_id_prev = lag(playId),
           game_id_prev = lag(gameId),
           nfl_id_prev = lag(nflId)) %>%
    mutate(same_play_as_prev = ifelse(play_id_prev == playId & gameId == game_id_prev & nflId == nfl_id_prev, TRUE, FALSE)) %>%
    mutate(play_time_index = 0) %>%
    mutate(play_time_index = ifelse(same_play_as_prev, lag(play_time_index) + 0.1, 0))
  
  play_starts = week_data %>%
    mutate(is_snap = ifelse(str_detect(event, 'snap'), TRUE, FALSE)) %>%
    filter(is_snap == TRUE) %>%
    group_by(gameId, playId, nflId) %>%
    mutate(time = min(time, na.rm = TRUE)) %>%
    ungroup() %>%
    select(gameId, playId, nflId, event, time) %>%
    distinct(gameId, playId, nflId, time, .keep_all = TRUE)
  
  week_data_with_play_start = week_data %>%
    left_join(play_starts, by = c('gameId', 'playId', 'nflId'), suffix = c('', '_play_start')) %>%
    mutate(time_since_play_start = time - time_play_start)
  return(week_data_with_play_start)
}

create_weekly_play_summary = function(weeks) {
  weeks_summarized = weeks %>%
    mutate(time_since_play_start_rounded = round(time_since_play_start, 3)) %>%
    filter(!is.na(time_since_play_start)) %>%
    filter(time_since_play_start >= 0) %>%
    filter(time_since_play_start_rounded <= 1) %>%
    arrange(time_since_play_start_rounded)
  weeks_summarized = weeks_summarized %>%
    pivot_wider(id_cols = c(gameId, playId, nflId, playDirection),
                names_from = time_since_play_start_rounded,
                values_from = c(x, y, s, a, dis, o, dir))
  write_csv(weeks_summarized, "plays_by_player_with_motion.csv")
}

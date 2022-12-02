library(tidyverse)

plays = read_csv("plays.csv")
plays_passing = plays %>% 
  filter(passResult != 'R') %>%
  filter(passResult != ' ')

plays_passing = plays_passing %>%
  filter(!(foulName1 %in% c('Offensive Holding', 'Defensive Offside', 'Illegal Formation',
                              'Illegal Formation', 'Illegal Block Above the Waist', 'Face Mask (15 Yards)',
                              'Disqualification', 'Illegal Blindside Block', 'Chop Block',
                              'Low Block', 'Illegal Shift', 'Tripping', 'Illegal Use of Hands',
                              'Illegal Substitution', 'Illegal Motion', 'Clipping')))

week1 = read_csv("week1.csv")

week1_subset = week1 %>% 
  filter(event %in% c('ball_snap', 'autoevent_ballsnap', 'autoevent_passforward', 'pass_forward', 'qb_sack', 'qb_strip_sack'))


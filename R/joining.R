
join_summarized_weeks_to_position = function(weeks_summarized) {
  pff = read_pff_role()
  weeks_summarized_with_role = weeks_summarized %>%
    left_join(pff, by = c('gameId', 'playId', 'nflId')) %>%
    filter(pff_positionLinedUp %in% c('LT', 'LG', 'C', 'RG', 'RT'))
  return(weeks_summarized_with_role)
}

read_pff_role = function() {
  pff = read_csv("pffScoutingData.csv")
  return(pff)
}

pivot_by_role = function(weeks_summarized_with_role) {
  weeks_pivoted_by_role = weeks_summarized_with_role %>%
    pivot_wider(id_cols = c(gameId, playId),
                names_from = pff_positionLinedUp,
                values_from = c(starts_with('x'), starts_with('y'), starts_with('s'), starts_with('a'), starts_with('dis'), starts_with('o'), starts_with('dir')))
  weeks_pivoted_by_role %>% write_csv("role_encoded_lineman_movements_by_play.csv")
  return(weeks_pivoted_by_role)
}

join_time_to_throw = function(weeks_summarized) {
  time_of_throw = read_csv("time_of_throw_or_sack.csv")
  weeks_summarized_with_time_to_throw = weeks_summarized %>% inner_join(time_of_throw, by = c('gameId', 'playId')) # Inner join b/c can't train without time of throw
  return(weeks_summarized_with_time_to_throw)
}



dat_00 <- get_play_by_play()


dat_01 <- dat_00 %>% clean_play_by_play()

dat_02 <- dat_01 %>% clean_posessions()


toronto_games_2019 <- get_season_games() %>%

  clean_season_games() %>%

  mutate(
    play_by_play = game_id %>% map(get_play_by_play) %>% map(clean_play_by_play),
    posessions = play_by_play %>% map(clean_posessions)
  )



game_ids <- toronto_games_2019$game_id


pbp <- game_ids %>% map(get_play_by_play)



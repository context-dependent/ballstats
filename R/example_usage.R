library(tidyverse)

dat_00 <- get_play_by_play()


dat_01 <- dat_00 %>% clean_play_by_play()

dat_02 <- dat_01 %>% clean_posessions()


games_2017 <- get_season_games(Season = 2017) %>%

  filter(team_abbreviation == "TOR")

games_2017 %>%

  mutate(batch = row_number()) %>%
  group_split(team_abbreviation, batch) %>%
  map(
    function(x) {
      message(glue::glue("\n{x$game_id}", .sep = ", "))
      p <- x$game_id %>% map(get_play_by_play)
      p2 <- p %>% map(clean_play_by_play)
      p3 <- p2 %>% map2(x$game_id, clean_posessions)
      rm(list = ls())
      res <- NULL
    }
  )

whole_ting <- function(game_id) {
  message(glue::glue("\n{x$game_id}", .sep = ", "))
  p <- game_id %>% get_play_by_play
  p2 <- p %>% clean_play_by_play
  p3 <- p2 %>% clean_posessions(game_id)
  rm(list = ls())
  res <- NULL

}


ids <- games_2017$game_id

pbp <- ids %>% map(get_play_by_play)

pbp_clean <- pbp %>% map(clean_play_by_play)

games_2017_with_plays <- games_2017 %>%
  mutate(
    play_by_play = game_id %>% map(get_play_by_play),
    play_by_play_clean = play_by_play %>% map(clean_play_by_play),
    posessions = play_by_play_clean %>% map2( game_id, clean_posessions)
  )

play_by_play_clean <- games_2017_with_plays %>%
  select(game_id, play_by_play_clean)

get_play_by_play_safe <- purrr::safely(get_play_by_play)
clean_play_by_play_safe <- purrr::safely(clean_play_by_play)
clean_posessions_safe <- purrr::safely(clean_posessions)

left_join <- function(...) {

  suppressMessages(left_join(...))

}

game_ids <- toronto_games_2019$game_id

pbp <- game_ids %>% map(get_play_by_play)

batchify_games <- function(game_data) {

  res <- game_data %>%

    mutate(
      batch = row_number() %% 20
    ) %>%

    group_split(batch)


  res
}

download_game_data_batch <- function(games) {

  res <- games %>%

    mutate(
      play_by_play = game_id %>% map(get_play_by_play) %>% map(clean_play_by_play),
      posessions = play_by_play %>% map(clean_posessions)
    )

  res %>%

    group_split(game_id) %>%
    map(
      ~ write_rds(.x, glue::glue("{.x$game_id}.rds"))
    )

}

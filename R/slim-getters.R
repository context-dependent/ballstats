clean_rapm_matrix <- function(posession_players, posession_team) {


}

get_posessions <- function(GameID = "0021800001") {

  play_by_play_raw <- get_play_by_play(GameID = GameID)
  players_by_quarter <- 1

}

clean_season_games <- function(season_games) {

  season_games %>%

    janitor::clean_names()

}




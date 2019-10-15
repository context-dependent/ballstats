
get_play_by_play <- function(GameID = "0021800001", StartPeriod=0, EndPeriod = 14, browse = FALSE) {

  if(browse) browser()

  raw <- make_nba_stats_request(endpoint = "playbyplayv2", GameID = GameID, StartPeriod = StartPeriod, EndPeriod = EndPeriod, browse = browse)

  data <- raw$resultSet$rowSet[[1]]

  column_names <- raw$resultSet$headers[[1]]

  colnames(data) <- column_names

  res <- tibble::as_tibble(data)

  res

}


get_box_score <- function(GameID, StartPeriod, EndPeriod, StartRange = "", EndRange = "", browse = FALSE) {


  if(browse) browser()

  raw <- make_nba_stats_request("boxscoreadvancedv2", GameID = GameID, StartPeriod = StartPeriod, EndPeriod = EndPeriod, StartRange = StartRange, EndRange = EndRange, RangeType = 2, debug_url = FALSE)

  data <- raw$resultSets$rowSet[[1]]

  column_names <- raw$resultSets$headers[[1]]

  colnames(data) <- column_names

  res <- tibble::as_tibble(data) %>% janitor::clean_names()

  res

}


get_players_by_period <- function(periods, GameID = "0021800001", StartPeriod=0, EndPeriod = 14, browse = FALSE) {

  if(browse) browser()


  period_table <- tibble::tibble(
    period = periods,
    quarter = period < 5,
    time_in_period = 3000 + quarter * 4200,
    start_range = cumsum(time_in_period) - (3000 + quarter * 4195),
    end_range = cumsum(time_in_period) - 5
  ) %>%

    select(-quarter, -time_in_period)


  player_dictionary <- get_box_score(
    GameID = GameID,
    StartPeriod = 1,
    EndPeriod = 14,
    StartRange = min(period_table$start_range),
    EndRange = max(period_table$end_range)
  )


  players_by_period <- period_table %>%
    dplyr::mutate(
      all_players_in_period = purrr::map2(
        start_range,
        end_range,
        ~ get_box_score(
          GameID = GameID,
          StartPeriod = StartPeriod,
          EndPeriod = EndPeriod,
          StartRange = .x,
          EndRange = .y
        ) %>%

          select(
            player_id,
            player_team = team_abbreviation
          )
      )
    ) %>%
    unnest() %>%
    select(-start_range, -end_range) %>%
    group_by(period) %>%
    nest(.key = "all_players") %>%
    mutate(
      all_players = all_players %>% map(unlist) %>% map(unname)
    )

  res <- list(
    player_dictionary = player_dictionary,
    players_by_period = players_by_period
  )

  res
}


get_season_games <- function(Season = "2017-18") {

  raw <- make_nba_stats_request(
    endpoint = "leaguegamelog",
    Season = Season,
    LeagueID = "00",
    PlayerOrTeam = "T",
    Direction = "DESC",
    SeasonType = "Regular Season",
    Sorter = "DATE"

    # , browse = TRUE

    #, debug_url = TRUE
  )

  dat <- raw$resultSets$rowSet[[1]]

  column_names <- raw$resultSets$headers[[1]]

  colnames(dat) <- column_names

  res <- as_tibble(dat) %>% janitor::clean_names()

  res
}

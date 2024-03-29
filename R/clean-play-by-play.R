
#' Clean play-by-play response from NBA stats api
#'
#' @param play_by_play_response
#' @param browse
#'
#' @return
#' @export
#'
#' @examples
clean_play_by_play <- function(play_by_play_response, browse = FALSE) {


  if(browse) browser()

  game_id <- play_by_play_response$GAME_ID[1]

  if(file.exists(glue::glue("data/cache/clean/play-by-play/{game_id}.rds"))) {

    res <- read_rds(glue::glue("data/cache/clean/play-by-play/{game_id}.rds"))

    return(res)
  }

  dat <- play_by_play_response %>%

    janitor::clean_names() %>%
    rename(
      time = wctimestring,
      time_remaining = pctimestring
    ) %>%
    mutate(
      period = as.integer(period),
      event_id = str_c(game_id, str_pad(eventnum, 4, pad = "0")),
      event_type_label = recode(eventmsgtype, !!!sr$event),
      seconds_played = clean_game_time(period, time_remaining)
    ) %>%
    mutate_at(
      vars(matches("person\\dtype")),
      list(~ recode(., !!!sr$player_type_dictionary))
    ) %>%
    mutate_at(
      vars(matches("player\\d_id")),
      list(~ na_if(., 0))
    )

  # clean tables for different types of events
  tables <- list()


  tables$shots = clean_shots(dat)
  tables$turnovers = clean_turnovers(dat)
  tables$rebounds = clean_rebounds(dat)
  tables$blocks = clean_blocks(dat)
  tables$steals = clean_steals(dat)
  tables$substitutions = clean_substitutions(dat, game_id = game_id)
  tables$timeouts = clean_timeouts(dat)


  res <- tables %>%
    bind_rows() %>%
    arrange(seconds_played, event_id) %>%
    select(
      event_id, event_category,
      player_id, player_name, player_team,
      period, seconds_played, everything()
    ) %>%
    fill_players_on_court() %>%
    drop_first_ft_rebounds() %>%
    fill_home_away_teams()

  write_rds(res, glue::glue("data/cache/clean/play-by-play/{game_id}.rds"))

  res

}

clean_timeouts <- function(dat) {

  team_id_dictionary <- dat %>%

    select(
      player_team_id = player1_team_id,
      player_team = player1_team_abbreviation
    ) %>%

    group_by_all() %>%
    slice(1) %>%

    ungroup()


  res <- dat %>%

    filter(
      event_type_label == "time out"
    ) %>%

    select(
      event_id,
      seconds_played,
      event_type_label,
      player_type = person1type,
      player_team_id = player1_id
    ) %>%

    left_join(
      team_id_dictionary
    ) %>%

    mutate(event_category = "Time Out") %>%

    rename(
      player_id = player_team_id
    ) %>%

    select(
      -event_type_label
    )

  res

}

clean_substitutions <- function(dat, game_id) {

  #_if(browse) browser()

  periods_in_game <- dat$period %>% unique()

  # get players by quarter
  player_reference <- get_players_by_period(periods_in_game, GameID = game_id)

  substitutions <- dat %>%

    filter(event_type_label == "substitution") %>%
    select(
      event_id,
      event_type_label,
      period,
      seconds_played,
      player_off_name = player1_name,
      player_off_id = player1_id,
      player_on_name = player2_name,
      player_on_id = player2_id,
      player_type = person2type,
      player_team = player2_team_abbreviation,
      player_team_name = player2_team_nickname,
      homedescription,
      visitordescription
    ) %>%
    mutate(
      event_description = coalesce(homedescription, visitordescription)
    )

  period_starters <- substitutions %>%
    group_by(period) %>%
    summarize(
      event_id = list(c(event_id)),
      subs_on = list(c(player_on_id)),
      subs_off = list(c(player_off_id))
    ) %>%

    ungroup() %>%

    mutate(
      all_players = player_reference$players_by_period$all_players[1:max(row_number())]
    )

  period_starters <- period_starters %>%
    mutate(
      period_starters =pmap(period_starters %>% select(all_players, subs_on, subs_off), find_period_starters)
    )

  players_on_court <- period_starters %>%
    select(event_id, subs_on, subs_off, period_starters) %>%
    pmap_dfr(find_players_on_court, player_reference$player_dictionary)

  res <- substitutions %>%
    mutate(
      players_on_court_before = players_on_court$players_on_court_before,
      players_on_court_after = players_on_court$players_on_court_after
    ) %>%
    group_by(period, player_name = player_team_name, seconds_played, player_team, player_type) %>%
    summarize(
      event_id = first(event_id),
      player_id = first(player_on_id),
      players_on_court_before = first(players_on_court_before),
      players_on_court_after = last(players_on_court_after)
    ) %>%
    mutate(
      event_category = "Substitution"
    )


  res

}



find_period_starters <- function(all_players, subs_on, subs_off) {

  # browser()

  index <- logical(length(all_players))


  for(i in seq_along(all_players)) {

    player <- all_players[i]

    if(!(player %in% subs_on)) {
      index[i] <- TRUE
    } else if(!(player %in% subs_off)) {
      index[i] <- FALSE
    } else {
      first_sub_off <- min(which(player == subs_off))
      first_sub_on <- min(which(player == subs_on))

      if(first_sub_off < first_sub_on) {
        index[i] <- TRUE
      } else {
        index[i] <- FALSE
      }
    }
  }

  res <- all_players[index]

  res

}

find_players_on_court <- function(event_id, subs_on, subs_off, period_starters, player_dictionary) {

  player_dictionary <- player_dictionary %>%

    select(
      team_id, player_team = team_abbreviation, player_name, player_id
    )


  players_on_court <- list(
    period_starters
  )

  for(i in seq_along(event_id)) {

    x <- players_on_court[[i]]

    x[x == subs_off[i]] <- subs_on[i]

    players_on_court[[i+1]] <- x
  }

  res <- tibble::tibble(
    event_id = event_id,
    players_on_court_before = players_on_court[seq_along(event_id)],
    players_on_court_after  = lead(players_on_court, default = c())
  ) %>%
    mutate_at(
      vars(matches("players")),
      list(~ map(., ~ player_dictionary %>% filter(player_id %in% .x)))
    )

  res

}

fill_home_away_teams <- function(dat) {

  dat_home_away <- dat %>% select(player_team, player_type) %>%
    filter(!is.na(player_team)) %>%
    group_by(player_team) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(player_type)

  list_home_away <- list(
    home = dat_home_away$player_team[2],
    away = dat_home_away$player_team[1]
  )

  res <- dat %>%
    mutate(
      player_team = case_when(
        !is.na(player_team) ~ player_team,
        player_type == "Home Team" ~ list_home_away$home,
        player_type == "Away Team" ~ list_home_away$away,
        TRUE ~ NA_character_
      )
    )

  res

}

fill_players_on_court <- function(dat) {

  res <- dat %>%


    fill(players_on_court_before, .direction = "up") %>%
    fill(players_on_court_after, .direction = "down") %>%

    mutate(
      players_on_court = map2(
        players_on_court_after,
        players_on_court_before,
        ~ if(is_tibble(.x)) {
          .x
        } else {
          .y
        }
      )
    ) %>%

    select(
      -matches("players_on_court_")
    )

  res

}

clean_game_time <- function(period, time_remaining) {


  period_time_index <- cumsum(c(0, 300 + 420 * (unique(period) < 5)))
  seconds_remaining_in_period <- lubridate::ms(time_remaining) %>% lubridate::period_to_seconds()
  total_time_in_period <- 300 + 420 * (period < 5)
  seconds_advanced_in_period <- total_time_in_period - seconds_remaining_in_period
  res <- seconds_advanced_in_period + period_time_index[period]

  res

}

# Team rebounds are not coded offenseive or defensive, and on further analysis represent a mix of both
clean_rebounds <- function(dat) {

  rebounds <- dat %>%
    filter(
      str_detect(event_type_label, "rebound"),

    ) %>%
    select(
      event_id,
      event_type_label,
      period,
      seconds_played,
      player_name = player1_name,
      player_id = player1_id,
      player_type = person1type,
      player_team = player1_team_abbreviation,
      homedescription,
      visitordescription
    ) %>%
    mutate(
      event_description = coalesce(homedescription, visitordescription),
      player_name = coalesce(player_name, player_type),
      offensive_total = as.numeric(str_extract(event_description, "(?<=Off:)\\d+")),
      defensive_total = as.numeric(str_extract(event_description, "(?<=Def:)\\d+")),

    ) %>%

    group_by(player_id) %>%

    mutate(



      # this seems to be a reliable indicator
      offensive_rebound = offensive_total - lag(offensive_total, default = 0),
      defensive_rebound = defensive_total - lag(defensive_total, default = 0),

      event_detail = case_when(
        offensive_rebound > 1 | defensive_rebound > 1 ~ "Error: Over-counted",
        offensive_rebound + defensive_rebound == 2 ~ "Error: Both Offensive and Defensive",
        offensive_rebound == 1 ~ "Offensive",
        defensive_rebound == 1 ~ "Defensive",
        TRUE ~ "Unknown"
      )

    ) %>%
    select(-matches("description|type_label|total|rebound")) %>%
    mutate(event_category = "Rebound Opportunity") %>%

    ungroup()


  res <- rebounds

  # to clean team rebounds
  # - remove rebounds between free throws
  #   + can do this when combining data sets, remove rebounds with same timestamp as fts
  # - do team rebounds later get assigned to players? No, thankfully
  # - I have to determine the team of the last shot before I can determine the direction of team rebounds

  res
}

drop_first_ft_rebound <- function(plays_in_second, ...) {

  any_first_ft_rebounds <-

    any(
      plays_in_second$shot_made %in% TRUE &
        plays_in_second$event_category == "Free Throws"
    )

  res <- plays_in_second

  if(any_first_ft_rebounds) {
    res <- plays_in_second %>%
      filter(event_category != "Rebound Opportunity")
  }

  res

}

drop_first_ft_rebounds <- function(play_by_play_data) {

  res <- play_by_play_data %>%
    group_by(seconds_played) %>%
    group_map(drop_first_ft_rebound) %>%
    bind_rows() %>%
    ungroup()

  res

}

clean_blocks <- function(dat) {

  blocks <- dat %>%
    filter(str_detect(event_type_label, "field goal")) %>%
    select(
      event_id,
      event_type_label,
      period,
      seconds_played,
      player_name = player3_name,
      player_id = player3_id,
      player_type = person3type,
      player_team = player3_team_abbreviation,
      homedescription,
      visitordescription
    ) %>%
    mutate_at(
      # I'm thinking it makes sense to pull out the blocks for now, and then apply them after, on their own
      vars(homedescription, visitordescription),
      list(~ ifelse(!str_detect(., "BLOCK"), NA_character_, .))
    ) %>%
    mutate(
      event_description = coalesce(homedescription, visitordescription)
    ) %>%
    filter(!is.na(event_description)) %>%
    select(-matches("description|type_label")) %>%
    mutate(event_category = "Block")

  res <- blocks

  res

}

clean_steals <- function(dat) {

  steals <- dat %>%
    filter(event_type_label == "turnover") %>%
    select(
      event_id,
      event_type_label,
      period,
      seconds_played,
      player_name = player2_name,
      player_id = player2_id,
      player_type = person2type,
      player_team = player2_team_abbreviation,
      homedescription,
      visitordescription
    ) %>%
    mutate_at(
      # I'm thinking it makes sense to pull out the blocks for now, and then apply them after, on their own
      vars(homedescription, visitordescription),
      list(~ ifelse(!str_detect(., "STEAL"), NA_character_, .))
    ) %>%
    mutate(
      event_description = coalesce(homedescription, visitordescription)
    ) %>%
    filter(!is.na(event_description)) %>%
    select(-matches("description|type_label")) %>%
    mutate(event_category = "Steal")

  res <- steals

  res
}

clean_turnovers <- function(dat) {

  turnovers <- dat %>%
    filter(event_type_label == "turnover") %>%
    select(
      event_id,
      event_type_label,
      period,
      seconds_played,
      player_name = player1_name,
      player_id = player1_id,
      player_type = person1type,
      player_team = player1_team_abbreviation,
      homedescription,
      visitordescription
    ) %>%
    mutate_at(
      # I'm thinking it makes sense to pull out the blocks for now, and then apply them after, on their own
      vars(homedescription, visitordescription),
      list(~ ifelse(str_detect(., "STEAL"), NA_character_, .))
    ) %>%
    mutate(
      event_description = coalesce(homedescription, visitordescription),
      event_detail = event_description %>% str_remove("^[^ ]+ ") %>% str_remove(" Turnover.+$")
    ) %>%
    select(-matches("description|type_label")) %>%
    mutate(event_category = "Turnover")

  res <- turnovers

  res

}

clean_shots <- function(dat) {

  # the relationship between the home/visitor description and the player1, player2 info is janky
  # how do I know a missed shot is from 3 or not?
  # - it says so in the description
  field_goals_and_free_throws <- dat %>%
    filter(
      event_type_label %in% c(
        "field goal made",
        "field goal missed",
        "free throw"
      )
    ) %>%
    select(
      event_id,
      event_type_label,
      period,
      seconds_played,
      player_name = player1_name,
      player_id = player1_id,
      player_team = player1_team_abbreviation,
      player_type = person1type,
      homedescription,
      visitordescription
    ) %>%
    mutate_at(
      # I'm thinking it makes sense to pull out the blocks for now, and then apply them after, on their own
      vars(homedescription, visitordescription),
      list(~ ifelse(str_detect(., "BLOCK"), NA_character_, .))
    ) %>%
    mutate(
      event_description = coalesce(homedescription, visitordescription),
      shot_made = case_when(
        event_type_label == "free throw" & !str_detect(event_description, "MISS") ~ TRUE,
        event_type_label %>% str_detect("made") ~ TRUE,
        TRUE ~ FALSE
      ),
      potential_value = case_when(
        event_type_label == "free throw" ~ 1,
        str_detect(event_description, "3PT") ~ 3,
        TRUE ~ 2
      ),
      actual_value = potential_value * shot_made,
      # feet_to_hoop is sometimes missing, with defrault game id, it's only for 3s.
      # If this remains the case, I can pretty easily deal with it by averaging the distance
      # of all threes, or assuming they're from the corner
      feet_to_hoop = as.numeric(str_extract(event_description, "\\d+(?=\')")),
      event_detail = case_when(
        event_type_label == "free throw" ~ "Free Throw",
        TRUE ~ str_extract(event_description, "(?<=\' ).+$") %>% str_remove(" \\(.+")
      )
    ) %>%
    select(-homedescription, -visitordescription)

  # strip made and missed labels from event_type_label
  # convert shot_made to numeric for compatibility with free throws
  field_goals <- field_goals_and_free_throws %>%
    filter(event_type_label %>% str_detect("field goal")) %>%
    mutate(
      event_category = "Field Goal Attempt",
      feet_to_hoop = replace_na(feet_to_hoop, 24)
    )

  free_throws <- field_goals_and_free_throws %>%
    filter(event_type_label == "free throw")

  free_throws_01 <- free_throws %>%
    group_by(seconds_played) %>%
    summarize(
      event_id = first(event_id),
      event_description = first(event_description),
      period = first(period),
      player_id = first(player_id),
      player_name = first(player_name),
      player_type = first(player_type),
      player_team = first(player_team),
      # For posession purposes, we only care about whether the last shot was made.
      # This makes calculating free throw percentages a bit awk, but i think I can do it with actual / potential values
      shot_made = last(shot_made),
      potential_value = sum(potential_value),
      actual_value = sum(actual_value)
    ) %>%
    mutate(
      event_category = "Free Throws",
      event_detail = "Free Throws"
    )

  res <- field_goals %>%
    bind_rows(free_throws_01) %>%
    arrange(seconds_played) %>%
    select(-event_description, -event_type_label)

  res

}

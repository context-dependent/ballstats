
#' Extract posession information from cleaned play-by-play data
#'
#' @param play_by_play_data
#' @param game_id
#'
#' @return
#' @export
#'
#' @examples
clean_posessions <- function(play_by_play_data, game_id) {


  # if(file.exists(glue::glue("data/cache/clean/posessions/{game_id}.rds"))) {
  #
  #   return(read_rds(glue::glue("data/cache/clean/posessions/{game_id}.rds")))
  # }

  # so, we can filter out steals and blocks, or
  # consider collapsing them into turnovers and missed shots

  dat <- play_by_play_data %>%

    filter(
      !(event_category %in% c("Steal", "Block", "Substitution"))
    ) %>%
    # posessions change on:
    # - made shots
    # - made last free throws
    # - defensive rebounds
    # - turnovers

    # posessions end on
    # - the above plus
    # - offensive rebounds
    mutate(
      posession_end =
        shot_made %in% TRUE |
        event_category %in% c("Turnover", "Rebound Opportunity"),
      posession_change =
        posession_end &
        !(event_detail %in% "Offensive"),
      posession_number = lag(cumsum(posession_end), default = 0) + 1
    ) %>%

    group_by(posession_number) %>%

    mutate(
      posession_team = first(player_team),
      event_category = case_when(
        event_category == "Rebound Opportunity" ~ str_c(event_category, event_detail, sep = " "),
        TRUE ~ event_category
      )
    ) %>%

    group_by(
      posession_team,
      posession_number
    ) %>%

    nest()

  dat <- dat %>%

    ungroup() %>%

    mutate(
      data = data %>% map(drop_events_before_time_out),
      shot = data %>% map(~ .x %>% filter(event_category %in% c("Field Goal Attempt", "Free Throws", "Turnover")) %>% summarize_all(last)),
      posession_end_event = data %>% map_chr(~ .x %>% pull(event_category) %>% last()),
      posession_start_event = lag(posession_end_event, default = "Tip-off"),
      posession_time_out = data %>% map_lgl(~ "Time Out" %in% .x$event_category),
      posession_start_event = ifelse(posession_time_out, "Time Out", posession_start_event)
    ) %>%
    rename(
      plays = data
    ) %>%
    unnest(shot, keep_empty = TRUE) %>%
    mutate(
      game_id = str_sub(event_id, 1, -5),
      posession_id = str_c(game_id, str_pad(posession_number, 4, "left"))
    ) %>%
    select(
      - event_category,
      - event_id
    ) %>%
    select(
      game_id, posession_id,
      posession_team,
      posession_start_event,
      posession_end_event,
      everything()
    )

  dat <- dat %>%
    mutate(
      players_on_court = players_on_court %>% map2(lead(players_on_court), ~ if(!is_tibble(.x)) {.y} else {.x}),
      players_on_court = players_on_court %>% map2(posession_team, ~ if(!is_tibble(.x)) {NULL} else {mutate(.x, on_court_value = ifelse(player_team == .y, 1, -1))})
    )

  res <- dat

  write_rds(res, glue::glue("data/cache/clean/posessions/{game_id}.rds"))

  res


}

drop_events_before_time_out <- function(posession) {

  if(!("Time Out" %in% posession$event_category)) {

    res <- posession

  } else if(first(posession$event_category) == "Time Out") {

    res <- posession

  } else {

    first_time_out_index <- which(posession$event_category == "Time Out")[1]

    res <- posession %>%

      filter(row_number() >= first_time_out_index)

  }

  res

}

find_posession_number <- function(dat) {

  res <- dat %>%
    mutate(
      posession_end =
        shot_made %in% TRUE |
        event_category %in% c("Turnover", "Rebound Opportunity"),
      posession_change =
        posession_end &
        !(event_detail %in% "Offensive"),
      posession_number = lag(cumsum(posession_end), default = 0) + 1
    )

}

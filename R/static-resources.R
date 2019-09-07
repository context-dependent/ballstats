sr <- list(

  event_message_type_dictionary = list(
    "1"  = "field goal made",
    "2"  = "field goal missed", # includes blocks (player3 blocks)
    "3"  = "free throw", # includes makes and misses
    "4"  = "rebound",
    "5"  = "turnover", # steals are listed separately
    "6"  = "foul", # offensive, personal, and shooting, includes player and team counts, as well
    "7"  = "lane violation", # not sure if this includes other violations
    "8"  = "substitution", # player2 subs FOR player1
    "9"  = "time out",
    "10" = "jump ball", # player1 is at home, player3 ends up with ball
    "11" = "", # all missing from game sample
    "12" = "quarter start",
    "13" = "",
    "14" = "quarter end",
    "18" = "game end"
  ),


  player_type_dictionary = list(
    "0" = NA_character_,
    "1" = NA_character_,
    "2" = "Home Team",
    "3" = "Away Team",
    "4" = "Home Player",
    "5" = "Away Player"
  )

)


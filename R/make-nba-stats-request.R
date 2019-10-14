
cookie <- ""

make_nba_stats_request <- function(endpoint = "playbyplayv2", ..., debug_url = FALSE, browse = FALSE) {

  params <- list(...)

  if(browse) browser()

  query_string <- paste(names(params), unlist(params), sep = "=", collapse = "&")

  url <-

    glue::glue(
      "https://stats.nba.com/stats/{endpoint}?{query_string}"
    ) %>%

    URLencode()

  meta <- curl::new_handle() %>%
    curl::handle_setheaders(
      'Host'= 'stats.nba.com',
      'User-Agent'= 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.132 Safari/537.36',
      'Accept'= 'application/json, text/plain, */*',
      'Accept-Language'= 'en-US,en;q=0.5',
      'Accept-Encoding'= 'gzip, deflate, br',
      'Connection'= 'keep-alive',
      'cookie' = cookie
    )

  if(debug_url) return(url)

  request_time <- Sys.time()

  if(!dir.exists(glue::glue("data/cache/{endpoint}"))) {
    dir.create(glue::glue("data/cache/{endpoint}"))
  }

  if(!file.exists(glue::glue("data/cache/{endpoint}/{query_string}"))) {

    req <- curl::curl_fetch_disk(url,path = glue::glue("data/cache/{endpoint}/{query_string}"), handle = meta)
    response_time <- Sys.time()
    time_diff <- round(response_time - request_time, 3)

    cookie <<- req$headers %>% rawToChar() %>% str_extract("ak_bmsc[^;]+")


    message(glue::glue("\nroundtrip time: {time_diff} seconds"))

    content <- req$content %>% read_file()
  }

  content <- read_file(glue::glue("data/cache/{endpoint}/{query_string}"))


  if(content %>% str_detect("<!DOCTYPE html>")) {

    message(glue::glue("malformed url: {url}"))
    return(NULL)

  }

  res <- jsonlite::fromJSON(content)

  res


}

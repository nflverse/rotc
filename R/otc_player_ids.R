#' Retrieve player ID mappings
#'
#' @param endpoint endpoint (defaults to environment variable OTC_PLAYERID_ENDPOINT)
#' @param api_key api key (defaults to environment variable OTC_API_KEY)
#'
#' @export
otc_player_ids <- function(endpoint = Sys.getenv("OTC_PLAYERID_ENDPOINT"),
                           api_key = Sys.getenv("OTC_API_KEY")){
  stopifnot(
    length(endpoint) == 1 && nchar(endpoint) > 0,
    length(api_key) == 1 && nchar(api_key) > 0
  )

  resp <- httr2::request(endpoint) %>%
    httr2::req_auth_bearer_token(api_key) %>%
    httr2::req_retry(max_tries = 3) %>%
    httr2::req_perform()

  player_ids <- resp %>%
    httr2::resp_body_string() %>%
    jsonlite::fromJSON() %>%
    dplyr::mutate_all( ~replace(.x, .x %in% c("", 0), NA)) %>%
    dplyr::rename(
      gsis_it_id = gsis_id,
      gsis_id = gsis_player_id
    )

  return(player_ids)
}

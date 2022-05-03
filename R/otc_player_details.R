#' Scrape Player Details
#'
#' @param player_url A valid OverTheCap player url
#'
#' @return A tibble containing draft details and contract season history
#' @export
#'
#' @examples
#' \donttest{
#'   otc_player_details("https://overthecap.com/player/aaron-rodgers/1085/")
#' }
otc_player_details <- function(player_url){
  # for tests
  # player_url <- "https://overthecap.com/player/aaron-rodgers/1085/"
  # player_url <- "https://overthecap.com/player/brett-favre/6357/"
  # player_url <- "https://overthecap.com/player/donovan-mcnabb/6750/"
  # player_url <- "https://overthecap.com/player/kyle-spalding/9822/"

  cli::cli_progress_step("Scrape {.url {player_url}}")

  html_scrape <- httr2::request(player_url) %>%
    httr2::req_retry(max_tries = 5) %>%
    httr2::req_perform() %>%
    httr2::resp_body_html()

  season_history <-
    xml2::xml_find_all(html_scrape, ".//*[@class = 'contract salary-cap-history player-new']") %>%
    rvest::html_table() %>%
    purrr::pluck(1)

  # catch missing season history
  if (!is.null(season_history)){
    season_history <- season_history %>%
      janitor::remove_empty("cols") %>%
      janitor::clean_names()
  }

  # Entry info of active players
  entry_info <- xml2::xml_find_all(html_scrape, ".//*[@class = 'league-entry-info']") %>%
    xml2::xml_contents()

  # Entry info of non-active players
  player_bio <- xml2::xml_find_all(html_scrape, ".//*[@class = 'player-bio inactive-fg']") %>%
    xml2::xml_contents()

  # decide which entry info to parse
  # if both are missing, just return season history and player url
  if (length(entry_info) != 0){
    to_parse <- entry_info
  } else if (length(player_bio) == 0 || all(xml2::xml_text(player_bio) == "")){
    return(
      data.frame(
        season_history = list(season_history),
        player_url = player_url
      )
    )
  } else {
    to_parse <- player_bio
  }

  to_parse %>%
    xml2::xml_text() %>%
    stringr::str_split(": ") %>%
    purrr::map_dfc(function(i){data.frame(out = i[[2]]) %>% rlang::set_names(i[[1]])}) %>%
    janitor::clean_names() %>%
    tidyr::separate(
      entry,
      into = c("draft_year", "draft_round", "draft_overall"),
      sep = ", ",
      fill = "right",
      remove = FALSE
    ) %>%
    dplyr::mutate(
      draft_year = stringr::str_extract(draft_year, "[:digit:]+") %>% as.integer(),
      draft_round = stringr::str_extract(draft_round, "[:digit:]+") %>% as.integer(),
      draft_team = stringr::str_extract(entry, "(?<=\\()[:[:alpha:]:]+(?=\\))"),
      draft_overall = stringr::str_extract(draft_overall, "[:digit:]+") %>% as.integer(),
      season_history = list(season_history),
      player_url = player_url
    ) %>%
    dplyr::select(-entry)

}

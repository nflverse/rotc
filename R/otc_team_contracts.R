#' Scrape Historical Contracts By Position
#'
#' @param team A valid character string naming the position to scrape
#'   historical contracts for.
#'
#' @return A tibble
# @export
#'
#' @examples
#' \donttest{
#' otc_historical_contracts("QB")
#' }
otc_team_contracts <- function(team){

  cli::cli_progress_step("Scrape {.val {team}}")

  html_scrape <- httr2::request("https://overthecap.com/salary-cap/") %>%
    httr2::req_url_path_append(team_names[team]) %>%
    httr2::req_retry(max_tries = 5) %>%
    httr2::req_perform() %>%
    httr2::resp_body_html()

  tabs <- xml2::xml_find_all(html_scrape, ".//div[@id='tabs']") %>%
    xml2::xml_find_all(".//div[@id]") %>%
    purrr::map_dfr(function(tab){
      year <- xml2::xml_attr(tab, "id") %>% stringr::str_extract("[:digit:]+") %>% as.integer()
      table_names <- xml2::xml_find_all(tab, ".//h4") %>% xml2::xml_text()
      tables <- rvest::html_table(xml2::xml_find_all(tab, ".//table"))
    })

  hrefs <- xml2::xml_find_all(html_scrape, ".//a") %>%
    xml2::xml_attrs() %>%
    dplyr::bind_rows() %>%
    dplyr::filter(stringr::str_detect(href, "/player/")) %>%
    dplyr::pull(href)

  contratct_status <- xml2::xml_find_all(html_scrape, ".//tr[.//td]") %>%
    xml2::xml_attr("class")

  tbl <- rvest::html_table(html_scrape)[[1]] %>%
    janitor::remove_empty("cols") %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(
      .cols = c(
        dplyr::ends_with("value"),
        dplyr::ends_with("apy"),
        dplyr::starts_with("apy"),
        dplyr::ends_with("guaranteed")
      ),
      .fns = readr::parse_number
    )) %>%
    dplyr::rename(apy_cap_pct = apy_as_percent_of_cap_at_signing) %>%
    dplyr::mutate(
      apy_cap_pct = apy_cap_pct / 100,
      position = position,
      player_page = paste0("https://overthecap.com", hrefs),
      otc_id = as.integer(stringr::str_extract(hrefs, "[:digit:]+")),
      is_active = contratct_status == "active"
    ) %>%
    dplyr::select(player, position, team, is_active, dplyr::everything()) %>%
    tidyr::replace_na(list(is_active = FALSE))

  structure(
    tbl,
    class = c("nflverse_data","tbl_df","tbl","data.table","data.frame"),
    nflverse_timestamp = Sys.time(),
    nflverse_type = "Historical Contract Data from OverTheCap.com"
  )
}

#' Scrape Historical Contracts for Multiple Positions
#'
#' @description This is a wrapper around [otc_historical_contracts()] that
#'   scrapes and binds multiple positions.
#'
#' @param positions A character vector with valid position names forwarded to
#'   [otc_historical_contracts()].
#'
#' @return A tibble
# @export
#'
#' @examples
#' \donttest{
#' # otc_historical_contracts_all()
#' }
otc_historical_contracts_all <- function(positions = NULL){
  if(is.null(positions)) positions <- names(available_positions)
  purrr::map_dfr(positions, otc_historical_contracts)
}

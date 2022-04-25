#' Scrape Historical Contracts By Position
#'
#' @param position A valid character string naming the position to scrape
#'   historical contracts for.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \donttest{
#' otc_historical_contracts("QB")
#' }
otc_historical_contracts <- function(position = c("QB", "RB", "FB", "WR",
                                                  "TE", "LT", "LG", "C",
                                                  "RG", "RT", "IDL", "ED",
                                                  "LB", "CB", "S", "K",
                                                  "P", "LS")){
  position <- rlang::arg_match(position)

  cli::cli_progress_step("Scrape {.val {position}}")

  html_scrape <- httr2::request("https://overthecap.com/contract-history/") %>%
    httr2::req_url_path_append(available_positions[position]) %>%
    httr2::req_retry(max_tries = 5) %>%
    httr2::req_perform() %>%
    httr2::resp_body_html()

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

  tbl
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
#' @export
#'
#' @examples
#' \donttest{
#' # otc_historical_contracts_all()
#' }
otc_historical_contracts_all <- function(positions = NULL){
  if(is.null(positions)) positions <- names(available_positions)
  purrr::map_dfr(positions, otc_historical_contracts)
}

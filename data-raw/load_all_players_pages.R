already_loaded <- readRDS("data-raw/all_players.rds")
already_loaded <- nflreadr::rds_from_url("https://github.com/nflverse/nflverse-data/releases/download/contracts/otc_player_details.rds")

all_players <- nflreadr::load_contracts() |>
  dplyr::filter(!player_page %in% already_loaded$player_url) |>
  dplyr::distinct(player_page)# |> dplyr::slice_sample(n = 50)

purrr::walk(all_players$player_page, function(url){
  # Sys.sleep(0.5)
  load <- try(rotc::otc_player_details(url), silent = TRUE)

  if (inherits(load, "try-error")) {
    cli::cli_warn("Failed to scrape {.url {url}}")
  } else {
    readRDS("data-raw/all_players.rds") |>
      dplyr::bind_rows(load) |>
      dplyr::distinct() |>
      saveRDS("data-raw/all_players.rds")
  }
})

bad_ids <- already_loaded |> count(player_url) |> filter(n>1)
fine_ids <- readRDS("data-raw/all_players.rds") |> count(player_url) |> filter(n==1)

missing <- nflreadr::load_contracts() |>
  dplyr::filter(!player_page %in% fine_ids$player_url) |>
  dplyr::distinct(player_page)

fine_data <- readRDS("data-raw/all_players.rds") |>
  dplyr::filter(player_url %in% fine_ids$player_url) |>
  janitor::remove_empty("cols")

saveRDS(fine_data, "data-raw/otc_player_details.rds")

### DANGER AREA ###
# Reset all_players.rds
# saveRDS(data.frame(), "data-raw/all_players.rds")

df <- purrr::map_dfr(all_players$player_page, function(url){
  # Sys.sleep(0.5)
  load <- try(rotc::otc_player_details(url), silent = TRUE)

  if (inherits(load, "try-error")) {
    cli::cli_warn("Failed to scrape {.url {url}}")
  } else {
    return(load)
  }
})

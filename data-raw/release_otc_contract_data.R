# This file loads and releases OTC contract data
# It is meant to be run in an github action to automate data updates

player_details <- nflreadr::rds_from_url(
  "https://github.com/nflverse/nflverse-data/releases/download/contracts/otc_player_details.rds"
)

# Load OTC Player ID data and join gsis_ids
player_ids <- nflreadr::rds_from_url(
  "https://github.com/nflverse/nflverse-data/releases/download/players_components/otc_players.rds"
)
id_map <- player_ids$gsis_id |> rlang::set_names(player_ids$otc_id)

# we unnest season_history and contract_history to parse strings
# including $ or % signs for compatibility with arrow
# after this step, there won't be any NULL season_history or contract_history
# rows. Instead, there will be nested tibbles with 1 row containing all NA
player_details_parsed <- player_details |>
  tidyr::unnest(
    season_history,
    keep_empty = TRUE,
    names_sep = "_"
  ) |>
  tidyr::unnest(
    contract_history,
    keep_empty = TRUE,
    names_sep = "_"
  ) |>
  dplyr::mutate(
    # parse columns that include a $ sign for arrow compatibility
    dplyr::across(
      dplyr::where(~ any(stringr::str_detect(.x, "\\$"), na.rm = TRUE)),
      ~ readr::parse_number(.x) / 1e6
    ),
    # parse columns that include a % sign for arrow compatibility
    dplyr::across(
      dplyr::where(~ any(stringr::str_detect(.x, "\\%"), na.rm = TRUE)),
      ~ readr::parse_number(.x, na = "--") / 100
    )
  ) |>
  tidyr::nest(
    season_history = dplyr::starts_with("season_history"),
    contract_history = dplyr::starts_with("contract_history"),
    .names_sep = "_"
  ) |>
  # player url would be duplicated so we drop it here
  dplyr::select(-player_url)

contracts <- rotc::otc_historical_contracts_all()
contracts$gsis_id <- id_map[as.character(contracts$otc_id)]

save <- dplyr::left_join(contracts, player_details_parsed, by = "otc_id")

nflversedata::nflverse_save(
  data_frame = save,
  file_name = "historical_contracts",
  nflverse_type = "Historical Contract Data from OverTheCap.com",
  release_tag = "contracts",
  file_types = c("rds", "parquet")
)

cli::cli_alert_success("DONE!")

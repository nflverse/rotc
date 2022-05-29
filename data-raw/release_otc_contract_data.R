# This file loads and releases OTC contract data
# It is meant to be run in an github action to automate data updates

player_details <- nflreadr::rds_from_url("https://github.com/nflverse/nflverse-data/releases/download/contracts/otc_player_details.rds")

contracts <- rotc::otc_historical_contracts_all()

save <- dplyr::left_join(contracts, player_details, by = c("player_page" = "player_url"))

options(piggyback.verbose = FALSE)

nflversedata::nflverse_save(
  data_frame = save,
  file_name = "historical_contracts",
  nflverse_type = "Historical Contract Data from OverTheCap.com",
  release_tag = "contracts",
  file_types = c("rds","qs","parquet")
)

cli::cli_alert_success("DONE!")

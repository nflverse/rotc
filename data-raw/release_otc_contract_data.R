# This file loads and releases OTC contract data
# It is meant to be run in an github action to automate data updates

player_details <- nflreadr::rds_from_url("https://github.com/nflverse/nflverse-data/releases/download/contracts/otc_player_details.rds")

contracts <- rotc::otc_historical_contracts_all()

save <- dplyr::left_join(contracts, player_details, by = c("player_page" = "player_url"))

save_dir <- tempdir()

cli::cli_alert_info("Save Files in working directory {.path {save_dir}}...")
try({
  saveRDS(save, file.path(save_dir, "historical_contracts.rds"))
  readr::write_csv(save, file.path(save_dir, "historical_contracts.csv.gz"))
  arrow::write_parquet(save, file.path(save_dir, "historical_contracts.parquet"))
  qs::qsave(save, file.path(save_dir, "historical_contracts.qs"),
            preset = "custom",
            algorithm = "zstd_stream",
            compress_level = 22,
            shuffle_control = 15
  )
})

to_upload <- list.files(path = save_dir, full.names = TRUE)
to_upload <- to_upload[stringr::str_detect(to_upload, "historical")]

cli::cli_ul("Will release {.path {to_upload}}")

nflversedata::nflverse_upload(to_upload, "contracts")

cli::cli_alert_info("Remove Temporary Directory")
unlink(save_dir, recursive = TRUE)

cli::cli_alert_success("DONE!")

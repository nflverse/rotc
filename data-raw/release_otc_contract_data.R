# This file loads and releases OTC contract data
# It is meant to be run in an github action to automate data updates

save <- rotc::otc_historical_contracts_all()

cli::cli_alert_info("Save Files in working directory {.path {getwd()}}...")
saveRDS(save, "data-raw/to_upload/otc_historical_contracts.rds")
readr::write_csv(save, "data-raw/to_upload/otc_historical_contracts.csv.gz")
arrow::write_parquet(save, "data-raw/to_upload/otc_historical_contracts.parquet")
qs::qsave(save, "data-raw/to_upload/otc_historical_contracts.qs",
          preset = "custom",
          algorithm = "zstd_stream",
          compress_level = 22,
          shuffle_control = 15
)

to_upload <- list.files(path = "data-raw/to_upload", full.names = TRUE)

nflversedata::nflverse_upload(to_upload, "otc_contract_data")

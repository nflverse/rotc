# This file loads and releases OTC contract data
# It is meant to be run in an github action to automate data updates

save <- rotc::otc_historical_contracts_all()

save_dir <- tempdir()

cli::cli_alert_info("Save Files in working directory {.path {save_dir}}...")
saveRDS(save, paste0(save_dir, "/historical_contracts.rds"))
readr::write_csv(save, paste0(save_dir, "/historical_contracts.csv.gz"))
arrow::write_parquet(save, paste0(save_dir, "/historical_contracts.parquet"))
qs::qsave(save, paste0(save_dir, "/historical_contracts.qs"),
          preset = "custom",
          algorithm = "zstd_stream",
          compress_level = 22,
          shuffle_control = 15
)

to_upload <- list.files(path = save_dir, full.names = TRUE)
to_upload <- to_upload[stringr::str_detect(to_upload, "historical")]

cli::cli_ul("Will release {.path {to_upload}}")

nflversedata::nflverse_upload(to_upload, "contract_data")

cli::cli_alert_info("Remove Temporary Directory")
unlink(save_dir, recursive = TRUE)

cli::cli_alert_success("DONE!")

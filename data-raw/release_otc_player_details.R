current_year <- Sys.Date() |> format("%Y") |> as.integer()

details_to_update <- nflreadr::load_contracts() |>
  dplyr::group_by(player_page) |>
  dplyr::filter(year_signed == max(year_signed)) |>
  dplyr::ungroup() |>
  dplyr::mutate(potentially_active = year_signed + years >= current_year) |>
  dplyr::filter(potentially_active == TRUE | is_active == TRUE) |>
  dplyr::distinct(player_page) |>
  dplyr::pull(player_page)

player_details <- nflreadr::rds_from_url("https://github.com/nflverse/nflverse-data/releases/download/contracts/otc_player_details.rds")

cli::cli_alert_info("Start updating {length(details_to_update)} player page{?s}...")

updated <- details_to_update |>
  purrr::map_dfr(purrr::possibly(
    .f = function(url){
      Sys.sleep(0.5)
      rotc::otc_player_details(url)
    },
    otherwise = tibble::tibble(),
    quiet = FALSE
  ))

save <- player_details |>
  dplyr::filter(!player_url %in% updated$player_url) |>
  dplyr::bind_rows(updated) |>
  dplyr::distinct() |>
  dplyr::arrange(draft_year)

if (nrow(save) < nrow(player_details)){
  cli::cli_abort(
    "Number of players to release is {.val {nrow(save)}} but currently
    released are {.val {nrow(player_details)}}. The update workflow potentially
    removed players. Please check that. (Data will NOT be released)"
  )
}

save_dir <- tempdir()

cli::cli_alert_info("Save Files in working directory {.path {save_dir}}...")
try({
  saveRDS(save, file.path(save_dir, "otc_player_details.rds"))
})

to_upload <- list.files(path = save_dir, full.names = TRUE)
to_upload <- to_upload[stringr::str_detect(to_upload, "otc_player_details")]

cli::cli_ul("Will release {.path {to_upload}}")

nflversedata::nflverse_upload(to_upload, "contracts")

cli::cli_alert_success("DONE!")

current_year <- Sys.Date() |> format("%Y") |> as.integer()

# this queries player page URLs of players that are likely active
# we want active players only because those are the ones where contract details
# could potentially change
details_to_update <- nflreadr::load_contracts() |>
  dplyr::group_by(player_page) |>
  dplyr::filter(year_signed == max(year_signed)) |>
  dplyr::ungroup() |>
  dplyr::mutate(potentially_active = year_signed + years >= current_year) |>
  dplyr::filter(potentially_active == TRUE | is_active == TRUE) |>
  dplyr::distinct(player_page) |>
  dplyr::pull(player_page)

player_details <- nflreadr::rds_from_url(
  "https://github.com/nflverse/nflverse-data/releases/download/contracts/otc_player_details.rds"
)

cli::cli_alert_info(
  "Start updating {length(details_to_update)} player page{?s}..."
)

updated <- details_to_update |>
  purrr::map(purrr::possibly(
    .f = function(url) {
      Sys.sleep(0.5)
      rotc::otc_player_details(url)
    },
    otherwise = tibble::tibble(),
    quiet = FALSE
  )) |>
  purrr::list_rbind() |>
  # we use otc ID from player urls because the urls can change when
  # player change names or OTC updates names
  dplyr::mutate(
    otc_id = as.integer(stringr::str_extract(
      player_url,
      "(?<=/)[:digit:]+(?=/)"
    ))
  )

# This will break if columns don't match.
# That's by design to make sure we don't mess things up
save <- player_details |>
  dplyr::rows_upsert(
    updated,
    "otc_id"
  )

if (nrow(save) < nrow(player_details)) {
  cli::cli_abort(
    "Number of players to release is {.val {nrow(save)}} but currently
    released are {.val {nrow(player_details)}}. The update workflow potentially
    removed players. Please check that. (Data will NOT be released)"
  )
}

# only rds because this file is for internal use
nflversedata::nflverse_save(
  data_frame = save,
  file_name = "otc_player_details",
  nflverse_type = "OverTheCap.com Player Details",
  release_tag = "contracts",
  file_types = "rds"
)

cli::cli_alert_success("DONE!")

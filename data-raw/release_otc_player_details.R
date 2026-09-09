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

# we found in https://github.com/nflverse/nflreadr/issues/316#issuecomment-5490778263
# that some drafted players are missing draft info on OTC. I decided to patch these
# by running the following part once. I don't really expect OTC to miss draft info
# on future players so this should be a one time thing. I add the code snippet
# for future reference.
# SEB
if (FALSE) {
  # This snippet computes all players where load_players has otc_id and draft_info
  # and it's not already available in contracts and/or player_details

  players <- nflreadr::load_players() |>
    dplyr::mutate(otc_id = as.integer(otc_id)) |>
    dplyr::filter(!is.na(otc_id), !is.na(draft_year)) |>
    dplyr::select(
      otc_id,
      draft_team,
      draft_year,
      draft_team,
      draft_round,
      draft_overall = draft_pick
    )

  contracts <- nflreadr::load_contracts() |>
    dplyr::filter(!is.na(otc_id)) |>
    dplyr::distinct(
      otc_id,
      draft_team,
      draft_year,
      draft_team,
      draft_round,
      draft_overall
    )

  # This might look weird but it's intentional. Contracts includes player_details
  # but it is created through a left_join. This means that player_details might
  # hold data not included in contracts.
  already_available <- contracts |>
    dplyr::rows_upsert(
      player_details |>
        dplyr::select(
          otc_id,
          draft_team,
          draft_year,
          draft_team,
          draft_round,
          draft_overall
        ),
      by = "otc_id"
    ) |>
    dplyr::filter_out(is.na(draft_year))

  updatable <- players |>
    dplyr::anti_join(already_available, by = "otc_id")

  # use this save object and release it with the nflverse_save block
  # at the end of this script
  save <- player_details |>
    dplyr::rows_upsert(updatable, by = "otc_id")
}

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

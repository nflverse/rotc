
player_ids <- rotc::otc_player_ids()

nflversedata::nflverse_save(
  data_frame = player_ids,
  file_name = "otc_players",
  nflverse_type = "OverTheCap Player IDs",
  release_tag = "players_components"
)

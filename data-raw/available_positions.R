## code to prepare `available_positions` dataset goes here

available_positions <- httr2::request("https://overthecap.com/contract-history/") |>
  httr2::req_perform() |>
  httr2::resp_body_html() |>
  rvest::html_elements("ul") |>
  rvest::html_elements(xpath = ".//a") |>
  xml2::xml_attrs() |>
  stringr::str_extract("(?<=/position/)[:graph:]+") |>
  stats::na.omit() |>
  as.character() |>
  rlang::set_names(
    "QB", "RB", "FB", "WR", "TE", "LT", "LG", "C", "RG", "RT", "IDL", "ED",
    "LB", "CB", "S", "K", "P", "LS"
  )
usethis::use_data(available_positions, overwrite = TRUE, internal = TRUE)

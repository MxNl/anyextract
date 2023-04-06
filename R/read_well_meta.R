read_well_meta <- function(filepath, n_max) {
  readr::read_csv2(filepath, n_max = n_max) |>
    janitor::clean_names() |>
    dplyr::select(
      well_id = proj_id,
      x = x_coord_etrs1989utm32n,
      y = y_coord_etrs1989utm32n
      ) |>
    sf::st_as_sf(coords = c("x", "y")) |>
    sf::st_sf(crs = CRS_WELL_META)
}

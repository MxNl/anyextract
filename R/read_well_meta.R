read_well_meta <- function(filepath) {
  readr::read_delim(filepath, delim = ";") |>
    dplyr::slice_sample(n = N_SITES) |>
    janitor::clean_names() |>
    dplyr::select(
      well_id = proj_id,
      x = x_coord_etrs1989utm32n,
      y = y_coord_etrs1989utm32n
      ) |>
    dplyr::distinct(well_id, x, y) |>
    tidyr::drop_na(x, y) |>
    sf::st_as_sf(coords = c("x", "y")) |>
    sf::st_sf(crs = CRS_WELL_META)
}

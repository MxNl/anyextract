any_extract <- function(extract_source, extract_locations) {
  test <- FALSE
  if(test) {
    extract_source <- targets::tar_read(import_list) |> purrr::chuck(1)
    extract_locations <- targets::tar_read(well_meta)
  }

  crs_origin_extract_locations <- sf::st_crs(extract_locations)
  filepath <- extract_source$data_source_location |> unique()
  file_format <- tools::file_ext(filepath)
  variable_name_origin <- extract_source$variable_name_origin
  variable_name_new <- extract_source$variable_name_new

  if(file_format == "tif") {
    # Extract from Raster data as GeoTIFF
    extract_source_data <- stars::read_stars(filepath, proxy = TRUE)
    extract_locations <- extract_locations |>
      sf::st_transform(sf::st_crs(extract_source_data))
    extract_data <- stars::st_extract(extract_source_data, extract_locations) |>
      dplyr::mutate(well_id = extract_locations$well_id, .before = 1) |>
      dplyr::rename_with(~variable_name_new, dplyr::all_of(variable_name_origin)) |>
      sf::st_drop_geometry()

  } else if (file_format == "shp") {
    # Extract from Vector data as Shapefile
    extract_source_data <- sf::st_read(filepath) |>
      dplyr::select(dplyr::all_of(variable_name_origin))
    extract_locations <- extract_locations |>
      sf::st_transform(sf::st_crs(extract_source_data))
    extract_data <- extract_locations |>
      sf::st_join(extract_source_data) |>
      dplyr::rename_with(~variable_name_new, dplyr::all_of(variable_name_origin)) |>
      sf::st_drop_geometry()
  }
  extract_data |>
    dplyr::as_tibble()
}

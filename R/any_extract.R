any_extract <- function(extract_source, extract_locations) {
  test <- FALSE
  if(test) {
    extract_source <- targets::tar_read(import_list) |> purrr::chuck(11)
    extract_locations <- targets::tar_read(well_meta)
  }

  crs_origin_extract_locations <- sf::st_crs(extract_locations)
  filepath <- extract_source$data_source_location |> unique()
  file_format <- tools::file_ext(filepath)
  variable_name_origin <- extract_source$variable_name_origin
  variable_name_new <- extract_source$variable_name_new

  tictoc::tic()
  if(file_format == "tif") {
    # Extract from Raster data as GeoTIFF
    extract_source_data <- stars::read_stars(filepath, proxy = TRUE) |>
      as("Raster") |>
      terra::rast()
    extract_locations <- extract_locations |>
      sf::st_transform(sf::st_crs(extract_source_data))
    data_extracted <- terra::extract(extract_source_data, extract_locations) |>
      dplyr::rename_with(~variable_name_new, -ID) |>
      dplyr::mutate(well_id = extract_locations$well_id, .before = 1) |>
      dplyr::select(-ID) |>
      dplyr::as_tibble()
      # readr::write_rds(stringr::str_glue("data_proj/well_extract_separate/{variable_name_new}.rds"))

  } else if (file_format == "shp") {
    # Extract from Vector data as Shapefile
    extract_source_data <- sf::st_read(filepath) |>
      dplyr::select(dplyr::all_of(variable_name_origin))
    extract_locations <- extract_locations |>
      sf::st_transform(sf::st_crs(extract_source_data))
    data_extracted <- extract_locations |>
      sf::st_join(extract_source_data) |>
      dplyr::rename_with(~variable_name_new, dplyr::all_of(variable_name_origin)) |>
      sf::st_drop_geometry() |>
      dplyr::as_tibble()
      # readr::write_rds(stringr::str_glue("data_proj/well_extract_separate/{variable_name_new}.rds"))
  }
  tictoc::toc()
  data_extracted
}

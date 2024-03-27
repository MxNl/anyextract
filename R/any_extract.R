#' any_extract
#'
#' @param extract_from tibble containing information about file location from which data should be extracted
#' @param extract_at sf object which is used to extract data at
#' @param id_column Variable in `extract at` that contains an identifier. Default is `NULL`. If `NULL`, an identifier column will be guessed by searching for column names that contain "ID" or "id" and have as many unique values as the number of rows in `extract at`
#'
#' @return
#' @export
#'
any_extract <- function(extract_from, extract_at, id_column = NULL) {
  if(!is.null(id_column)) id_column <- enquo(id_column)
  test <- FALSE
  if(test) {
    id_column = NULL
    extract_from <- targets::tar_read(import_list) |> purrr::chuck(1)
    extract_from <- targets::tar_read(import_list) |> purrr::chuck(24)
    extract_from <- targets::tar_read(import_list) |> purrr::chuck(29)
    extract_from <- targets::tar_read(import_list) |> purrr::chuck(35)
    extract_from <- targets::tar_read(import_list) |> purrr::keep(.p = ~ nrow(.x) > 1) |> purrr::chuck(2)
    extract_at <- targets::tar_read(well_meta)
  }
  if(is.null(id_column)) {
    id_column <- extract_at |>
      sf::st_drop_geometry() |>
      dplyr::select(dplyr::contains("id")) |>
      dplyr::summarise_all(~ unique(.x) |> length()) |>
      dplyr::select_if(.predicate = ~.x == nrow(extract_at)) |>
      colnames() |>
      purrr::chuck(1)

    id_column <- dplyr::sym(id_column)
  }


  crs_origin_extract_from <- extract_from$crs |> unique()
  crs_origin_extract_locations <- sf::st_crs(extract_at)
  filepath <- extract_from$data_source_location |> unique()
  file_format <- tools::file_ext(filepath)
  variable_name_origin <- extract_from$variable_name_origin
  variable_name_new <- extract_from$variable_name_new
  extraction_shape_is_numeric <- extract_from$extraction_shape |>
    unique() |>
    as.numeric() |>
    is.na() |>
    isFALSE()
  extraction_shape <- ifelse(
    extraction_shape_is_numeric,
    as.numeric(extract_from$extraction_shape),
    extract_from$extraction_shape
    )

  extraction_summary_function <- extract_from$extraction_summary_function |> unique()
  if(extraction_shape == "point" & !is.na(extraction_summary_function)) {
    message("You provided a value for the argument extraction_summary_function which is not necessary with extraction_shape == 'point'. Therefore, it will be ignored.")
  }


  tictoc::tic()
## TIFF + Point --------------------------------------------------------------------
  if(file_format %in% c("tif", "adf", "img", "ovr") & extraction_shape == "point") {

    # Extract point from Raster data as GeoTIFF
    extract_from_data <- stars::read_stars(filepath, proxy = TRUE) |>
      as("Raster")
    extract_from_data_crs <- sf::st_crs(extract_from_data)
    if (is.na(extract_from_data_crs)) {
      raster::crs(extract_from_data) <- crs_origin_extract_from
    }
    extract_from_data <- extract_from_data |>
      terra::rast()
    extract_at <- extract_at |>
      sf::st_transform(sf::st_crs(extract_from_data))
    data_extracted <- terra::extract(extract_from_data, extract_at, ID = FALSE) |>
      dplyr::rename_with(~variable_name_new) |>
      dplyr::bind_cols(dplyr::select(extract_at, !! id_column) |> sf::st_drop_geometry()) |>
      dplyr::relocate(!! id_column) |>
      dplyr::as_tibble()
      # readr::write_rds(stringr::str_glue("data_proj/well_extract_separate/{variable_name_new}.rds"))




## TIFF + Circle --------------------------------------------------------------------
### TIFF + Circle + largest area class --------------------------------------------------------------------
  # Extract circle from Raster data as GeoTIFF
  } else if (file_format %in% c("tif", "adf", "img", "ovr") & is.numeric(extraction_shape) & extraction_summary_function == "largest area class") {
    extract_from_data <- stars::read_stars(filepath, proxy = TRUE) |>
      as("Raster")
    extract_from_data_crs <- sf::st_crs(extract_from_data)
    if (is.na(extract_from_data_crs)) {
      raster::crs(extract_from_data) <- crs_origin_extract_from
    }
    extract_at <- extract_at |>
      sf::st_buffer(dist = extraction_shape) |>
      sf::st_transform(sf::st_crs(extract_from_data))
    data_extracted <- extract_from_data |>
      exactextractr::exact_extract(
        extract_at
      ) |>
      purrr::map2_df(
        extract_at |> dplyr::pull(!!id_column),
        ~ .x |> dplyr::group_by(value) |>
          dplyr::summarise(coverage_fraction = sum(coverage_fraction)) |>
          dplyr::mutate(!!id_column := .y, .before = 1)
      ) |>
      dplyr::group_by(!!id_column) |>
      dplyr::slice_max(order_by = coverage_fraction, n = 1) |>
      dplyr::ungroup() |>
      dplyr::select(-coverage_fraction) |>
      dplyr::rename(!!dplyr::sym(variable_name_new) := value)



### TIFF + Circle + area fractions all classes --------------------------------------------------------------------
  # Extract circle from Raster data as GeoTIFF
  } else if (file_format %in% c("tif", "adf", "img", "ovr") & is.numeric(extraction_shape) & extraction_summary_function == "area fractions all classes") {
    extract_from_data <- stars::read_stars(filepath, proxy = TRUE) |>
      as("Raster")
    extract_from_data_crs <- sf::st_crs(extract_from_data)
    if (is.na(extract_from_data_crs)) {
      raster::crs(extract_from_data) <- crs_origin_extract_from
    }
    extract_at <- extract_at |>
      sf::st_buffer(dist = extraction_shape) |>
      sf::st_transform(sf::st_crs(extract_from_data))
    data_extracted <- extract_from_data |>
      exactextractr::exact_extract(
        extract_at
      ) |>
      purrr::map2_df(
        extract_at |> dplyr::pull(!!id_column),
        ~ .x |> dplyr::group_by(value) |>
          dplyr::summarise(coverage_fraction = sum(coverage_fraction)) |>
          dplyr::mutate(!!id_column := .y, .before = 1)
      ) |>
      dplyr::group_by(!!id_column) |>
      dplyr::mutate(
        area_total = sum(coverage_fraction),
        area_frac = as.numeric(coverage_fraction / area_total)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-area_total, -coverage_fraction) |>
      tidyr::pivot_wider(
        id_cols = !!id_column,
        names_from = value,
        values_from = area_frac,
        names_prefix = stringr::str_c(variable_name_new, "_"),
        values_fill = 0
      )



### TIFF + Circle + any stat function --------------------------------------------------------------------
  # Extract circle from Raster data as GeoTIFF
  } else if (file_format %in% c("tif", "adf", "img", "ovr") & is.numeric(extraction_shape)) {
    extract_from_data <- stars::read_stars(filepath, proxy = TRUE) |>
      as("Raster")
    extract_from_data_crs <- sf::st_crs(extract_from_data)
    if (is.na(extract_from_data_crs)) {
      raster::crs(extract_from_data) <- crs_origin_extract_from
      }
    extract_at <- extract_at |>
      sf::st_buffer(dist = extraction_shape) |>
      sf::st_transform(crs = sf::st_crs(extract_from_data))
    data_extracted <- extract_from_data |>
      exactextractr::exact_extract(extract_at, fun = extraction_summary_function) |>
      tibble::enframe() |>
      dplyr::rename_with(~variable_name_new, dplyr::all_of("value")) |>
      dplyr::bind_cols(dplyr::select(extract_at, !! id_column) |> sf::st_drop_geometry()) |>
      dplyr::relocate(!! id_column) |>
      dplyr::select(-name)



### NetCDF + Point --------------------------------------------------------------------
  # Extract point from raster data as NetCDF
    } else if (file_format == "nc" & extraction_shape == "point") {
      extract_from_data <- terra::rast(filepath, subds = variable_name_origin)
      extract_at <- extract_at |>
        sf::st_buffer(dist = extraction_shape) |>
        sf::st_transform(sf::st_crs(extract_from_data))
      data_extracted <- terra::extract(extract_from_data, extract_at, ID = FALSE) |>
        dplyr::rename_with(~variable_name_new) |>
        dplyr::bind_cols(dplyr::select(extract_at, !! id_column) |> sf::st_drop_geometry()) |>
        dplyr::relocate(!! id_column) |>
        dplyr::as_tibble()


### NetCDF + Circle + any stat function --------------------------------------------------------------------
  # Extract circle from Raster data as GeoTIFF
    } else if (file_format == "nc" & is.numeric(extraction_shape)) {
      extract_from_data <- terra::rast(filepath, subds = variable_name_origin)
      extract_at <- extract_at |>
        sf::st_buffer(dist = extraction_shape) |>
        sf::st_transform(sf::st_crs(extract_from_data))
      data_extracted <- extract_from_data |>
        exactextractr::exact_extract(extract_at, fun = extraction_summary_function) |>
        dplyr::as_tibble() |>
        purrr::set_names(paste(variable_name_new, terra::time(extract_from_data), sep = "_")) |>
        dplyr::bind_cols(dplyr::select(extract_at, !! id_column) |> sf::st_drop_geometry()) |>
        dplyr::relocate(!! id_column)



# Shape + Point --------------------------------------------------------------------
  # Extract point from Vector data as Shapefile
  } else if (file_format == "shp" & extraction_shape == "point") {
    extract_from_data <- sf::st_read(filepath) |>
      dplyr::select(dplyr::all_of(variable_name_origin))
    extract_at <- extract_at |>
      sf::st_transform(sf::st_crs(extract_from_data))
    data_extracted <- extract_at |>
      sf::st_join(extract_from_data) |>
      dplyr::rename_with(~variable_name_new, dplyr::all_of(variable_name_origin)) |>
      sf::st_drop_geometry() |>
      dplyr::as_tibble()



# Shape + Circle + largest area class --------------------------------------------------------------------
  # Extract circle from Vector data as Shapefile
  } else if (file_format == "shp" & extraction_shape_is_numeric & extraction_summary_function == "largest area class") {
    extract_from_data <- sf::st_read(filepath) |>
      dplyr::select(dplyr::all_of(variable_name_origin))
    extract_at <- extract_at |>
      sf::st_buffer(dist = extraction_shape) |>
      sf::st_transform(sf::st_crs(extract_from_data))
    data_extracted <- extract_at |>
      sf::st_intersection(extract_from_data) |>
      dplyr::mutate(area = sf::st_area(geometry)) |>
      sf::st_drop_geometry() |>
      tidyr::pivot_longer(cols = -c(!! id_column, area), values_transform = as.character) |>
      dplyr::group_by(!! id_column, name, value) |>
      dplyr::summarise(area = sum(as.numeric(area)), .groups = "drop") |>
      dplyr::group_by(!! id_column, name) |>
      dplyr::slice_max(order_by = area, n = 1) |>
      dplyr::select(-area) |>
      tidyr::pivot_wider() |>
      dplyr::ungroup() |>
      dplyr::rename_with(~variable_name_new, dplyr::all_of(variable_name_origin)) |>
      dplyr::as_tibble()



# Shape + Circle + area fractions all classes --------------------------------------------------------------------
  # Extract circle from Vector data as Shapefile
  } else if (file_format == "shp" & extraction_shape_is_numeric & extraction_summary_function == "area fractions all classes") {
    extract_from_data <- sf::st_read(filepath) |>
      dplyr::select(dplyr::all_of(variable_name_origin))
    extract_at <- extract_at |>
      sf::st_buffer(dist = extraction_shape) |>
      sf::st_transform(sf::st_crs(extract_from_data))

    data_extracted <-
      extract_at |>
      sf::st_intersection(extract_from_data) |>
      dplyr::mutate(area = sf::st_area(geometry)) |>
      sf::st_drop_geometry() |>
      dplyr::rename_with(~variable_name_new, dplyr::all_of(variable_name_origin)) |>
      dplyr::group_by(!!id_column) |>
      dplyr::mutate(
        area_total = sum(area),
        area_frac = as.numeric(area / area_total)
        ) |>
      dplyr::select(- area_total, - area) |>
      tidyr::pivot_longer(cols = dplyr::all_of(variable_name_new)) |>
      dplyr::group_by(!!id_column, name, value) |>
      dplyr::summarise(area_frac = sum(area_frac), .groups = "drop") |>
      tidyr::pivot_wider(
        id_cols = !!id_column,
        names_from = c("name", "value"),
        values_from = area_frac,
        names_glue = "{name}_{value}",
        values_fill = 0
        )



# Shape + Circle + mean --------------------------------------------------------------------
  # Extract circle from Vector data as Shapefile
  } else if (file_format == "shp" & extraction_shape_is_numeric & extraction_summary_function == "mean") {
    extract_from_data <- sf::st_read(filepath) |>
      dplyr::select(dplyr::all_of(variable_name_origin))
    extract_at <- extract_at |>
      sf::st_buffer(dist = extraction_shape) |>
      sf::st_transform(sf::st_crs(extract_from_data))

    data_extracted <-
      extract_at |>
      sf::st_intersection(extract_from_data) |>
      # TODO: now selects maximum area feature: make this an argument with choices
      dplyr::mutate(area = sf::st_area(geometry)) |>
      sf::st_drop_geometry() |>
      dplyr::rename_with(~variable_name_new, dplyr::all_of(variable_name_origin)) |>
      # dplyr::group_by(!! id_column) |>
      dplyr::group_by(!!id_column) |>
      dplyr::mutate(
        area_total = sum(area),
        area_frac = as.numeric(area / area_total)
      ) |>
      dplyr::select(- area_total, - area) |>
      tidyr::pivot_longer(cols = dplyr::all_of(variable_name_new)) |>
      dplyr::group_by(!!id_column, name) |>
      # dplyr::arrange(!!id_column) |>
      dplyr::mutate(value_weighted = value * area_frac) |>
      dplyr::summarise(
        value_weighted_sum = sum(value_weighted),
        .groups = "drop"
      ) |>
      tidyr::pivot_wider(
        id_cols = !!id_column,
        names_from = name,
        values_from = value_weighted_sum
      )
  }
  tictoc::toc()
  data_extracted
}



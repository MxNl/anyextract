#' read_import_list
#'
#' @param filepath filepath to the csv containing the dataset informations
#'
#' @return
#' @export
#'
read_import_list <- function(filepath) {
  import_list <- filepath |>
    readr::read_csv2(show_col_types = FALSE) |>
    tidyr::drop_na(data_source_location, variable_name_new, variable_name_origin) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("data_source_location", "extraction_shape", "extraction_summary_function")),
        ~ .x |> factor(levels = unique(.x))
      )
    ) |>
    dplyr::group_by(data_source_location, extraction_shape, extraction_summary_function) |>
    dplyr::group_split() |>
    purrr::map(
      ~.x |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(c("data_source_location", "extraction_shape", "extraction_summary_function")),
            as.character
          )
        )
    )

  return(import_list)
}

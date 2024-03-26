#' read_import_list
#'
#' @param filepath
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

  if (RUN_MODE == "live") {
    import_list
  } else if (RUN_MODE == "test") {
    import_list |>
      magrittr::extract(1:ifelse(N_DATASETS == Inf, length(import_list), N_DATASETS))
  }
}

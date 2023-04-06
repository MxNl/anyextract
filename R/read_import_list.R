read_import_list <- function(filepath) {
  filepath |>
    readr::read_csv2() |>
    tidyr::drop_na(data_source_location, variable_name_new, variable_name_origin) |>
    dplyr::group_by(data_source_location) |>
    dplyr::group_split() %>%
    magrittr::extract(1:ifelse(RUN_MODE == "live", length(.), N_DATASETS))
}

bind_cols_single_idcol <- function(list_to_bind) {
  well_id_column <- list_to_bind |>
    purrr::chuck(1) |>
    dplyr::select(well_id)

  list_to_bind |>
    purrr:::map(~ .x |> dplyr::select(-well_id)) |>
    purrr::reduce(dplyr::bind_cols) |>
    dplyr::bind_cols(well_id_column) |>
    dplyr::relocate(well_id)
}

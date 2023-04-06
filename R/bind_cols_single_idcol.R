bind_cols_single_idcol <- function(list_to_bind) {
  list_to_bind |>
    purrr::map_at(-1, select, -well_id) |>
    purrr::reduce(dplyr::bind_cols)
}

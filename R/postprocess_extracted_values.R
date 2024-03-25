postprocess_extracted_values <- function(x) {
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::contains("_recharge"),
             ~.x |> stringr::str_remove_all(" mm/Jahr") |>
               as.numeric()
             )
      ) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))
}


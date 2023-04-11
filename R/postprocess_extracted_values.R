postprocess_extracted_values <- function(x) {
  x |>
    mutate(
      across(all_of(c("gwn_recharge")),
             ~.x |> stringr::str_remove_all(" mm/Jahr") |>
               as.numeric()
             )
      ) |>
    mutate(across(where(is.character), as.factor))
}

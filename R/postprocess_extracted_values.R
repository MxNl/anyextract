postprocess_extracted_values <- function(x) {
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("feat_hydro_gwn_recharge")),
             ~.x |> stringr::str_remove_all(" mm/Jahr") |>
               as.numeric()
             )
      ) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor)) |>
    dplyr::select(-matches("100km|50km|10km|5km")) |>
    dplyr::select(-dplyr::num_range("feat_topo_eumohp_sd", 2:9)) |>
    dplyr::select(-dplyr::num_range("feat_topo_eumohp_dsd", 2:9)) |>
    dplyr::select(-dplyr::num_range("feat_topo_eumohp_lp", 2:9))
}


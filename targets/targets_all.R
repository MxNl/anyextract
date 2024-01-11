
# static features ---------------------------------------------------------


targets_all <- list(
  tar_file(
    import_list_location,
    IMPORT_LIST_LOCATION
  ),
  tar_target(
    import_list,
    read_import_list(IMPORT_LIST_LOCATION),
    iteration = "list",
    cue = targets::tar_cue("always")
  ),
  tar_file(
    well_meta_location,
    WELL_META_LOCATION
  ),
  tar_target(
    well_meta,
    read_well_meta(well_meta_location)
  ),
  # tar_target(
  #   well_extracted_list_singlebranch,
  #   import_list |>
  #     furrr::future_map(~ any_extract(.x, well_meta))
  #   # iteration = "list"
  #   # format = "fst_tbl"
  #   # resources = tar_resources(
  #   #   fst = tar_resources_fst(compress = 100)
  #   # )
  # ),
  tar_target(
    well_extracted_list,
    any_extract(import_list, well_meta),
    pattern = map(import_list),
    iteration = "list"
  ),
  tar_target(
    well_extracted,
    well_extracted_list |>
      bind_cols_single_idcol()
      # tidytable::pivot_longer(
      #   cols = -well_id,
      #   names_to = c(".value", "date"),
      #   names_pattern = "(.*)_(.*)"
      # ) |>
      # tidytable::mutate(date = lubridate::ymd_hms(date))
  ),
  tar_target(
    well_extracted_postprocessed,
    well_extracted |>
      postprocess_extracted_values()
  ),
  tar_target(
    export_well_extracted,
    arrow::write_parquet(
      well_extracted_postprocessed,
      "data_proj/well_extracted_static_features_all.parquet"
      ),
    priority = 1
  )
  # tar_quarto(
  #   eda_static_features,
  #   "eda_static_features.qmd"
  # )
)




# dynamic features -------------------------------------------------------------


# targets_all <- list(
#   tar_file(
#     import_list_location,
#     IMPORT_LIST_LOCATION
#   ),
#   tar_target(
#     import_list,
#     read_import_list(IMPORT_LIST_LOCATION),
#     iteration = "list",
#     cue = targets::tar_cue("always")
#   ),
#   tar_file(
#     well_meta_location,
#     WELL_META_LOCATION
#   ),
#   tar_target(
#     well_meta,
#     read_well_meta(well_meta_location)
#   ),
#   # tar_target(
#   #   well_extracted_list_singlebranch,
#   #   import_list |>
#   #     furrr::future_map(~ any_extract(.x, well_meta))
#   #   # iteration = "list"
#   #   # format = "fst_tbl"
#   #   # resources = tar_resources(
#   #   #   fst = tar_resources_fst(compress = 100)
#   #   # )
#   # ),
#   tar_target(
#     well_extracted_list,
#     any_extract(import_list, well_meta),
#     pattern = map(import_list),
#     iteration = "list"
#   ),
#   tar_target(
#     well_extracted,
#     well_extracted_list |>
#       bind_cols_single_idcol() |>
#       dplyr::rename_with(.fn = ~ .x |> stringr::word(end = 1), .cols = -well_id) |>
#       tidytable::pivot_longer(
#         cols = -well_id,
#         names_to = c(".value", "date"),
#         names_pattern = "(.*)_(.*)"
#       ) |>
#       tidytable::mutate(date = lubridate::ymd(date))
#   ),
#   tar_target(
#     export_well_extracted,
#     arrow::write_parquet(
#       well_extracted,
#       "data_proj/well_extracted_t_p_rh_1990-2020.parquet"
#       ),
#     priority = 1
#   )
# )

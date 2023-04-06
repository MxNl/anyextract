targets_import <- list(
  tar_file(
    import_list_location,
    IMPORT_LIST_LOCATION
  ),
  tar_target(
    import_list,
    read_import_list(IMPORT_LIST_LOCATION),
    iteration = "list"
    # cue = tar_cue("always")
  ),
  tar_file(
    well_meta_location,
    WELL_META_LOCATION
  ),
  tar_target(
    well_meta,
    read_well_meta(well_meta_location, n_max = N_SITES)
  ),
  # tar_target(
  #   well_extracted_list,
  #   import_list |>
  #     purrr::map(any_extract, well_meta),
  #   iteration = "list"
  # ),
  tar_target(
    well_extracted_list,
    any_extract(import_list, well_meta),
    pattern = map(import_list),
    iteration = "list",
    format = "fst_tbl",
    resources = tar_resources(
      fst = tar_resources_fst(compress = 100)
    )
  ),
  tar_target(
    well_extracted,
    well_extracted_list |>
      bind_cols_single_idcol()
  ),
  tar_target(
    export_well_extracted,
    readr::write_csv2(
      well_extracted,
      "data_proj/well_extracted.csv"
      )
  )
)

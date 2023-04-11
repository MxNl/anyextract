targets_all <- list(
  tar_file(
    import_list_location,
    IMPORT_LIST_LOCATION
  ),
  tar_target(
    import_list,
    read_import_list(IMPORT_LIST_LOCATION),
    iteration = "list"
  ),
  tar_file(
    well_meta_location,
    WELL_META_LOCATION
  ),
  tar_target(
    well_meta,
    "data_proj/gwlstations.shp" |>
      sf::st_read() |>
      rename(well_id = MEST_ID)
  ),
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
    well_extracted_postprocessed,
    well_extracted |>
      postprocess_extracted_values()
  ),
  tar_target(
    export_well_extracted,
    readr::write_csv(
      well_extracted_postprocessed,
      "data_proj/well_extracted_mariana_commasep.csv"
      )
  )
  # tar_quarto(
  #   eda_static_features,
  #   "eda_static_features.qmd"
  # )
)

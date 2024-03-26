YML_CONFIG <- yaml::read_yaml("config.yml")

RUN_MODE <- purrr::chuck(YML_CONFIG, "run_mode")
IMPORT_LIST_LOCATION <- purrr::chuck(YML_CONFIG, "import_list_location")
WELL_META_LOCATION <- purrr::chuck(YML_CONFIG, "well_meta_location")
N_SITES <- purrr::chuck(YML_CONFIG, RUN_MODE, "n_sites") |> as.numeric()
N_DATASETS <- purrr::chuck(YML_CONFIG, RUN_MODE, "n_datasets") |> as.numeric()
CRS_WELL_META <- 25832


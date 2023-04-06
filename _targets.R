library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "dplyr",
    "readr",
    "janitor",
    "stars",
    "sf"
    ),
  memory = "transient",
  garbage_collection = TRUE
)

# Source target pipelines
path_targets <- "targets"
here::here(path_targets) |>
  list.files() |>
  {\(x) here::here(path_targets, x)}() |>
  purrr::map(source)

# Source functions | Can be replaced later by loading custom package
path_functions <- "R"
here::here(path_functions) |>
  list.files() |>
  {
    \(x) here::here(path_functions, x)
  }() |>
  purrr::map(source)

# switch between parallel and sequential execution
if(yaml::read_yaml(file = "config.yml")$parallel) future::plan(future::multicore)
# RUN_MODE <- yaml::read_yaml(file = "config.yml")$run_mode

# Define targets
c(
  targets_import
)


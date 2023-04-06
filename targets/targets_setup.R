setup_targets_for <- function(run_mode) {
  pipelines <- list(

    generall = list(
      tarchetypes::tar_file(
        python_files_tune,
        list_python_files("tune")
      ),
      tarchetypes::tar_file(
        python_files_train,
        list_python_files("train")
      ),
      tarchetypes::tar_file(
        python_files_predict,
        list_python_files("predict")
      )
    ),


    test = list(
    ),

    life = list(

  )
  )

  purrr::keep(pipelines, names(pipelines) %in% c("generall", RUN_MODE))
}

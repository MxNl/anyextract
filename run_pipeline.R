# targets::tar_renv(extras = c("styler", "showtext", "renv", "visNetwork"))
# tar_watch()

# targets::tar_make_future(workers = future::availableCores() - 1)
if(yaml::read_yaml(file = "config.yml")$parallel) {
  targets::tar_make_future(workers = ceiling(future::availableCores()*0.6))
} else {
  targets::tar_make()
}
# targets::tar_make()
message('targets::tar_visnetwork(label = c("time", "size"), targets_only = TRUE)')

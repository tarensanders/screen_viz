
#' Load Data From CloudStor
#'
#' Load the dataset from CloudStor
#'
#' @importFrom rlang .data
#'
#' @param path The location of the dataset on CloudStor
#' @export
#' @return The loaded dataset
load_data <- function(path) {
  stopifnot(Sys.getenv("CLOUD_USER") != "", Sys.getenv("CLOUD_PASS") != "")

  df <- cloudstoR::cloud_get(
    path,
    user = Sys.getenv("CLOUD_USER"),
    password = Sys.getenv("CLOUD_PASS")
  )
}
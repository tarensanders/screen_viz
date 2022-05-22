
#' Load Data From CloudStor
#'
#' Load the dataset from CloudStor
#'
#' @importFrom rlang .data
#'
#' @param path The location of the dataset on CloudStor
#' @param use_cache Use a cached version of the dataset, if available?
#' @export
#' @return The loaded dataset
load_data <- function(path, use_cache = TRUE) {
  cache_path <- file.path("data", basename(path))

  if (use_cache & file.exists(cache_path)) {
    return(readRDS(cache_path))
  }

  stopifnot(Sys.getenv("CLOUD_USER") != "", Sys.getenv("CLOUD_PASS") != "")

  dest <- ifelse(use_cache, cache_path, NULL)

  df_path <- cloudstoR::cloud_get(
    path,
    dest = dest,
    user = Sys.getenv("CLOUD_USER"),
    password = Sys.getenv("CLOUD_PASS"),
    open_file = FALSE
  )

  return(readRDS(df_path))
}
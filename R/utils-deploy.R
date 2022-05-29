#' Deploy To Staging Environment
#'
#' Deploy the app to the testing environment.
#'
#' @return Nothing
deploy_to_dev <- function() {
  rsconnect::deployApp(appName = "screenviz-dev")
}
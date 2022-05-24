not_implemented_err <- function(text) {
  shinyalert::shinyalert(
    title = "Not Implemented",
    text = text,
    html = TRUE, type = "error"
  )
}
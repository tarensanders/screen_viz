not_implemented_err <- function(text) {
  shinyalert::shinyalert(
    title = "Not Implemented",
    text = text,
    html = TRUE, type = "error"
  )
}

check_if_last <- function(clicked_exp, clicked_out, clicked_cell) {
  if (!is.null(clicked_exp) & !is.null(clicked_out)) {
    if (clicked_exp == clicked_cell$value[1] &
      clicked_out == clicked_cell$value[2]) {
      return(TRUE)
    }
  }
  return(FALSE)
}

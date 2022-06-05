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

push <- function(obj, input) {
  # Credit: https://stackoverflow.com/a/70487708/11053826
  variable <- deparse(substitute(obj))
  out <- get(variable, envir = parent.frame(n = 2))
  out[[length(out) + 1]] <- input
  assign(variable, out, envir = parent.frame(n = 2))
}

pop <- function(obj) {
  # Credit: https://stackoverflow.com/a/70487708/11053826
  variable <- deparse(substitute(obj))
  obj <- get(variable, envir = parent.frame(n = 2))

  if (length(obj) > 0) {
    out <- obj[[length(obj)]]
    assign(variable, obj[-length(obj)], envir = parent.frame(n = 2))
  } else {
    out <- NULL
    assign(variable, out, envir = parent.frame(n = 2))
  }

  return(out)
}

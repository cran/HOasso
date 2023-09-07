print.assortativity <- function(x, ...) {
  h <- length(x)
  x <- matrix(x, dimnames = list(order = 1:h, "assortativity"))
  print(x)

  invisible(NULL)
}

plot.assortativity <- function(x,
  type = "h",
  ylim = c(-1, 1),
  xlab = "Orders",
  ylab = "Assortativity",
  ...
  ) {

  h <- length(x)

  plot.default(
    x = 1:h,
    y = as.numeric(x),
    type = type,
    ylim = ylim,
    xlab = xlab,
    ylab = ylab,
    ...
  )

  invisible(NULL)
}

HOasso <- function(
    g,
    h = 1,
    weighted = is.weighted(g),
    x = c("sout", "dout", "lout", "sin", "din", "lin"),
    y = c("sin", "din", "lin", "sout", "dout", "lout")
) {

  h <- round(max(h))

  A <- W <- get.adjacency(g)
  if (is.weighted(g))
    W <- get.adjacency(g, attr = "weight")
  din <- degree(g, mode = "in")
  dout <- degree(g, mode = "out")
  sin <- strength(g, mode = "in")
  sout <- strength(g, mode = "out")
  lin <- log(sin, 10)
  lout <- log(sout, 10)

  if (weighted) {
    E <- W / sum(W)
  } else {
    E <- A / sum(A)
  }

  q0 <- rowSums(as.matrix(E))

  Dinv <- 1 / q0
  Dinv[is.infinite(Dinv)] <- 0
  Dinv <- diag(Dinv)

  P <- Dinv %*% E
  Ph <- list(P)

  if (h > 1)
    for (i in 2:h)
      Ph[[i]] <- Ph[[i - 1]] %*% P

  Eh <- lapply(Ph, function(x) diag(q0) %*% x)

  qh <- lapply(Eh, function(x) colSums(as.matrix(x)))

  M <- list()
  for (i in 1:h)
    M[[i]] <- (Eh[[i]] - q0 %*% t(qh[[i]]))

  x <- switch(x[1],
              din = din,
              dout = dout,
              sin = sin,
              sout = sout,
              lin = lin,
              lout = lout
  )

  y <- switch(y[1],
              din = din,
              dout = dout,
              sin = sin,
              sout = sout,
              lin = lin,
              lout = lout
  )

  num <- list()
  for (i in 1:h)
    num[[i]] <- t(x) %*% M[[i]] %*% y

  Mx <- (diag(q0) - q0 %*% t(q0))

  My <- list()
  for (i in 1:h)
    My[[i]] <- (diag(qh[[i]]) - qh[[i]] %*% t(qh[[i]]))

  denX <- t(x) %*% Mx %*% x

  denY <- list()
  for (i in 1:h)
    denY[[i]] <- t(y) %*% My[[i]] %*% y

  res <- list()
  for (i in 1:h)
    res[[i]] <- as.numeric(num[[i]] / sqrt(denX * denY[[i]]))

  res <- unlist(res)

  sel <- abs(res) > 1
  res[sel] <- NaN
  if (any(sel, na.rm = TRUE)) {
    warning(paste0("Error of approximation to zero of the covariance or a variance, ", sum(sel), "results forced to NaN"))
  }

  return(res)
}

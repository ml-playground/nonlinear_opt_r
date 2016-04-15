sq.f <- function(x)
{
  nn <- length(x)
  yy <- 1:nn
  f <- sum((yy - x)^2)
  cat("Fv=", f, " at")
  print(x)
  f
}

sq.g <- function(x)
{
  nn <- length(x)
  yy <- 1:nn
  gg <- 2 * (x - yy)
}
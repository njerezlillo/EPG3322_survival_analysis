x <- NULL #Tiempos de vida; hombres.
y <- NULL #Tiempos de vida; mujeres.

logLx <- function(theta)
{
  a <- theta[1]
  b <- theta[2]
  out <- -(a*x)^b + b*log(a) + log(b) + (b-1)*log(x)
  sum(out)
}

logLy <- function(theta)
{
  a <- theta[1]
  b <- theta[2]
  out <- -(a*y)^b + b*log(a) + log(b) + (b-1)*log(y)
  sum(out)
}



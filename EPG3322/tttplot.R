tttplot<-function(t)
{
  t <- sort(t)
  n <- length(t)
  aux <- matrix(0, n, 2)
  S <- sum(t)
  for(i in 1:n)
  {
    aux[i, 1] <- i/n
    aux[i, 2] <- (sum(t[1:i]) + (n - i) * t[i])/S
  }
  return(aux)
}

t<-NULL #inserte datos!

ttt <- tttplot(t)
ttt[1,1] <- 0 ; ttt[1,2] <- 0

par(mfrow = c(1,1))

plot(ttt[,1], ttt[,2], xlim = c(0, 1), ylim = c(0, 1), xlab = "r/n",
     ylab = "G(r/n)", type = "l", col = "1", lwd = 1, lty = 1, main = NULL)
abline(a = 0, b = 1)

require(ggplot2)

nt <- 1000
n  <- 5
iv <- 100

gain <- 0.50
loss <- 0.40

x <- numeric()
x[1:n] <- iv
Xt <- matrix(0.0, nrow=n, ncol=nt+1)
Xt[, 1] = x 
x
for (i in 1:nt){
  rands = (runif(n, 0, 1) < 0.5)
  for (j in 1:n){
    if (rands[j]) {
      x[j] = (x[j] + x[j]*gain)
    } else {
      x[j] = (x[j] - x[j]*loss)
    }
  }
  Xt[, i+1] = x
}

# plot(1:(nt+1), Xt[1, ], type="l", log="y")
matplot(t(Xt), type="l", lty=2, col="gray", log="y", ylab="Wealth", xlab="Time")
Xtmean = colMeans(Xt)
XtGmean = exp(colMeans(log(Xt)))
lines(1:(nt+1), XtGmean, col="blue")
legend("topright", legend=c("Individual trajectories", "Geometric mean"), col = c("grey","blue"),
       pch=c("-", "-"), lwd = c(3,3))





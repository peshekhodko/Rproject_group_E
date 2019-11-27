
# Visualizing Convergence in Probability
N<-1000
n<-50

lambda = 1
pdf_exp <- rep(0, N*n)

for (j in 1:n) {
  for (i in 1:N) {
    pdf_exp[i] <- min (rexp(n,rate = lambda) )
  }
}
plot(x=1:(n*N), y = pdf_exp, cex=0.1)
abline (h=0)

# Visualizing Convergence in Distribution
n = c (5, 10, 30, 100)
N = 50000

lambda = c(1, 5, 25)

data <- list()

for (i in 1:length (n)) {
  data[[i]] <- matrix (0, nrow = N, ncol = n[i])
}

for (j in 1:length(n)){
  for (i in 1:N) {
    data[[j]][i,] <- rpois(n=n[j], lambda = lambda[1])
    
  }
}

mu <- 1
sigma <- 1

means_n5_lbd_1   <- apply (X = data[[1]], MARGIN=1,  mean)
means_n10_lbd_1  <- apply (X = data[[2]], MARGIN=1,  mean)
means_n30_lbd_1  <- apply (X = data[[3]], MARGIN=1,  mean)
means_n100_lbd_1 <- apply (X = data[[4]], MARGIN=1,  mean)

par(mfrow=c(2,2))
hist(means_n5_lbd_1,   main = "Means, sample size n = 5,lambda=1") 
hist(means_n10_lbd_1,  main = "Means, sample size n = 10,lambda=1") 
hist(means_n30_lbd_1,  main = "Means, sample size n = 30,lambda=1")
hist(means_n100_lbd_1, main = "Means, sample size n = 100,lambda=1")


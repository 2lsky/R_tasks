#1
TT <- 100
plot.ts(eps)
y = c(1, 0, 2)
z = c(1, 0, 2)
corr = c()
for (t in 1:100){
  for (j in 4:TT){
    eps_1 <- rnorm(1, 0, 60)
    y[j] <- 0.45*y[j - 1] + 0.45*y[j-3] + eps_1
    eps_2 <- rnorm(1, 0, 60)
    z[j] <- 0.45*z[j - 1] + 0.45*z[j-3] + eps_2
  
  }
  corr[t] = cor(z, y)
}
length(corr)
hist(corr, breaks=20)
mean(corr)
sd(corr)
summary(corr)
x1 = t.test(corr)$conf.int[1]
x2 = t.test(corr)$conf.int[2]
length(corr[(corr > x2) | (corr < x1)])/length(corr)

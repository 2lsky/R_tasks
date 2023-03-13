setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

TT <- 200
eps <- rnorm(TT, 0, 1)
plot.ts(eps)
plot(eps, type = 'l')
plot(eps, type = 'b')

plot(eps, type = "l")
points(eps, pch = 18)
abline(h = 0, col = "blue")

phi <- 0.8
y <- c()
y[1] <- eps[1]
for (t in 2:TT){
  y[t] <- eps[t] + phi * y[t - 1]
}

plot(y, type = 'l')
plot(y, type = 'b')

acf(y)
#----
phi <- -0.8
y <- c()
y[1] <- eps[1]
for (t in 2:TT){
  y[t] <- eps[t] + phi * y[t - 1]
}

plot(y, type = 'l')
plot(y, type = 'b')

acf(y)
#----
acf(y, 20)
lines(phi^(1:20), col = 2)

phi <- 1
y <- c()
y[1] <- eps[1]
for (t in 2:TT){
  y[t] <- eps[t] + phi * y[t - 1]
}

acf(eps)

plot.ts(y)

acf(y, 20)
lines(phi^(1:20), col = 2)

df <- read.csv("data.txt", sep = ';')
df <- data.frame(date = df$X.DATE., x = df$X.VOL.)
names(df) <- c("date", "x")
df$date <- as.Date(df$date)
TT <- nrow(df)
df$x <- df$x / 1e6

plot(df, type = 'l')
summary(df)

acf(df$x)

df <- within(df, {
  x1 <- c(x[1], x[-TT])
  x2 <- c(x1[1], x1[-TT])
  x3 <- c(x2[1], x2[-TT])
  x4 <- c(x3[1], x3[-TT])})
View(df)

ar3 <- lm(x ~ x1 + x2 + x3, data = df)
summary(ar3)

plot(resid(ar3) ~ date, data = df, type =  "l")
abline(h = 0, col = "blue")

library(lmtest)
library(sandwich)

coeftest(ar3)
coeftest(ar3, vcov = vcovHAC)

bgtest(ar3)

summary(reg.bg)

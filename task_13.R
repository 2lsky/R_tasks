setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.delim("GBR_Dairy.tsv")
View(df)
h <- 24
train_len <- length(df$dairy) - h
data <- c(df$dairy[1:train_len])
test <- c(df$dairy[(train_len): length(df$dairy)])
plot.ts(data, type = "o", pch = 18, col = "#0000ff")
grid()

library(forecast)
train_ts <- ts(data = data, start = c(1990, 1), frequency = 12)
test_ts <- ts(data = test, start = c(2008, 1), frequency = 12)
model1 <- auto.arima(train_ts, seasonal = TRUE)
summary(model1)
print(model1)
acf(resid(model1))

model2 <- arima(train_ts, c(2, 1, 1), list(order = c(2, 1, 1)))
summary(model2)
print(model2)
acf(resid(model2))

pr <- predict(model1, n.ahead = h)
yp <- pr$pred
str(yp)

plot(test_ts, type = "o", pch = 18)
grid()
lines(yp, col = "#ff0000", type = "o", pch = 18)

ep <- test_ts - yp
plot(ep, type = "o", pch = 18, ylim = range(ep, 0))
abline(h = 0, col = "#00FF00")

bottom_boundary <- pr$pred - pr$se * qnorm(1 - 0.1)
top_boundary <- pr$pred + pr$se * qnorm(1 - 0.1)

str(bottom_boundary)
str(top_boundary)
str(yp)
full_ts <- ts(data= df$dairy, start = c(1990, 1), frequency = 12)
plot(full_ts, type = "o", pch = 18, xlim = c(2005, 2010))
grid()
lines(yp, col = "blue", type = "o", pch = 18, lty = 2)
lines(bottom_boundary, col = "#FF0000", lty = 2, lwd = 2)
lines(top_boundary, col = "#00FF00", lty = 2, lwd = 2)

#Task2
df <- read.delim("US_sp500.csv", sep = ",")
View(df)
df$Date <- rev(df$Date)
df$Date <- as.Date(df$Date, "%Y-%m-%d")
df$Open <- rev(df$Open)
df$Close <- rev(df$Close)
df$High <- rev(df$High)
df$Low <- rev(df$Low)
df$Volume <- rev(df$Volume)
df[Adj.Close] <- rev(df[Adj.Close])
# Рассчитайте логарифмические доходности r=diff log * 100
r <- diff(log(df$Close)) * 100
plot.ts(r)
grid()
abline(h = 0, col = "#FF0000")
# Постройте график АКФ для r^2 (квадратов доходностей)
acf(r^2)
# и |r| (модулей доходностей)
acf(abs(r))
# Рассчитайте волатильность по методу Riskmetrics *, при λ=0.95. Постройте график доходностей и ±волатильности.
# (* см. http://uu.diva-portal.org/smash/get/diva2:304496/FULLTEXT01.pdf формула (30), p. 14)
data_len <- length(df$Close)
lambda <- 0.95
s2 <- c(1)
for (t in 2:data_len){
  s2[t] <- lambda * s2[t - 1] + (1 - lambda) * r[t - 1]^2
}
vol.rm <- sqrt(s2)

plot.ts(vol.rm, ylim = range(vol.rm, 0))
abline(h = 0, col = "#FF0000", lwd = 2)

plot.ts(r)
lines(vol.rm, col = "#0000FF", lwd = 3)
lines(-vol.rm, col = "#ff0000", lwd = 3)
abline(h = 0, col = "#00ff00", lwd = 2)

library(fGarch)
ga <- fGarch::garchFit(~ garch(1, 1), r,
                       include.mean = FALSE,
                       cond.dist = "std",
                       trace = FALSE)
print(ga)

vol.ga <- volatility(ga)

plot.ts(vol.ga, ylim = range(vol.ga, 0))
abline(h = 0, col = "#FF0000", lwd = 2)

plot.ts(r)
lines(vol.ga, col = "#0000FF", lwd = 3)
lines(-vol.ga, col = "#ff0000", lwd = 3)
abline(h = 0, col = "#00ff00", lwd = 2)

pred <- predict(ga, n.ahead = 100)
plot(pred$standardDeviation)
#volatility predict = volp
volp <- c(vol.ga, pred$standardDeviation)

plot.ts(volp, xlim = c(12500, data_len + 100),
        col = "#000000", ylim = c(0, 3))
abline(v = TT, lty = 3)
lines(vol.ga, col = "#ff0000")
abline(h = 0, col = "#00ff00", lwd = 2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read.delim(file = "Exchange_Rate_Report.csv", sep = ",",
                 stringsAsFactors = TRUE) 

names(df) <- c("Date", "Thai")
dfThai <- as.numeric(df$Thai)
df$Date <- as.Date(df$Date)
df$log <- log(df$MXN)
head(df)
data_len <- length(df$log)
tr <- 1:data_length
train_data_len <- 365 * 2
train_data <- tr <= data_len - train_data_len
test_data <- !train_data
x <- df$log
x1 <- c(x[1], x[-length(x)])
plot(x ~ x1)

x2 <- c(x1[1], x1[-length(x1)])
plot(x ~ x2)

xp.RW <- x1[test_data]
ar_1 <- lm(x ~ x1, suset = train_data)
summary(ar_1) # смотрим статистические параметры

cf.ar_1 <- coef(ar_1)
xp.ar_1 <- cf.ar_1[1] + cf.ar_1[2] * x1[test_data]

xpxp <- cbind(xp.RW, xp.ar_1)
ts.plot(xpxp, col = 1:2)
epep <- x[test_data] - xpxp

mse <- colMeans(epep^2)
rmse <- sqrt(mse)

reg.DM <- lm(epep[, 2]^2 - epep[, 1]^2 ~ 1)
summary(reg.DM)
library(lmtest)
library(sandwich)
coeftest(reg.DM)
coeftest(reg.DM, vcov = vcovHAC)

ts.plot(epep, col = 1:2)

# Task 1
data_length <- length(df$Thai)
train_length <- 365 * 2
x1 <- df$Thai[c(2:((data_length) - 1))]
x2 <- df$Thai[c(1:((data_length) - 2))]
altdf <- df[c(3:data_length), ]
altdf$x1 <- x1
altdf$x2 <- x2

train_data <- altdf[c(1:(data_length - train_length)), ]
test_data <- altdf[c(data_length - train_length:data_length), ]
predicted_data <- data.frame(RW = test_data$x1)

ar_1 <- lm(data = train_data, Thai ~ x1)
summary(ar_1)
predicted_data$ar_1 <-  predict(ar_1,
                                newdata = data.frame(x1 = test_data$x1))

ar_2 <- lm(data = train_data, Thai ~ x1 + x2)
summary(ar_2)
predicted_data$ar_2 <-  predict(ar_2,
                                newdata = data.frame(x1 = test_data$x1, x2 = test_data$x2))

ar_02 <- lm(data = train_data, Thai ~ x2)
summary(ar_02)
predicted_data$ar_02 <-  predict(ar_02,
                                 newdata = data.frame(x1 = test_data$x1, x2 = test_data$x2))

predicted_data$m2 <- (test_data$x1 + test_data$x2) / 2

draw_plot <- function(test_data, predicted_data,
                      data_name, data_begin, data_end) {
  plot(data = test_data[c(data_begin:data_end), ],
       MXN ~ Date, type = "l", xlab = data_name)
  lines(predicted_data[c(data_begin:data_end)] ~
          test_data$Date[c(data_begin:data_end)], col = "#0000ff")
}

draw_plot(test_data, predicted_data$RW,
          "RW predict", 700, train_length)

draw_plot(test_data, predicted_data$ar_1,
          "ar_1 predict", 700, train_length)

draw_plot(test_data, predicted_data$ar_2,
          "ar_2 predict", 700, train_length)

draw_plot(test_data, predicted_data$ar_02,
          "ar_02 predict", 700, train_length)

draw_plot(test_data, predicted_data$m2,
          "m2 predict", 700, train_length)

rmse <- {}
rmse$rw <- sqrt(mean((test_data$MXN - predicted_data$RW)^2)) * 100
rmse$ar_1 <- sqrt(mean((test_data$MXN - predicted_data$ar_1)^2)) * 100
rmse$ar_2 <- sqrt(mean((test_data$MXN - predicted_data$ar_2)^2)) * 100
rmse$ar_02 <- sqrt(mean((test_data$MXN - predicted_data$ar_02)^2)) * 100
rmse$m2 <- sqrt(mean((test_data$MXN - predicted_data$m2)^2)) * 100
print(rmse)

rw_compare <- function(rw_data, compare_data, test_data) {
  compare <- lm((test_data - rw_data)^2 -
                  (test_data - compare_data)^2 ~ 1)
  summary(compare)
}

rw_compare(predicted_data$RW, predicted_data$ar_1, test_data$MXN)
rw_compare(predicted_data$RW, predicted_data$ar_2, test_data$MXN)
rw_compare(predicted_data$RW, predicted_data$ar_02, test_data$MXN)
rw_compare(predicted_data$RW, predicted_data$m2, test_data$MXN)

#
# Task 2
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.delim("Rus_el.tsv")
df2 <- read.delim("Rus_Sugar.tsv")
head(df)
df <-data.frame(dairy = df$dairy[1:length(df2$confect)],
                month = df$month[1:length(df2$confect)])
df$log <- log(df$dairy)
df2$log <- log(df2$confect)

plot.ts(df$dairy)
grid()
cor(df$dairy, df2$confect)

cor.test_data(df$dairy, df2$confect)
plot(df$log ~ df2$log)

#
#
predict_length <- 252
dairy <- df$dairy
month <- rep(1:12, length(dairy) / 12)
times <- c(1:length(dairy))
lms <- lm(dairy ~ times + factor(month), subset = c(1:predict_length))

predicted_data <- predict(lms,
                          newdata = data.frame("times" = c(1:(predict_length + 24)), "month" = month))

plot(dairy, type = "l", lwd = 2, col = "#2b00ff")
lines(predicted_data, col = "#04ff00")
abline(v = predict_length, lty = 4, col = "#ff0000")

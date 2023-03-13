##2 задание
#1
df <- read.delim(file = "C:/Users/Vitam/OneDrive/Рабочий стол/JapCement.csv", sep = ",",
                 stringsAsFactors = TRUE, skip=4) 
names(df) <- c("Date", "Value")
df
df$Value <- as.numeric(df$Value)
df$log <- log(df$Value)
head(df)
vals = df$Value
names(vals) = 1:length(df$Date)
log_vals = df$log
names(log_vals) = 1:length(df$Date)
#2
acf(log_vals, col = "#0008ff")
lag_1 <- c(log_vals[1], log_vals[-length(log_vals)])
length(lag_1)
length(log_vals)
first_diff = log_vals - lag_1
plot(first_diff)
acf(first_diff, col = "#0008ff")
plot(log_vals)
##Вывод - ряд нестационарный, наблюдается периодичность первой разности с значимым лагом кратным 4.
#3
length(log_vals)
test_period = 4*3
train_vals = log_vals[1:(length(log_vals) - test_period - 1)]
test_vals = log_vals[(length(log_vals) - test_period + 1):length(log_vals)]
test_vals
train_vals
df$Index = 1:length(df$Date)
df$Index
simple_reg <- lm(train_vals ~ df$Index, subset = c(1:(length(log_vals) - test_period - 1)))
a = summary(simple_reg)$coefficients[2,1]
a
b = summary(simple_reg)$coefficients[1,1]
b
summary(simple_reg)
preds = a*((length(log_vals) - test_period + 1):length(log_vals)) + b
names(preds) = (length(log_vals) - test_period + 1):length(log_vals)
plot(test_vals, col='blue')
points(preds, col='red')
abline(h = 0, col = "#00ff00", lwd = 2)
preds
test_vals
diff = test_vals - preds
diff
plot(diff)
abline(h = 0, col = "#00ff00", lwd = 2)
#4
install.packages('forecast')
library(forecast)
train_data <- c(df$log[1:(length(log_vals) - test_period - 1)])
test_data <- c(df$log[(length(log_vals) - test_period + 1):length(log_vals)])
plot.ts(train_data, type = "o", pch = 18, col = "#0000ff")
grid()
train_ts <- ts(data = train_data, start = c(1956, 1), frequency = 4)
test_ts <- ts(data = test_data, start = c(2011, 1), frequency = 4)
sarima_model = arima(train_ts, c(1, 1, 1), list(order = c(1, 1, 1)))
summary(sarima_model)
print(sarima_model)
acf(resid(sarima_model))
pr <- predict(sarima_model, n.ahead = 12)
yp <- pr$pred
test_vals
#5
mean_error_sarima = sum(abs(yp - test_vals))/length(test_vals)
mean_error_sarima
mean_error_simple_reg = sum(abs(preds - test_vals))/length(test_vals)
mean_error_simple_reg


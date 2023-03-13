path = "C:/Users/Vitam/Downloads/"
setwd(path)
getwd()
library(readr)
df <- read.delim("Exchange_Rate_Report.tsv", 
                                   skip = 2, stringsAsFactors = FALSE)
names(df) = c('None', 'Date', 'Thai', 'None')
library("stringr")
for(row in 1:nrow(df))
  df$Thai[row] <- as.double(gsub(",", ".", str_split(df$Thai[row], ' ')[[1]][1]))
df$Thai = as.double(df$Thai)
install.packages("anytime")
library("anytime")
df$Date = anydate(df$Date)
df$log <- log(df$Thai)
head(df)
df$Thai
acf(df$Thai, main = "Thai/USD", col = "#0008ff", na.action = na.pass)
plot(df$log ~ df$Date, col = "#0008ff", ylab = "log(USD/Thai)", type = "l")

mat <- matrix(df$log, nrow = 60)
mat
iter <- sample(1:80, 10)
plot.ts(mat[, iter])

install.packages("corrplot")

cor_1 <- cor(mat[, iter])
corrplot::corrplot(cor_1, method = "number")
cor_2 <- cor(mat[, sample(1:80, 50)])
corrplot::corrplot(cor_2)

cor_3 <- cor(mat)
cor_3 <- c(cor_3[lower.tri(cor_3)])
hist(cor_3, breaks = 27)

# Task2 

data_len <- length(df$log)
train_supp <- 1:data_len
train_supp_pow2 <- train_supp^2
tr_len <- length(train_supp)
predict_len <- 365 * 2
train_data_len <- tr_len - predict_len

data <- data.frame(df$log[1:tr_len], train_supp[1:tr_len],
                   train_supp_pow2[1:tr_len])
colnames(data) <- c('log', 'train_supp', 'train_supp_pow2')
reg <- lm(df$log ~ train_supp + train_supp_pow2, data)

summary(reg)

plot(df$log ~ df$Date, col = "#0008ff", ylab = "log(USD/Thai)", type = "l")
lines(fitted(reg) ~ df$Date, col = "#000000", na.action = na.pass)

length(fitted(reg))
length(df$Date)
predicted_data <- predict(reg,
                          newdata = list(train_supp = train_supp[train_data_len+1:tr_len],
                                         train_supp_pow2 = train_supp_pow2[train_data_len + 1:tr_len]))
temp <- df$log[train_data_len + 1:data_len]
temp <- temp[1:predict_len]
plot.ts(temp)
lines(predicted_data, col = 2)

plot.ts(df$log)
vec <- c(fitted(reg), predicted_data)
lines(vec, col = 2)
lines(fitted(reg), col = 3)


prediction_diff <- df$log[train_data_len + 1:data_len] - predicted_data
prediction_diff <- prediction_diff[1:predict_len]
plot(prediction_diff, type = 'l')
abline(h = 0, col = 2)

MSE <- mean(prediction_diff^2)
RMSE <- sqrt(MSE)
temp_arr <- df$log[train_data_len + 1:data_len]
temp_arr <- temp_arr[1: predict_len]
temp <- var(temp_arr)
R2 <- 1 - MSE / temp

print(MSE)
print(RMSE)
print(R2)

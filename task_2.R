df <- read.delim(file = "C:/Users/Vitam/OneDrive/Рабочий стол/crypto.csv", sep = ",",
                 stringsAsFactors = TRUE)
log_dp = diff(log(df$ETC))/log(df$ETC[-length(df$ETC)])*100
date <- as.Date(df$Date)
names(log_dp) = date[c(1:length(date) - 1)]
#1
plot.ts(log_dp)
lambda <- 0.95
s2 <- c(1)
for (t in 2:length(log_dp)){
  s2[t] <- lambda * s2[t - 1] + (1 - lambda) * log_dp[t - 1]^2
}
vol.rm <- sqrt(s2)
plot.ts(vol.rm, ylim = range(vol.rm, 0))
abline(h = 0, col = "#FF0000", lwd = 2)
vol.rm
plot.ts(log_dp)
lines(vol.rm, col = "#0000FF", lwd = 3)
lines(-vol.rm, col = "#ff0000", lwd = 3)
abline(h = 0, col = "#00ff00", lwd = 2)
indicator = ifelse(log_dp > 0, '+', '-')
names(indicator) = names(log_dp)
#2
new_data = data.frame(vol = vol.rm, sign = indicator)
new_data
n = nrow(new_data)
train = 1:60
new_data$sign = ifelse(new_data$sign == '+', 1, 0)
new_data
train
lgt = glm(sign ~ vol.rm, data=new_data, subset=train, family="binomial")
summary(lgt)
#является незначимой по t тесту
#3
pp = predict(lgt, newdata=new_data[c(61:length(new_data$sign)), ], type='response')
pp
mean(pp)
mean(new_data$sign[-train])
install.packages('pROC')
length(new_data$sign[c(61:length(new_data$sign))])
length(pp)
test_len = c(61:length(new_data$sign))
pROC::plot.roc(new_data$sign[test_len], pp[test_len], col='green')
pROC::roc(new_data$sign[test_len], pp[test_len])
#4
new_diff = new_data$sign[test_len] - pp[test_len]
new_diff_sqrt = new_diff^2
brier = mean(new_diff_sqrt)
brier
#5
all_preds <- cbind(pp[test_len], rep(0.5, length(pp[test_len])))
ep <- new_data$sign[test_len] - all_preds
reg.DM <- lm(ep[,2]^2 - ep[,1]^2 ~ 1) #тест Д-М
summary(reg.DM)

install.packages("modeldata")
df = modeldata::lending_club
table(df$Class)
prop.table(table(df$Class))*100
df$bad = (df$Class =="bad")+0
mean(df$bad)
hist(df$funded_amnt, breaks=100)
df$log_amnt = log(df$funded_amnt)
hist(df$log_amnt, breaks=100)
hist(df$annual_inc)
summary(df$annual_inc)
df$log_inc = log(pmax(df$annual_inc, 1000))
hist(df$log_inc, breaks=100)
df$log_inc
rug(df$log_inc)
table(df$term)
hist(df$int_rate, breaks=100)
hist(df$int_rate, breaks=seq(5, 29, 1))
table(df$sub_grade)
barplot(proportions(table(df$sub_grade)))
table(df$emp_length)
barplot(proportions(table(df$emp_length)))
df$term_new = ifelse(df$term == "term_36", 36, 60)
df$sub_grade = as.numeric(df$sub_grade)
df$sub_grade
barplot(proportions(table(df$sub_grade)))
df$empl = c(0, 1, 10, 2:9, 11)[df$emp_length]
df$empl
barplot(proportions(table(df$empl)))
barplot(proportions(table(df$emp_length)))
round(cor(df[c("bad", "log_amnt", "term_new", "int_rate",
                "sub_grade", "log_inc", "empl")]), 3)
plot(proportions(table(df$Class, df$term), margin=2)*100)
plot(proportions(table(df$sub_grade, df$Class), margin=1)*100)
plot(proportions(table(df$int_rate, df$Class), margin=1)*100)

lgt = glm(bad ~ log_inc + term_new + int_rate + log_amnt + sub_grade, data=df, family="binomial")
summary(lgt)
plot(bad ~ predict(lgt), data=df, pch="|"); grid()
points(fitted(lgt) ~ predict(lgt))
prop = table(fitted(lgt) > 0.5, df$Class)
library("pROC")
pROC::plot.roc(df$bad, fitted(lgt))
pROC::roc(df$bad, fitted(lgt))
n = nrow(df)
train = sample(1:n, 0.8*n)
test = -train
train
lgt_1 = glm(bad ~ term_new + int_rate + log_amnt + sub_grade, data=df, subset=train, family="binomial")
summary(lgt_1)
lgt_2 = glm(bad ~ log_inc + term_new, data=df, subset=train, family="binomial")
summary(lgt_2)
pp1 = predict(lgt_1, newdata=df[test, ], type='response')
mean(pp1)
pp3 = predict(lgt_2, newdata=df[test, ], type='response')
mean(pp3)
pp2 = predict(lgt, newdata=df[test, ], type='response')
mean(pp2)
table(df$Class[test], pp2>0.5)
pp = cbind(pp1, pp2, pp3)
pROC::plot.roc(df$bad[test], pp1, col='green')
pROC::roc(df$bad[test], pp1)
pROC::plot.roc(df$bad[test], pp2, add=1, col='red')
pROC::roc(df$bad[test], pp2)
pROC::plot.roc(df$bad[test], pp3, add=1, col='blue')
pROC::roc(df$bad[test], pp3)
Brier = colMeans((df$bad[test] - pp)^2); Brier
sqrt(Brier) * 100
LogS1 = mean(df$bad[test] * log(pp1) + (1 - df$bad[test])*log(1 - pp1))
LogS1
LogS2 = mean(df$bad[test] * log(pp2) + (1 - df$bad[test])*log(1 - pp2))
LogS2
LogS3 = mean(df$bad[test] * log(pp3) + (1 - df$bad[test])*log(1 - pp3))
LogS3
BB = (df$bad[test] - pp)^2
reg.DM = lm(BB[, 1] - BB[,2] ~ 1)
summary(reg.DM)

heartDisease = read.table("heartDisease.csv", header=T, sep=",", dec = ".")

library(ggplot2)

library(pROC)

hist(heartDisease$age)

summary(heartDisease$age)

ggplot(heartDisease, aes(y = age, x = as.factor(TenYearCHD))) + geom_boxplot()

table(heartDisease$age, heartDisease$male)

model = glm(TenYearCHD ~ male + currentSmoker + diabetes + age, family = binomial, data = heartDisease)

model = glm(TenYearCHD ~  age + male, family = binomial, data = heartDisease)

summary(model)

x_age = seq(32, 100, by = 1)

linear_pred_male0 = model$coefficients[1] + x_age*model$coefficients[2] + 0 *model$coefficients[3]

y_proba_male0 = exp(linear_pred_male0)/ (1 + exp(linear_pred_male0))

linear_pred_male1 = model$coefficients[1] + x_age*model$coefficients[2] + 1 *model$coefficients[3]

y_proba_male1 = exp(linear_pred_male1)/ (1 + exp(linear_pred_male1))

plot(y_proba_male1~x_age,type = "l", ylab = "Probalité d'être à risque", xlab = "Âge", col = "blue")

lines(y_proba_male0~x_age,type = "l", ylab = "Probalité d'être à risque", xlab = "Âge", col = "red")

abline(h = 0.5,lty = 2)


###########################################################################################

nrow(heartDisease)

table(heartDisease$TenYearCHD)

table(heartDisease$TenYearCHD[1:(nrow(heartDisease)/2)])

table(heartDisease$TenYearCHD[(nrow(heartDisease)/2 + 1):nrow(heartDisease)])

data_app = heartDisease[(nrow(heartDisease)/2 + 1):nrow(heartDisease),]

data_test = heartDisease[1:(nrow(heartDisease)/2),]

data_test[,-ncol(data_test)]

mod_app = glm(TenYearCHD ~ male + age, family = binomial, data = data_app)

mod_comp = glm(TenYearCHD ~., family = binomial, data = data_app)

summary(mod_app)

summary(mod_comp)

predict_values = predict.glm(mod_app, data_test, type = 'response')

predict_values2 = predict.glm(mod_comp, data_test, type = 'response')

auc(data_test$TenYearCHD ~ predict_values)

auc(data_test$TenYearCHD ~ predict_values)

auc(data_test$TenYearCHD ~ predict_values2)

roc(data_test$TenYearCHD,predict_values, plot = T)

roc(data_test$TenYearCHD,predict_values2, plot = T)

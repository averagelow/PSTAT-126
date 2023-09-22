sim_1 = function(sample_size = 500) 
{
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = 1)
  data.frame(x, y)
}
sim_2 = function(sample_size = 500)
{
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = x)
  data.frame(x,y)
}
sim_3 = function(sample_size = 500)
{
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x ^ 2 + rnorm(n = sample_size, mean = 0, sd = 5)
  data.frame(x, y)
}

#Model 2
#Residuals Plot
set.seed(42)
sim_data_1 = sim_1()
plot(y ~ x, data = sim_data_1, col = "grey", pch = 20,
     main = "Data from Model 1")
fit_1 = lm(y ~ x, data = sim_data_1)
abline(fit_1, col = "darkorange", lwd = 3)

plot(fitted(fit_1), resid(fit_1), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)


sim_data_2 = sim_2()
fit_2 = lm(y ~ x, data = sim_data_2)
plot(y ~x, data = sim_data_2, col = "grey", pch = 20,
     main = "Data from Model 2")
abline(fit_2, col = "darkorange", lwd =3)
#May not have constant variance bc points start to spread out at end of line

#Constant variance assumption = homostachisity 
#Fitted plot useful for multiple regression
plot(fitted(fit_2), resid(fit_2), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 2")
abline(h = 0, col = "darkorange", lwd =2)


#Using Model 3
sim_3 = function(sample_size = 500)
{
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x^2 + rnorm(n = sample_size, mean = 0, sd = 5)
  data.frame(x,y)
}
#Residuals Plot
set.seed(42)
sim_data_3 = sim_3()
fit_3 = lm(y ~ x, data = sim_data_3)
plot(y ~x, data = sim_data_3, col = "grey", pch = 20,
     main = "Data from Model 3")
abline(fit_3, col = "darkorange", lwd =3)
#Does not have even distribution of data pts

#Fitted plot
plot(fitted(fit_3), resid(fit_3), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 3")
abline(h = 0, col = "darkorange", lwd =2)
#More apparent does not have even distribution of data pts

#Model 4
sim_4 = function(sample_size = 500)
{
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x^2 + rnorm(n = sample_size, mean = 0, sd = x)
  data.frame(x,y)
}
#Residuals Plot
set.seed(42)
sim_data_4 = sim_4()
fit_4 = lm(y ~ x, data = sim_data_4)
plot(y ~x, data = sim_data_4, col = "grey", pch = 20,
     main = "Data from Model 3")
abline(fit_4, col = "darkorange", lwd =3)
#Does not have even distribution of data pts

#Fitted plot
plot(fitted(fit_4), resid(fit_4), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 3")
abline(h = 0, col = "darkorange", lwd =2)


#Histogram plot for models 1-3
par(mfrow = c(1,3))
hist(resid(fit_1),
     xlab = "Residuals",
     main = "Histogram of residuals, fit_1",
     col = "darkorange",
     border = "dodgerblue",
     breaks = 20)
hist(resid(fit_2),
     xlab = "Residuals",
     main = "Histogram of residuals, fit_2",
     col = "darkorange",
     border = "dodgerblue",
     breaks = 20)
hist(resid(fit_3),
     xlab = "Residuals",
     main = "Histogram of residuals, fit_3",
     col = "darkorange",
     border = "dodgerblue",
     breaks = 20)

#Q-Q plots: Quantile - Quantile plot
qqnorm(resid(fit_1), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(fit_1), col = "dodgerblue", lwd =2)
#Normality of errors because the residuals are on the regression line

qqnorm(resid(fit_2), main = "Normal Q-Q Plot, fit_2", col = "darkgrey")
qqline(resid(fit_2), col = "dodgerblue", lwd =2)
#Indicates pts being violated

qqnorm(resid(fit_3), main = "Normal Q-Q Plot, fit_3", col = "darkgrey")
qqline(resid(fit_3), col = "dodgerblue", lwd =2)
##Indicates pts being violated


mpg_hp_am = lm(mpg ~ hp + am, data = mtcars)
plot(fitted(mpg_hp_am), resid(mpg_hp_am),col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residual",
     main = "mtcars: Fitted vs. Residuals")
abline(h = 0, col = "darkorange", lwd =2)
#looks balanced

qqnorm(resid(mpg_hp_am), col = "darkgrey")
qqline(resid(mpg_hp_am), col = "dodgerblue", lwd =2)
#Presence of Normal distr. 

# Use transformations for: 
#Lack of linearity, lack of constant variance

alltech = read.csv("~/Desktop/PSTAT 126/alltech.csv")

plot(salary ~ years, data = alltech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at alltech, By Seniority")

alltech_fit = lm(salary ~ years, data = alltech)
summary(alltech_fit)

plot(salary ~ years, data = alltech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at alltech, By Seniority")
abline(alltech_fit, col = "darkorange", lwd =2)

par(mfrow = c(1,2))

plot(fitted(alltech_fit), resid(alltech_fit), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted vs. Residuals")
abline(h = 0, col = "darkorange", lwd =2)
#Values fan out 

qqnorm(resid(alltech_fit), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(alltech_fit), col = "dodgerblue", lwd =2)

#Lecture 10 using log transformation
alltech_fit_log = lm(log(salary) ~ years, data = alltech)

plot(log(salary) ~ years, data = alltech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at alltech, By Seniority")
abline(alltech_fit_log, col = "darkorange", lwd =2)

plot(log(salary) ~ years, data = alltech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at alltech, By Seniority")
curve(exp(alltech_fit_log$coef[1] + alltech_fit_log$coef[2] * x),
      from = 0, to = 30, add = TRUE, col = "darkorange", lwd =2)

par(mfrow = c(1,2))

plot(fitted(alltech_fit_log), resid(alltech_fit_log), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted vs. Residuals")
abline(h = 0, col = "darkorange", lwd =2)

qqnorm(resid(alltech_fit_log), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(alltech_fit_log), col = "dodgerblue", lwd =2)

sqrt(mean(resid(alltech_fit)^2))
sqrt(mean(resid(alltech_fit_log)^2))

sqrt(mean((alltech$salary - fitted(alltech_fit))^2))
sqrt(mean((alltech$salary - exp(fitted(alltech_fit_log)))^2))

summary(alltech_fit_log)


































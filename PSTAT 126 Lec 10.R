#Lecture 9/10 using log transformation
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

#data he provided: 
initech = read.csv("data/initech.csv")
initech = read.csv("/Users/danielzanger/2020_Aug_15_Data_Set_CSV_2.csv")
plot(salary ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at Initech, By Seniority")

initech_fit = lm(salary ~ years, data = initech)
summary(initech_fit)

plot(salary ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at Initech, By Seniority")
abline(initech_fit, col = "darkorange", lwd = 2)

plot(fitted(initech_fit), resid(initech_fit), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(initech_fit), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(initech_fit), col = "dodgerblue", lwd = 2)

initech_fit_log = lm(log(salary) ~ years, data = initech)

plot(log(salary) ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at Initech, By Seniority")
abline(initech_fit_log, col = "darkorange", lwd = 2)

plot(salary ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at Initech, By Seniority")
curve(exp(initech_fit_log$coef[1] + initech_fit_log$coef[2] * x),
      from = 0, to = 30, add = TRUE, col = "darkorange", lwd = 2)

plot(fitted(initech_fit_log), resid(initech_fit_log), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(initech_fit_log), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(initech_fit_log), col = "dodgerblue", lwd = 2)






#Autompg dataset
autompg = read.table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",
  quote = "\"",
  comment.char = "",
  stringsAsFactors = FALSE)
# give the dataframe headers
colnames(autompg) = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year", "origin", "name")
# remove missing data, which is stored as "?"
autompg = subset(autompg, autompg$hp != "?")
# remove the plymouth reliant, as it causes some issues
autompg = subset(autompg, autompg$name != "plymouth reliant")
# give the dataset row names, based on the engine, year and name
rownames(autompg) = paste(autompg$cyl, "cylinder", autompg$year, autompg$name)
# remove the variable for name, as well as origin
autompg = subset(autompg, select = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year"))
# change horsepower from character to numeric
autompg$hp = as.numeric(autompg$hp)


plot(mpg ~ hp, data = autompg, col = "dodgerblue", pch = 20, cex = 1.5)
mpg_hp = lm(mpg ~ hp, data = autompg)
abline(mpg_hp, col = "darkorange", lwd = 2)
#Not all points on regression line

#Fitted plot
plot(fitted(mpg_hp), resid(mpg_hp), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
#Not on zero, the variance fans out, not evenly distributed

par(mfrow = c(1, 2))
#Log Transformation, plot(mpg:Y, hp:x)
plot(log(mpg) ~ hp, data = autompg, col = "dodgerblue", pch = 20, cex = 1.5)
mpg_hp_log = lm(log(mpg) ~ hp, data = autompg)
abline(mpg_hp_log, col = "darkorange", lwd = 2)

#Fitted plot
plot(fitted(mpg_hp_log), resid(mpg_hp_log), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
#Non-constant variance


#Transformation of x too (since we did y above)
par(mfrow = c(1, 2))
plot(log(mpg) ~ log(hp), data = autompg, col = "dodgerblue", pch = 20, cex = 1.5)
mpg_hp_loglog = lm(log(mpg) ~ log(hp), data = autompg)
abline(mpg_hp_loglog, col = "darkorange", lwd = 2)

plot(fitted(mpg_hp_loglog), resid(mpg_hp_loglog), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
#Non-constant variance corrected, looks decent


#Polynomial transformations
marketing = read.csv("~/Desktop/PSTAT 126/marketmodel.csv")
plot(sales ~ advert, data = marketing,
     xlab = "Advert Spending (in $100,000)", 
     ylab = "Sales (in $100,000)", 
     pch = 20, cex =2)

marketmodel = lm(sales ~ advert, data = marketing)

plot(fitted(marketmodel), resid(marketmodel), col = "dodgerblue", pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h=0, lty = 2, col = "darkorange", lwd = 2)

mark_mod = lm(sales ~ advert, data = marketing)
summary(mark_mod)

mark_mod_poly2 = lm(sales ~ advert + I(advert ^ 2), data = marketing)
summary(mark_mod_poly2)

plot(fitted(mark_mod_poly2), resid(mark_mod_poly2), col = "dodgerblue", pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h=0, lty = 2, col = "darkorange", lwd = 2)

mark_mod_poly3 = lm(sales ~ advert + I(advert ^ 3), data = marketing)
summary(mark_mod_poly3)

plot(fitted(mark_mod_poly3), resid(mark_mod_poly3), col = "dodgerblue", pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h=0, lty = 2, col = "darkorange", lwd = 2)


plot(sales ~ advert, data = marketing,
     xlab = "Advert Spending (in $100,000)", ylab = "Sales (in $100,000)", 
     pch = 20, cex = 2)
#Original Linear plot:green
abline(mark_mod, lty = 2, col = "green", lwd = 2)
xplot = seq(0, 16, by = 0.01)
#Quadratic fit: Blue
lines(xplot, predict(mark_mod_poly2, newdata = data.frame(advert = xplot)),
      col = "blue", lwd = 2)
#Cubic Fit: Red
lines(xplot, predict(mark_mod_poly3, newdata = data.frame(advert = xplot)),
      col = "red", lty = 3, lwd = 2)


library(ggplot2)
ggplot(data = marketing, aes(x = advert, y = sales)) +
  stat_smooth(method = "lm", se = FALSE, color = "green", formula = y ~ x) +
  stat_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x + I(x^2)) +
  stat_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x +  I(x^2) + I(x^3)) +
  geom_point(colour = "black", size = 3)


#Simulated Data 
sim_quad = function(sample_size = 500) 
{
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x^2 + rnorm(n = sample_size, mean = 0, sd = 5)
  data.frame(x, y)
}
set.seed(314)
quad_data = sim_quad(sample_size = 200)
lin_fit = lm(y ~ x, data = quad_data)
summary(lin_fit)
#Model is Quadratic
plot(y ~ x, data = quad_data, col = "grey", pch = 20, cex = 1.5,
     main = "Simulated Quadratic Data")
abline(lin_fit, col = "darkorange", lwd = 2)

#Tell linear model to incorporate quadratic term
quad_fit = lm(y ~ x + I(x^2), data = quad_data)
summary(quad_data)

par(mfrow = c(1,2))

plot(fitted(quad_fit), resid(quad_fit),col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residual",main = "Fitted vs. Residuals")
abline(h = 0, col = "darkorange", lwd =2)
#symmetric distribution of pts. 

qqnorm(resid(quad_fit), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(quad_fit), col = "dodgerblue", lwd =2)


str(cars)
lin_fit = lm(speed ~ dist, data = cars)
summary(lin_fit)

#Model as a polynomial by adding quadratic term
quad_fit23 = lm(speed ~ dist + I(dist^2), data = cars)
summary(quad_fit23)

str(airquality)
lin_fit101 = lm(Ozone ~ Temp + I(Temp^2), data = airquality)
summary(lin_fit101)
#adj R squared rised but it didn't change much 

lin_fit101 = lm(Ozone ~ Temp, data = airquality)
summary(lin_fit101)





































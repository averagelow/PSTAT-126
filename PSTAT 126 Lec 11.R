#Box-Cox Transformation 

m <-lm(y ~ x)
summary(m)
library(tidyverse)

m <- lm(y ~ x)
boxcox(m)

set.seed(9)
x <-10:100
eps <-rnorm(length(x), sd =5)
y <- (x + eps)^ (-1/1.5)
#not linear

m <- lm(y ~ x)
summary(m)
#R^2 and AdjR^2 relatively low, may be indication of problem
#p-values are low, which is good
#quartiles aren't well balanced

library(broom)
augmented_m <-augment(m)

#Fitted vs. Residual
library(ggplot2)
ggplot(augmented_m, aes(x = .fitted, y = .resid)) + 
  geom_point()
#Looking for even distribution, not seeing that, not the best linear model


plot(m, which = 1) #which =1 plots only the fitted vs residuals

#Boxcox transformation
library(MASS)
bc <-boxcox(m)
#optimal y value pairs: lambda approx. -1.5, 95% CI: (-1.25,-1.75)

which.max(bc$y)

#Best lambda value
lambda <- bc$x[which.max(bc$y)]
lambda

z <- y ^lambda
m2 <-lm(z ~x)
summary(m2)
#R^2 and adjR^2 improved after boxcox transformation

m2 <-lm(I(y^lambda) ~x)

augmented_m2 <-augment(m2)
ggplot(augmented_m2, aes(x = .fitted, y = .resid)) + 
  geom_point()

#Savings dataset, Boxcox transformation
library(MASS)
library(faraway)

savings_model = lm(sr ~ ., data = savings)

boxcox(savings_model, plotit= TRUE)
#Since lambda approx. = 1, don't really need boxcox transformation

boxcox(savings_model, plotit= TRUE, lambda = seq(0.5, 1.5, by = 0.1))

plot(fitted(savings_model), resid(savings_model),col = "dodgerblue", 
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residual",
     main = "Fitted vs. Residuals")
abline(h = 0, col = "darkorange", lwd =2)



#Gala Dataset,Boxcox Transformation 
library(lmtest)
bptest(savings_model)

gala_model = lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)

plot(fitted(gala_model), resid(gala_model),col = "dodgerblue", 
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residual",
     main = "Fitted vs. Residuals")
abline(h = 0, col = "darkorange", lwd =2)

boxcox(gala_model, lambda = seq(-0.25, 0.75, by = 0.05), plotit = TRUE)
#Applied Boxcox Transformation for Y(Species)
gala_model_cox = lm((((Species^0.3)-1)/0.3) ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
plot(fitted(gala_model_cox), resid(gala_model_cox),col = "dodgerblue", 
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residual",
     main = "Fitted vs. Residuals")
abline(h = 0, col = "darkorange", lwd =2)

#Boxcox alltech dataset
boxcox(alltech_fit)

#Formal Test for Normality 
#if residuals are Normally distributed
#Shapiro-Wilk Test
#H_not = Normal dist 
#H_one = Non-Normality 

#Shapiro-Wilk Test, Normality 
set.seed(42)
shapiro.test(rnorm(25))

shapiro.test(rexp(25))
shapiro.test(resid(fit_1))
shapiro.test(resid(fit_2)) #Not normal, reject null hyp.
shapiro.test(resid(fit_3)) #Not normal, reject null hyp.

#Formal Test for Homoscedasticity 
#if residuals have constant variance
#Breusch-Pagan Test
#H_null = errors have constant variance (Homoscedasticity)
#H_alternative = errors have non-constant variance  (Heteroscedasticity)

#Breusch-Pagan Test, constant variance
library(lmtest)

bptest(fit_1) #High p-value, can accept null hyp
bptest(fit_2) #Low p-value, reject Null hyp, conclude alternative hyp
bptest(fit_3) #p- value is high, accept null hyp, violates linearity but not Homoscedasticity 



























































dataset1 = read.csv("~/Desktop/PSTAT 126/dataset_1.csv")
plot(Y ~ X, data = dataset1,
     xlab = "x", 
     ylab = "y", 
     pch = 20, cex =2)

dataset1model = lm(Y ~ X, data = dataset1)
summary(dataset1model)

plot(fitted(dataset1model), resid(dataset1model), col = "dodgerblue", 
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h=0, lty = 2, col = "darkorange", lwd = 2)

dataset1_mod = lm(Y ~ X, data = dataset1)
summary(dataset1_mod)
#The plot is a polynomial, regression line is not on the plot


#polynomial transform 
dataset1_mod1 = lm(Y ~ X + I(X ^ 2), data = dataset1)
summary(dataset1_mod1)

plot(fitted(dataset1_mod1), resid(dataset1_mod1), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h=0, lty = 2, col = "darkorange", lwd = 2)

dataset1_mod2 = lm(Y ~ X + I(X ^ 3), data = dataset1)
summary(dataset1_mod2)

plot(fitted(dataset1_mod2), resid(dataset1_mod2), col = "dodgerblue", 
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h=0, lty = 2, col = "darkorange", lwd = 2)

qqnorm(resid(dataset1_mod2), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(dataset1_mod2), col = "dodgerblue", lwd =2)

#Breusch-Pagan Test, constant variance
library(lmtest)
bptest(dataset1_mod2)

#Shapiro-Wilk Test, Normality 
shapiro.test(resid(dataset1_mod2)) #p-value > 0.05 




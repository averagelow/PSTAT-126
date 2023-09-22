dataset2 = read.csv("~/Desktop/PSTAT 126/dataset2.csv")
plot(Y ~ X, data = dataset2,
     xlab = "x", 
     ylab = "y", 
     pch = 20, cex =2)

dataset2model = lm(Y ~ X, data = dataset2)
summary(dataset2model)

plot(fitted(dataset2model), resid(dataset2model), col = "dodgerblue", 
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h=0, lty = 2, col = "darkorange", lwd = 2)

dataset2_mod = lm(Y ~ X, data = dataset2)
summary(dataset2_mod)


#polynomial transform 
dataset2_mod1 = lm(Y ~ X + I(X ^ 2), data = dataset2)
summary(dataset2_mod1)

plot(fitted(dataset2_mod1), resid(dataset2_mod1), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h=0, lty = 2, col = "darkorange", lwd = 2)

qqnorm(resid(dataset2_mod1), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(dataset2_mod1), col = "dodgerblue", lwd =2)


#Log transformation
#wasn't as good a fit as above)
dataset2_log = lm(log(Y) ~ X, data = dataset2)

plot(log(Y) ~ X, data = dataset2, col = "grey", pch = 20, cex = 1.5)
abline(dataset2_log, col = "darkorange", lwd =2)

plot(log(Y) ~ X, data = dataset2, col = "grey", pch = 20, cex = 1.5)
curve(exp(dataset2_log$coef[1] + dataset2_log$coef[2] * x),
      from = 0, to = 30, add = TRUE, col = "darkorange", lwd =2)

plot(fitted(dataset2_log), resid(dataset2_log), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted vs. Residuals")
abline(h = 0, col = "darkorange", lwd =2)

qqnorm(resid(dataset2_log), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(dataset2_log), col = "dodgerblue", lwd =2)

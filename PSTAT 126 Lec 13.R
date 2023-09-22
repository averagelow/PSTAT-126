#Econ dataset and applying polynomials

econ = read.csv("~/Desktop/PSTAT 126/econ_dataset.csv")

plot_econ_curve = function(model)
{
  plot(mpg ~ mph, data = econ, xlab = "Speed (Miles per Hour)", 
       ylab = "Fuel Efficiency (Miles per Gallon)", col = "dodgerblue", 
       pch = 20, cex = 2)
  xplot = seq(10, 75, by = 0.1)
  lines(xplot, predict(model, newdata = data.frame(mph = xplot)), 
        col = "darkorange", lwd = 2, lty = 1)
}
str(econ)
fit1 = lm(mpg ~ mph, data = econ)

par(mfrow = c(1,2))
plot_econ_curve(fit1)
plot(fitted(fit1), resid(fit1), xlab = "Fitted", ylab = "Residuals", 
     col = "dodgerblue", pch = 20, cex = 2)
abline(h = 0, col = "darkorange", lwd = 2)

#Residual lines not on data 
#Not the correct model
fit1 = lm(mpg ~ mph, data = econ)
summary(fit1)
#p-value too high 
#R^2 is too low
#1Q and 3Q Quartiles balanced, but median not close to 0


#Add polynomial terms for a better fit
#Adding 1 quadratic term

fit2 = lm(mpg ~ mph + I(mph^2), data = econ)
summary(fit2)
#2nd term is significant bc p-value for 2nd term is low
#p-value low for mph and mph^2
#R^2 is higher which is good
#F-statistic p-value low 
#Median closer to zero

par(mfrow = c(1,2))
plot_econ_curve(fit2)
plot(fitted(fit2), resid(fit2), xlab = "Fitted", ylab = "Residuals", 
     col = "dodgerblue", pch = 20, cex = 2)
abline(h = 0, col = "darkorange", lwd = 2)
#residual line is better but Fitted vs residuals plot could be improved
#2nd order term is significant


#Add cubic term to model
fit3 = lm(mpg ~ mph + I(mph^2) + I(mph^3), data = econ)
summary(fit3)
#p-value very high for 3rd term
#T-test shows 3rd order term didn't really help, it didn't change much

par(mfrow = c(1,2))
plot_econ_curve(fit3)
plot(fitted(fit3), resid(fit3), xlab = "Fitted", ylab = "Residuals", 
     col = "dodgerblue", pch = 20, cex = 2)
abline(h = 0, col = "darkorange", lwd = 2)



fit4 = lm(mpg ~ mph + I(mph^2) + I(mph^3) + I(mph^4), data = econ)
summary(fit4)
#Changed it, made varaibles significant 
#4th term low p-value (t-test) reject null hyp, sig term for regression
#Good R^2

par(mfrow = c(1,2))
plot_econ_curve(fit4)
plot(fitted(fit4), resid(fit4), xlab = "Fitted", ylab = "Residuals", 
     col = "dodgerblue", pch = 20, cex = 2)
abline(h = 0, col = "darkorange", lwd = 2)



fit6 = lm(mpg ~ mph + I(mph^2) + I(mph^3) + I(mph^4) + I(mph^5) + I(mph^6), data = econ)
summary(fit6)
#p-values going up
#p-values low, can reject null hyp
#R62 is good 
#Higher order terms good 

#significance of models
anova(fit4, fit6)
#p-value = 0.0842 > 0.05, Don't reject null hyp, terms 5,6 don't give much significance


par(mfrow = c(1,2))
plot_econ_curve(fit6)
plot(fitted(fit6), resid(fit6), xlab = "Fitted", ylab = "Residuals", 
     col = "dodgerblue", pch = 20, cex = 2)
abline(h = 0, col = "darkorange", lwd = 2)



fit8 = lm(mpg ~ mph + I(mph^2) + I(mph^3) + I(mph^4) + I(mph^5) + I(mph^6) + I(mph^7) + I(mph^8), data = econ)
summary(fit8)

par(mfrow = c(1,2))
plot_econ_curve(fit8)
plot(fitted(fit8), resid(fit8), xlab = "Fitted", ylab = "Residuals", 
     col = "dodgerblue", pch = 20, cex = 2)
abline(h = 0, col = "darkorange", lwd = 2)
#8th term not sig for t-test, F-test p-value is significant

#significance of models
anova(fit6, fit8)
#No significance for adding 7 and 8th terms, don't reject null based on p-value = 0.8682 > 0.05

coef(fit6)

#Shortens it so you don't have to write each term
fit6_alt = lm(mpg ~ poly(mph, 6), data = econ)
all.equal(fitted(fit6), fitted(fit6_alt))
coef(fit6_alt)

summary(fit6)

summary(fit6_alt)

fit6_alt2 = lm(mpg ~ poly(mph, 6, raw = TRUE), data = econ)
coef(fit6_alt2)















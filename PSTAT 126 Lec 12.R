#BPtest, Boxcox, Shapiro 

alltech = read.csv("~/Desktop/PSTAT 126/alltech.csv")

plot(salary ~ years, data = alltech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at alltech, By Seniority")

alltech_fit = lm(salary ~ years, data = alltech)
summary(alltech_fit)

#Constant variance?
bptest(alltech_fit)
#P-value is too low, not constat variance,reject null hyp.

#Normal Distr.?
#shapiro.test(alltech_fit)

#Alltech dataset Log transformation, to make constant variance
alltech_fit_log = lm(log(salary) ~ years, data = alltech)

plot(log(salary) ~ years, data = alltech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at alltech, By Seniority")
abline(alltech_fit_log, col = "darkorange", lwd =2)


#autompg dataset Log transformation, to make constant variance
#missing reading file and etc.
plot(mpg ~ hp, data = autompg, col = "dodgerblue", pch = 20, cex = 1.5)
mpg_hp = lm(mpg ~ hp, data = autompg)
abline(mpg_hp, col = "darkorange", lwd = 2)

#Fitted plot
plot(fitted(mpg_hp), resid(mpg_hp), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

par(mfrow = c(1, 2))
#Log Transformation, plot(mpg:Y, hp:x)
plot(log(mpg) ~ hp, data = autompg, col = "dodgerblue", pch = 20, cex = 1.5)
mpg_hp_log = lm(log(mpg) ~ hp, data = autompg)
abline(mpg_hp_log, col = "darkorange", lwd = 2)

#Constant variance?
bptest(mpg_hp) #Has a low p-value, reject null hyp., heterostechastic

#When you apply Log transform to both variables(x,y)
bptest(mpg_hp_loglog)


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

bptest(savings_model) #High p-value, constant varaince
shapiro.test(resid(savings_model)) #High p-value, residuals valid, residuals normally distr.



#ANOVA(Analysis of Variance) using Melatonin dataset
melatonin = read.csv("~/Desktop/PSTAT 126/melatonin.csv")
str(melatonin)

t.test(sleep ~ group, data = melatonin, var.equal = TRUE)
#(p-value = 0.05154) is below .1 sig. level (our alpha given) so reject null hyp.
#Means are different

#Calculate estimated means
t.test(sleep ~ group, data = melatonin, var.equal = TRUE)$estimate
#Control/Treatment means different so there is a difference

#Boxplot of mean estimates
boxplot(sleep ~ group, data = melatonin, col = 5:6)

library(faraway)
names(coagulation)

plot(coag ~ diet, data = coagulation, col = 2:5)

coag_aov = aov(coag ~ diet, data = coagulation)
coag_aov

summary(coag_aov)
#Reject null hyp, accept alternative, since not all the means are equal

plot(coag ~ diet, data = coagulation, col = 2:5)







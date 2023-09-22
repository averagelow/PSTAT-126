#Simulated data (same as Lec 16)
#Linear combination of M predictor variables
sim_logistic_data = function(sample_size = 25, beta_0 = -2, beta_1 = 3)
{
  x = rnorm(n = sample_size)
  eta = beta_0 + beta_1 * x
  p = 1/ (1 + exp(-eta))
  y = rbinom(n = sample_size, size = 1, prob = p)
  data.frame(y, x)
}

#y: binary (output is 0 or 1)
set.seed(1)
example_data = sim_logistic_data()
head(example_data)

#ordinary linear regression
fit_lm = lm(y ~ x, data = example_data)
#logistic regression
fit_glm = glm(y ~ x, data = example_data, family = binomial)

#Generalized linear models
glm(y ~ x, data = example_data)

fit_glm = glm(y ~ x, data = example_data, family = binomial(link = "logit"))

plot(y ~ x, data = example_data, pch = 20, ylab = "Estimated Probability", 
     main = "Ordinary vs Logistic Regression")
grid()
abline(fit_lm, col = "darkorange")
curve(predict(fit_glm, data.frame(x), type = "response"), 
      add = TRUE, col = "dodgerblue", lty = 2)
legend("topleft", c("Ordinary", "Logistic", "Data"), lty = c(1,2,0), 
       pch = c(NA, NA, 20), lwd = 2, col = c("darkorange", "dodgerblue", "black"))
#Logistical regression has exponential term so fits better


round(coef(fit_glm), 1)
#estimates coef: Beta_not, Beta_1, when you increase data pts (sample_size) increase accuracy








#Genralized linear Models:
#install.packages("bestglm")
library(bestglm)

data("SAheart")
head(SAheart)

chd_mod_ldl = glm(chd ~ ldl, data = SAheart, family = binomial)
plot(jitter(chd, factor = 0.1) ~ ldl, data = SAheart, pch = 20, 
     ylab = "Probability of CHD", xlab = "Low Density Lipoprotein Cholesterol")
grid()
curve(predict(chd_mod_ldl, data.frame(ldl = x), type = "response"), 
      add = TRUE, col = "dodgerblue", lty = 2)
#Makes sense between 0 and 1, CHD rises as ldl cholesterol does

coef(summary(chd_mod_ldl))

#Compare model above to model with all other variables available in dataset
#Z TEST, REJECT NULL_HYP
chd_mod_additive = glm(chd ~., data = SAheart, family = binomial)
-2 * as.numeric(logLik(chd_mod_ldl) - logLik(chd_mod_additive))
#Log likelihood: test statistic D 

#HYPOTHESIS TEST
anova(chd_mod_ldl, chd_mod_additive, test = "LRT")
#GET A VERY SMALL P-VALUE, prefer larger complex model, reject null hyp

#Stepwise regresison, to estimate better model
chd_mod_selected = step(chd_mod_additive, trace = 0)
coef(chd_mod_selected)

anova(chd_mod_selected, chd_mod_additive, test = "LRT")
#high p-value, accept null hyp














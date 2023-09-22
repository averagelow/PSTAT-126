#Logistical Regression: Bernoulli and applying a polynomial transformation
#Step-wise reg

#Linear combination of M predictor variables
sim_logistic_data = function(sample_size = 2500, beta_0 = -2, beta_1 = 3)
{
  x = rnorm(n = sample_size)
  eta = beta_0 + beta_1 * x
  p = 1/ (1 + exp(-eta))
  y = rbinom(n = sample_size, size = 1, prob = p)
  data.frame(y, x)
}
#y: binary 
set.seed(1)
example_data = sim_logistic_data()
head(example_data)

#ordinary linear regression
fit_lm = lm(y ~ x, data = example_data)
#logistic regression
fit_glm = glm(y ~ x, data = example_data, family = binomial)

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

set.seed(1)
example_data = sim_logistic_data(sample_size = 50, beta_0 =1, beta_1 = -4)

fit_glm = glm(y ~ x, data = example_data, family = binomial)
plot(y ~ x, data = example_data, pch = 20, ylab = "Estimated Probability", 
     main = "Logistic Regression, Decreasing Probability")
grid()
curve(predict(fit_glm, data.frame(x), type = "response"), 
      add = TRUE, col = "dodgerblue", lty = 2)
curve(boot::inv.logit(1 - 4 * x), add = TRUE, col = "darkorange", lty = 1)
legend("bottomleft", c("True Probability", "Estimated Probability", "Data"), lty = c(1,2,0), 
       pch = c(NA, NA, 20), lwd = 2, col = c("darkorange", "dodgerblue", "black"))
#True probability vs estimated probability


#Transformation (using polynomial) of logistical Regression 
sim_quadratic_logistic_data = function(sample_size = 25)
{
  x = rnorm(n = sample_size)
  eta = -1.5 + 0.5 * x + x ^ 2
  p = 1/ (1 + exp(-eta))
  y = rbinom(n = sample_size, size = 1, prob = p)
  data.frame(y, x)
}

set.seed(42)
example_data = sim_quadratic_logistic_data(sample_size = 50)
fit_glm = glm(y ~ x + I(x^2), data = example_data, family = binomial)
plot(y ~ x, data = example_data, pch = 20, ylab = "Estimated Probability", 
     main = "Logistic Regression, Quadratic Relationship")
grid()
curve(predict(fit_glm, data.frame(x), type = "response"), 
      add = TRUE, col = "dodgerblue", lty = 2)
curve(boot::inv.logit(-1.5 + 0.5 * x + x^2), add = TRUE, col = "darkorange", lty = 1)

legend("bottomleft", c("True Probability", "Estimated Probability", "Data"), lty = c(1,2,0), 
       pch = c(NA, NA, 20), lwd = 2, col = c("darkorange", "dodgerblue", "black"))


#Plant Growth dataset
head(PlantGrowth)

attach(PlantGrowth)
weight.factor <- cut(weight, 2, labels = c('Light', 'Heavy')) #binarize weights
plot(table(group, weight.factor))
#used cut function to create binary response variables: light and heavy plants

glm.1 <- glm(weight.factor ~ group, family = binomial)

#ANOVA TEST
anova(glm.1, test = 'LRT')
#p-value is low so distribution of weights vary with treatment given

#predict for heavy plants for each treatment
predict(glm.1, type = 'response')

detach(PlantGrowth)

data('Pima.te', package = 'MASS') # loads data
head(Pima.te)
#step-wise regression
glm.2 <- step(glm(type ~., data = Pima.te, family = binomial(link = 'probit')))
#optimal set of variables given

summary(glm.2)
#Verifies bc the variables have low p-values


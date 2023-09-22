head(EuStockMarkets)
View(EuStockMarkets)
str(EuStockMarkets)


?EuStockMarkets

stock_indices = lm(DAX ~ SMI + CAC, data =EuStockMarkets)

#Residual Standard Error 
summary(stock_indices)$sigma

#Confidence Interval 99%
confint(stock_indices, level = 0.99)
#Beta_one:SMI (Swiss), Beta_1:CAC (French)

#Simulated data: Multiple Regression Model
num_obs = 1000
beta_0 = 10
beta_1 = -5
beta_2 = 2
sigma = 3

set.seed(1)
epsilon =rnorm(n = num_obs, mean = 0, sd = sigma)

x_vals = runif(num_obs, 0, 10)
z_vals = runif(num_obs, 10, 20)
y_vals = beta_0 + beta_1 * x_vals + beta_2 * z_vals + epsilon

#Multiple regression model: 2 predictor variables
sim_fit2 = lm(y_vals ~ x_vals + z_vals)
coef(sim_fit2)

summary(sim_fit2)$sigma

#Didn't use:
y_vals
fitted(sim_fit)
residuals(sim_fit)
summary(sim_fit)$sigma









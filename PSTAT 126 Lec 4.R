#Simulated 
num_obs = 100
beta_0 = 10
beta_1 = -5
sigma = 3
set.seed(1)
epsilon = rnorm(n = num_obs, mean = 0, sd = sigma)

x_vals = runif(num_obs, 0, 10)
y_vals = beta_0 + beta_1 * x_vals*x_vals + epsilon

plot(y_vals ~ x_vals,
     pch = 20,
     cex = 2,
     col = "grey")
#It's curved,regression line not linear 

#Attempt to solve model as if regression line was linear 
sim_fit = lm(y_vals ~ I(x_vals^2))
summary(sim_fit)
#had this but he didn't use it 
sim_fit = lm(y_vals ~ x_vals)
summary(sim_fit)
coef(sim_fit)
abline(sim_fit, lwd = 3, col = "darkorange")


#Cars data set analyzing f test
stop_distance = lm(dist ~ speed, data = cars)
summary(stop_distance)
#we have the value:1 for one of the parameters
#50-2parameters = 48 Degrees of freedom 
#p-value is very small so simple linear regression model with both parameters, 
#Beta not and Beta one are both significant 

wait = lm(eruptions ~ waiting, data = faithful)
summary(wait)
#p-value very small, Regression model significant 

sepal= lm(Sepal.Length ~ Sepal.Width, data = iris)
summary(sepal)

#Multiple Regression 
?mtcars
head(mtcars)
str(mtcars)

mpg_model = lm(mpg ~ wt + hp, data = mtcars)
coef(mpg_model)
mpg_model = lm(mpg ~ wt + hp, data = mtcars)
summary(mpg_model)

#p-value is very small, so has a significant regression 
#Multiple R-squared between 0 and 1, significant regression 
#residuals balanced 1Q vs 3Q

#2 predictor values
mtcars_model = lm(wt ~ mpg + hp, data = mtcars)
summary(mtcars_model)

#3 predictor values
mtcars_model2 = lm(mpg ~ wt + hp + disp, data = mtcars)
summary(mtcars_model2)

#5 predictor values
mtcars_model3 = lm(wt ~ mpg + hp + qsec + disp + cyl, data = mtcars)
summary(mtcars_model3)

?EuStockMarkets

stock_indices = lm(DAX ~ SMI + CAC, data = EuStockMarkets)
summary(stock_indices)
#Significant regression













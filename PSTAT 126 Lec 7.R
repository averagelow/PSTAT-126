#R^2 vs Adjusted R^2
#Adj R^2 favored when doing multiple linear regression
#R^2 is 0 to 1 vs Adjusted R^2 (can be neg)


mtcars_model15= lm(mpg ~ wt + hp + disp, data = mtcars)
summary(mtcars_model15)
#R-squared:  0.7528,	Adj R-squared:  0.7446
#Similar

mtcars_model15= lm(mpg ~ wt + hp, data = mtcars)
summary(mtcars_model15)
#R-squared:  0.8268,	Adj R-squared:  0.8148
#Both increased

mtcars_model15= lm(mpg ~ wt + hp + disp, data = mtcars)
summary(mtcars_model15)
#R-squared:  0.8268,	Adj R-squared:  0.8083
#R^2: same, Adj R^2: decreased 

mtcars_model15= lm(mpg ~ wt + hp + disp + cyl, data = mtcars)
summary(mtcars_model15)
#R-squared:  0.8486,	Adjusted R-squared:  0.8262
#R^2: increased, Adj R^2: increased

mtcars_model15= lm(mpg ~ wt + hp + disp + cyl + drat, data = mtcars)
summary(mtcars_model15)
#R-squared:  0.8513,	Adj R-squared:  0.8227
#R^2: increased, Adj R^2: decreased 


#airquality data:
airq_model201 = lm(Temp ~ Ozone, data = airquality)
summary(airq_model201)
#Multiple R-squared:  0.4877,	Adjusted R-squared:  0.4832 
#Low values

airq_model201 = lm(Temp ~ Ozone + Solar.R, data = airquality)
summary(airq_model201)
#Multiple R-squared:  0.4909,	Adjusted R-squared:  0.4815
#R^2: increased, Adj. R^2: decreased

airq_model201 = lm(Temp ~ Ozone + Solar.R + Wind, data = airquality)
summary(airq_model201)
#Multiple R-squared:  0.4999,	Adjusted R-squared:  0.4858 
#R^2: increased, Adj. R^2: increased

#Categorical data have diff R^2 vs Adj R^2 so focus on numerical data

stock_indeces23 = lm(DAX ~ FTSE + SMI, data = EuStockMarkets)
summary(stock_indeces23)
#Multiple R-squared:  0.9842,	Adjusted R-squared:  0.9842
#same

stock_indeces23 = lm(DAX ~ FTSE + CAC, data = EuStockMarkets)
summary(stock_indeces23)
#Multiple R-squared:  0.9842,	Adjusted R-squared:  0.9842
#same

stock_indeces23 = lm(DAX ~  CAC, data = EuStockMarkets)
summary(stock_indeces23)
#Multiple R-squared:  0.9336,	Adjusted R-squared:  0.9336
#same





#Matrix multiplication for Beta_hat and coef. function

#Simulate data
set.seed(1337)
N = 100
M = 2

beta_0 = 5
beta_1 = -2
beta_2 = 6
sigma = 4

x0 = rep(1, N)
x1 = sample(seq(1,10, length = N))
x2 = sample(seq(1,10, length = N))
X = cbind(x0, x1, x2) #concatenates 3 vectors above
C = solve(t(X) %*% X)  #Compute inverse of matrix (uses matrix multiplication: %*%)

eps = rnorm(N, mean = 0, sd = sigma) 
y        = beta_0 + beta_1 * x1 + beta_2 * x2 + eps #multiple linear regression + random noise
sim_data = data.frame(x1, x2, y)

(beta_hat = C %*% t(X) %*% y)

coef(lm(y ~ x1 + x2, data = sim_data))

c(beta_0, beta_1, beta_2)


#Didn't use
y_hat = X %*% beta_hat
(s_e = sqrt(sum((y - y_hat)^2)/ (N - M-1)))

summary(lm(y ~ x1 + x2, data = sim_data))$sigma


























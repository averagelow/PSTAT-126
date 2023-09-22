#Plots cars data using speed vs distance
plot(dist ~ speed, data = cars,
     pch = 20,
     cex = 2,
     col = "grey")

stop_distance = lm(dist ~ speed, data = cars)
stop_distance

#Confidence Interval for Intercept, speed
confint(stop_distance, level = 0.95)

#Calculating beta_0_hat, beta_1_hat
x = cars$speed
y = cars$dist

#Lec 2 slide of Method of Least-Squares for SLR
#Sxy = numerator of beta_1_hat
#Sxx = denominator of beta_1_hat
Sxy =sum((x - mean(x)) * (y - mean(y)))
Sxx =sum((x - mean(x))^2)


beta_1_hat = Sxy / Sxx
beta_0_hat = mean(y) - beta_1_hat * mean(x)
c(beta_0_hat, beta_1_hat)

#using lm()model get the same results
stop_distance = lm(dist ~ speed, data = cars)
stop_distance

data(faithful)
#Calculating beta_0_hat, beta_1_hat (Didn't work)

x = eruptions$faithful
y = waiting$faithful

Sxy =sum((x - mean(x)) * (y - mean(y)))
Sxx =sum((x - mean(x))^2)

beta_1_hat = Sxy / Sxx
beta_0_hat = mean(y) - beta_1_hat * mean(x)
c(beta_0_hat, beta_1_hat)

stop_distance = lm(eruptions ~ waiting, data = faithful)
stop_distance


#Confidence Interval
str(cars)
dim(cars)
stop_distance = lm(dist ~ speed, data = cars)
stop_distance

stop_distance = lm(dist ~ speed, data = cars)
speedsabc = data.frame(speed = c(6,16))
predict(stop_distance, newdata = speedsabc,
        interval = c("prediction"), level = 0.95)


#Slide 26 simulate “artificial” data samples
num_obs = 21
beta_0 = 5
beta_1 = -2
sigma = 3
set.seed(1)
epsilon = rnorm(n = num_obs, mean = 0, sd = sigma)
x_vals = seq(from = 0, to = 10, length.out = num_obs)
y_vals = beta_0 + beta_1 * x_vals + epsilon
sim_fit = lm(y_vals ~ x_vals)
coef(sim_fit)
sim_slr = function(x, beta_0 = 5, beta_1 = -2, sigma = 3) {
  n = length(x)
  epsilon = rnorm(n, mean = 0, sd = sigma)
  y = beta_0 + beta_1 * x + epsilon
  data.frame(predictor = x, response = y)
}
set.seed(1)
sim_data = sim_slr(x = x_vals, beta_0 = 5, beta_1 = -2, sigma = 3)
sim_fit = lm(response ~ predictor, data = sim_data)
coef(sim_fit)
plot(response ~ predictor, data = sim_data,
     xlab = "Simulated Predictor Variable",
     ylab = "Simulated Response Variable",
     main = "Simulated Regression Data",
     pch = 20,
     cex = 2,
     col = "grey")
abline(sim_fit, lwd = 3, lty = 1, col = "darkorange")
abline(beta_0, beta_1, lwd = 3, lty = 2, col = "dodgerblue")
legend("topright", c("Estimate", "Ground Truth"), lty = c(1, 2), lwd = 2,
       col = c("darkorange", "dodgerblue"))

#SLR Model Summary Report Output 
stop_distance = lm(dist ~ speed, data = cars)
summary(stop_distance)

wait = lm(eruptions ~ waiting, data = faithful)
summary(wait)

#Not a good regression bc p value high and R^2 very low
sepal= lm(Sepal.Length ~ Sepal.Width, data = iris)
summary(sepal)


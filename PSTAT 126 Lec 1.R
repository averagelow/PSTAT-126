#PSTAT 126 Lec 1

data()

?cars 

View(cars)
str(cars)
head(cars)
dim(cars)
nrow(cars)
ncol(cars)

#Plots cars data using speed vs distance
plot(dist ~ speed, data = cars,
     pch = 20,
     cex = 2,
     col = "grey")
# Solves for linear regression model to compute parameters: 
# (Beta not, Beta 1) or (intercept, slope)
stop_distance = lm(dist ~ speed, data = cars)
stop_distance

#95 % confidence interval limits for intercept and speed
confin(stop_distance, level= 0.95)

#Compute sigma which is standard dev of noise term (3rd parameter) 
summary(stop_distance)$sigma

c(beta_0_hat, beta_1_hat )

plot(dist ~ speed, data = cars,
     pch = 20,
     cex = 2,
     col = "grey")
abline(stop_distance, lwd = 3, col = "darkorange")

names(stop_distance1)

coef(stop_dist_model)

#Value of computed Regression function, 
#estimate of model/underlying relationship
#value of mean function of all 50 obsv
fitted(stop_distance)

predict(stop_distance)

#Same things but using faithful data
?faithful
View(faithful)

plot(eruptions ~ waiting, data = faithful,
     pch = 20,
     cex = 2,
     col = "blue")

wait = lm(eruptions ~ waiting, data = faithful)
wait

summary(wait)$sigma

#Simulating using own data
num_obs = 10
beta_0  = 10
beta_1  = -5
sigma   = 3

set.seed(1)
epsilon = rnorm(n = num_obs, mean = 0, sd =sigma)

x_vals = runif(num_obs, min = 5, max = 10)(from = 0, to =10, length.out = num_obs)
y_vals = beta_0 + beta_1 * x_vals + epsilon

sim_fit = lm(y_vals ~ x_vals)
coef(sim_fit)

summary(sim_fit)$sigma

plot(y_vals ~ x_vals)
abline(sim_fit)

sim_slr = function(x, beta_0 = 10, beta_1 = 5, sigma = 1)
{
  n=length(x)
  
}








  
  
  
  
  
  



  


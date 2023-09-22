#Simulate data
#Simulating same data as Lec 7
set.seed(1337)
N = 10000
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
y = beta_0 + beta_1 * x1 + beta_2 * x2 + eps #multiple linear regression + random noise
sim_data = data.frame(x1, x2, y)

(beta_hat = C %*% t(X) %*% y)

coef(lm(y ~ x1 + x2, data = sim_data))

c(beta_0, beta_1, beta_2)



#Residual standard error squared or sigma hat squared on 
#sampling distribution for the Beta_m_hat slide
#sum of the squared of the residuals

y_hat = X %*% beta_hat
(s_e = sqrt(sum((y - y_hat) ^ 2 )/ (N - M-1)))

#Linear model command
summary(lm(y ~ x1 + x2, data = sim_data))$sigma

#Sampling distribution Beta_m_hat

num_sims = 10000
beta_hat_2 = rep(0, num_sims)
for(i in 1:num_sims)
{
  eps         = rnorm(N, mean = 0, sd = sigma )
  sim_data$y  = beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + eps
  fit         = lm(y ~x1 + x2, data = sim_data)
  beta_hat_2[i]= coef(fit)[3]
}
mean(beta_hat_2)

beta_2

var(beta_hat_2)

#Sampling distr. Beta_hat_m x* x transpose
sigma ^ 2 * C[3, 3] #row 3, col 3
sd(beta_hat_2)
sqrt(sigma ^ 2 * C[3, 3] )

#Histogram of samples in Beta_2_hat to show Normal Distr.
hist(beta_hat_2, prob = TRUE, breaks = 20,
     xlab = expression(hat(beta)[2]), main = "", border = "dodgerblue")
curve(dnorm(x, mean = beta_2, sd = sqrt(sigma ^ 2 * C[2+ 1, 2 +1])),
      col = "darkorange", add = TRUE, lwd = 3)

#Didn't use: 
sd_bh2 = sqrt(sigma ^ 2 * C[2+ 1, 2 +1])
#we expect these to be 0.68, 0.95, 0.997
mean(beta_2 - 1 * sd_bh2 < beta_hat_2 & beta_hat_2 < beta_2 + 1 * sd_bh2)
mean(beta_2 - 3 * sd_bh2 < beta_hat_2 & beta_hat_2 < beta_2 + 3 * sd_bh2)

null_mpg_model = lm(mpg ~ wt + disp, data = mtcars)
full_mpg_model = lm(mpg ~ wt + disp + hp, data = mtcars)
anova(null_mpg_model, full_mpg_model)
#P-value: 0.01097 so larger model justifies itself

#Lecture 8 Part 3(last)

sim_1 = function(sample_size = 500)
{
  x = runif(n = sample_size) * 5
  y = 3+ 5 * x + rnorm(n = sample_size, mean = 0, sd = 1)
  data.frame(x, y)
}

set.seed(42)
sim_data_1 = sim_1()
head(sim_data_1)

#Only works for 2 variables, since SLR it's ok
plot(y ~ x, data = sim_data_1, col = "grey", pch = 20,
     main = "Data from Model 1")
fit_1 = lm(y ~ x, data = sim_data_1)
abline(fit_1, col = "darkorange", lwd = 3)
#constant variance

#Residuals vs fitted plot
#works with multiple linear regression too
plot(fitted(fit_1), resid(fit_1), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)
#model accurate, homostecatisity reflected in behavior along line






  










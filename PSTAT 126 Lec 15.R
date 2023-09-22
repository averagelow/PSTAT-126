#AIC/BIC 
install.packages("leaps")

library(leaps)
all_hipcenter_mod = summary(regsubsets(hipcenter ~ ., data = seatpos))
#Finds optimal model for AIC criterion
all_hipcenter_mod$which

#Numerical, tells you the rss values for 8 models
all_hipcenter_mod$rss
#Tells you adjR^2 values for 8 models
all_hipcenter_mod$adjr2

#Tells you the best model: 3
(best_r2_ind = which.max(all_hipcenter_mod$adjr2))
#Extracts predictors of best model: Age, Ht, Leg
all_hipcenter_mod$which[best_r2_ind, ]

M = length(coef(hipcenter_mod)) -1
N = length(resid(hipcenter_mod))

#Model with the best AIC
hipcenter_mod_aic = N * log(all_hipcenter_mod$rss / N) + 2 * (2:(M+1))
best_aic_ind = which.min(hipcenter_mod_aic)
all_hipcenter_mod$which[best_aic_ind, ]

hipcenter_mod_best_aic = lm(hipcenter ~ Age + Ht + Leg, data = seatpos)

extractAIC(hipcenter_mod_best_aic)
extractAIC(hipcenter_mod_back_aic)
extractAIC(hipcenter_mod_forw_aic)
extractAIC(hipcenter_mod_both_aic)

plot(hipcenter_mod_aic ~ I(2:p), xlab = "p, number of parameters", 
     ylab = "AIC", col = "dodgerblue", pch = 20, type = "b", cex = 2, 
     main = "AIC vs Model Complexity")

hipcenter_mod_bic = N * log(all_hipcenter_mod$rss / N) + log(N) * (2:(M+1))
which.min(hipcenter_mod_bic)
#One variable needed for optimal model
#Shows which variable: Ht
all_hipcenter_mod$which[1,]

#Exhaustive search
hipcenter_mod_best_bic = lm(hipcenter ~ Ht, data = seatpos)
extractAIC(hipcenter_mod_best_bic, k = log(n))

extractAIC(hipcenter_mod_back_bic, k = log(n))


#Define intercept-only model, mpg:y-variable
intercept_only_model <- lm(mpg ~ 1, data = mtcars)

#Define total model
total_model <- lm(mpg ~ ., data = mtcars)

#perform stepwise regression: wt, cyl, hp
step(intercept_only_model, direction = 'both', scope = formula(total_model))
#Not always optimal, exhaustive is optimal but could be data intensive for large datasets









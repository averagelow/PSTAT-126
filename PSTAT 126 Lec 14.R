#Categorical variables to Numerical ones 
#Interaction terms
#Stepwise(Both): Backwards/Forward/ searches for better fit model

?mtcars

plot(mpg ~ hp, data = mtcars, cex = 2)

plot(mpg ~ hp, data = mtcars, col = am + 1, pch = am + 1, cex = 2)

legend("topright", c("Automatic", "Manual"), col = c(1,2), pch = c(1,2))

mpg_hp_slr = lm(mpg ~ hp, data = mtcars)

plot(mpg ~ hp, data = mtcars, col = am + 1, pch = am + 1, cex = 2)
abline(mpg_hp_slr, lwd = 3, col = "grey")
legend("topright", c("Automatic", "Manual"), col = c(1,2), pch = c(1,2))
#Manual(red triangles) lie over regression line while Automatic(black circles) lie under
#Underestimates fuel transmissions of manual
#overestimates fuel efficiency of transmissions of automatic

mpg_hp_add = lm(mpg ~ hp + am, data = mtcars)
coef(mpg_hp_add)
#Categorical variables to Numerical variables
#Manual = 1
#Automatic = 0

int_auto = coef(mpg_hp_add) [1]
int_manu = coef(mpg_hp_add) [1] + coef(mpg_hp_add) [3]

slope_auto = coef(mpg_hp_add)[2]
slope_manu = coef(mpg_hp_add)[2]

plot(mpg ~ hp, data = mtcars, col = am + 1, pch = am + 1, cex = 2)
abline(int_auto, slope_auto, col = 1, lty = 1, lwd = 2) #add line for auto
abline(int_manu, slope_manu, col = 2, lty = 2, lwd = 2) #add line for manual 
legend("topright", c("Automatic", "Manual"), col = c(1,2), pch = c(1,2))

summary(mpg_hp_add)$coefficients["am",]
#t-test: p-value is small, so am is significant

anova(mpg_hp_slr, mpg_hp_add)


str(iris)
class(iris$Species)
contrasts(iris$Species)
#Contrast tells you what the mapping is, converts categorical variables to numerical variables
fit803 = lm(Sepal.Length ~ Species, data = iris)
summary(fit803)
#Standard multiple regression on Species (converted dataset)


#MLR model with interaction term
?mtcars
#hp*wt: interaction term
interaction_mod = lm(mpg ~ hp * wt, data = mtcars)
summary(interaction_mod)
#p-value significant 
#Reject null hyp


library(faraway)
?seatpos

hipcenter_mod = lm(hipcenter ~ ., data =seatpos)
coef(hipcenter_mod)

#backward search, deletes a predictor until it finds a good fit
hipcenter_mod_back_aic = step(hipcenter_mod, direction = "backward")
#Ends with variables: HtShoes, Age, Leg

extractAIC(hipcenter_mod) #returns both p and AIC

n = length(resid(hipcenter_mod))
(p=length(coef(hipcenter_mod)))

n * log(mean(resid(hipcenter_mod) ^2)) + 2 * p

coef(hipcenter_mod_back_aic)

n = length(resid(hipcenter_mod))
hipcenter_mod_back_bic = step(hipcenter_mod, direction = "backward", k = log(n))


#For model with all 8 variables
summary(hipcenter_mod)$adj.r.squared

#For backward model
summary(hipcenter_mod_back_aic)$adj.r.squared

summary(hipcenter_mod_back_bic)$adj.r.squared


#Forward search, starts with no predictors, adds until reaches best fit
hipcenter_mod_start = lm(hipcenter ~ 1, data = seatpos)
hipcenter_mod_forw_aic = step(
  hipcenter_mod_start, 
  scope = hipcenter ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg, 
  direction = "forward")

#For model with all 8 variables
summary(hipcenter_mod)$adj.r.squared
#For Forward model
summary(hipcenter_mod_forw_aic)$adj.r.squared


#Stepwise search, backwards and forward 
hipcenter_mod_both_aic = step(
  hipcenter_mod_start, 
  scope = hipcenter ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg, 
  direction = "both")

#For model with all 8 variables
summary(hipcenter_mod)$adj.r.squared
#For Both: Forward and Backward models
summary(hipcenter_mod_both_aic)$adj.r.squared




library(lmtest)
model_145 = lm(DAX ~ CAC + SMI + FTSE, data = EuStockMarkets) #Y(response)~X(predictors)
dwtest(model_145) # Durbin-Watson Test for Autocorrelation
#very small p-value, there is autocorrelation


model_147 = lm(mpg ~ disp + wt + hp, data = mtcars) #Y(response)~X(predictors)
dwtest(model_147)
#p-value < 0.05 still has autocorrelation

model_148 = lm(mpg ~ cyl, data = mtcars)
dwtest(model_148)
#p-value> 0.05, reject null hyp, doesn't have autocorr.

#Goldfeld-Quandt Test for Homoscedasticity
model_150 = lm(mpg ~ cyl + disp, data = mtcars)
gqtest(model_147)
#p-value is low, reject null hyp, based on this test we have heteroscedasticity (nonconstant variance in model)



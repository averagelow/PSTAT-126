#Analyzing Summary, lm, and confint

#Generate 99% confidence interval for regressional parameters
stock_indeces22 = lm(DAX ~ SMI + CAC + FTSE, data = EuStockMarkets)
confint(stock_indeces22, level = 0.99)
#0.5: left-hand confidence interval
#99.5: right-hand confidence interval

stock_indeces22 = lm(DAX ~ SMI + CAC + FTSE, data = EuStockMarkets)
summary(stock_indeces22)

#look at F statistic for overall significance of multiple regression 
#p value is significant 2* 10^-16

#T-test for significance of individual variables
#T-Test: SMI, CAC: significant. FTSE: Not Significant

mtcars_model= lm(wt~ mpg + hp, data = mtcars)
summary(mtcars_model)
#look at F Test: p value is significant 1.529e^-09

#T-value: mpg significant with small t-value
#Horse power is not









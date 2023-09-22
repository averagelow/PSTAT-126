#Lec 17: 
#Genralized linear Models:
install.packages("bestglm")
library(bestglm)

data("SAheart")
head(SAheart)

chd_mod_ldl = glm(chd ~ ldl, data = SAheart, family = binomial)
plot(jitter(chd, factor = 0.1) ~ ldl, data = SAheart, pch = 20, 
     ylab = "Probability of CHD", xlab = "Low Density Lipoprotein Cholesterol")
grid()
curve(predict(chd_mod_ldl, data.frame(ldl = x), type = "response"), 
      add = TRUE, col = "dodgerblue", lty = 2)
#Makes sense between 0 and 1, CHD rises as ldl cholesterol does

coef(summary(chd_mod_ldl))

#Compare model above to model wityh all other variables available in dataset
#Z TEST, REJECT NULL_HYP
chd_mod_additive = glm(chd ~., data = SAheart, family = binomial)
-2 * as.numeric(logLik(chd_mod_ldl) - logLik(chd_mod_additive))
#Log likelihood: test statistic D 

#HYPOTHESIS TEST
anova(chd_mod_ldl, chd_mod_additive, test = "LRT")
#GET A VERY SMALL P-VALUE, prefer larger complex model, reject null hyp

#Stepwise regresison, to estimate better model
chd_mod_selected = step(chd_mod_additive, trace = 0)
coef(chd_mod_selected)

anova(chd_mod_selected, chd_mod_additive, test = "LRT")
#high p-value, accept null hyp



#Lec 18
#Confidence interval 99%
confint(chd_mod_selected, level = 0.99)

new_obs = data.frame(
  sbp = 148.0,
  tobacco = 5, 
  ldl = 12, 
  adiposity = 31.23,
  famhist = "Present",
  typea = 47,
  obesity = 28.50,
  alcohol = 23.89,
  age = 60
)

eta_hat = predict(chd_mod_selected, new_obs, se.fit = TRUE, type = "link")
eta_hat
#eta_hat = 1.579545

z_crit = round(qnorm(0.975), 2)
round(z_crit, 2)

boot::inv.logit(eta_hat$fit + c(-1,1) * z_crit * eta_hat$se.fit)
#95% confint for p(x), note in between 0 and 1

#using parameters chosen by stepwise regression
chd_mod_interaction = glm(chd ~alcohol + ldl + famhist + typea + age + ldl:famhist, 
                          data = SAheart, family = binomial)
summary(chd_mod_interaction) #linear reg.
#interaction term deemed sig for regression

#add another term: ldl:famhist + I(ldl^2) (polynomia)
chd_mod_int_squad = glm(chd ~ alcohol + ldl + famhist + typea + age  + ldl:famhist + I(ldl^2),
                        data = SAheart, family = binomial)
summary(chd_mod_int_squad )
#Not significant, p-value = 0.91617


#Use of logistic regression as a classifier
#meaning either 0 or 1, ex. sick or not
install.packages("kernlab")
library(kernlab)
data("spam")
tibble::as.tibble(spam)

tibble::as_tibble(spam)

?spam

set.seed(42)
#spam_idx = sample(nrow(spam), round (nrow(spam)/2))
spam_idx = sample(nrow(spam), 1000)
spam_trn = spam[spam_idx, ]
spam_tst = spam[-spam_idx, ]

#3 models using diff variables
fit_caps = glm(type ~ capitalTotal,
               data = spam_trn, family = binomial)
fit_selected = glm(type ~ edu + money + capitalTotal + charDollar,
                   data = spam_trn, family = binomial)
fit_additive = glm(type ~ .,
                     data = spam_trn, family = binomial)
coef(fit_selected)
#coef of model, our Beta_hats
#misclassified = 1
#classified correctly = 0


#More complex the model(more predictor variables), error goes down but may be overfitting errors
mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") !=spam_trn$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") !=spam_trn$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") !=spam_trn$type)
#More complex the model(more predictor variables), 
#error goes down but may be over-fitting 


library(boot)
set.seed(1)
cv.glm(spam_trn, fit_caps, K = 5,)$delta[1]
cv.glm(spam_trn, fit_selected, K = 5,)$delta[1]
cv.glm(spam_trn, fit_additive, K = 5,)$delta[1] #lowest missclassification 
#best model has lowest missclassification rate


#Confusion matrix
make_conf_mat = function(predicted, actual)
{
  table(predicted = predicted, actual = actual)
}

spam_tst_pred = ifelse(predict(fit_additive, spam_tst) > 0, "spam", "nonspam")
spam_tst_pred = ifelse(predict(fit_additive, spam_tst, type ="response") > 0.5,
                       "spam", "nonspam")

(conf_mat_50 = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type))
#predicted nonspam                  spam
#nonspam    2057(true neg)          157 (false neg.)
#spam        127(false pos.)        1260 (true pos.)
#want most of values on true neg. & true pos.

table(spam_tst$type) / nrow(spam_tst)
#a bit unbalanced

mean(spam_tst_pred == spam_tst$type)
#accuracy of classifier

mean(spam_tst_pred != spam_tst$type)
#missclassification rate of classifier, not bad
               
 

#Using backward operator with autompg dataset              
autompg=read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data", 
                   quote = "\"",
                   comment.char = "",
                   stringsAsFactors = FALSE)
colnames(autompg) = c("mpg", "cyl", "disp", "hp", "wt", "acc",
                      "year", "origin", "name")
autompg = subset(autompg, autompg$hp !="?")
autompg = subset(autompg, autompg$name !="plymouth reliant")
rownames(autompg) = paste(autompg$cyl, "cylinder", autompg$year, autompg$name)
autompg$hp = as.numeric(autompg$hp)
autompg$domestic = as.numeric(autompg$origin ==1)
autompg = autompg[autompg$cyl != 5,]
autompg = autompg[autompg$cyl != 3,]
autompg$cyl = as.factor(autompg$cyl)
autompg$domestic = as.factor(autompg$domestic)
autompg = subset(autompg, select = c("mpg", "cyl", "disp", "hp", "wt", "acc",
                                    "year", "domestic"))

autompg_big_mod = lm(
  log(mpg) ~ . ^2 + I(disp^2) + I(hp^2) + I(wt^2) + I(acc^2), 
  data = autompg)

autompg_mod_back_aic = step(autompg_big_mod, direction = "backward")            

#PS2 Q3 
#download packages for the operations 
library(lmtest) #for coeftest() and bptest().
library(broom) #for glance() and tidy()
library(PoEdata) #for PoE4 datasets
library(car) #for hccm() robust standard errors
library(sandwich)
library(knitr)
library(stargazer)
library(haven)
library(psych)
library(MASS)



setwd('/Users/c-lo/ECON3412/PS2')


q3 = read_dta('CPS2015.dta')
y = q3$ahe
x = q3$female
z = q3$bachelor
print("Part C ")
modelC <- lm(y~x, data=q3)
summary(modelC)
ans_a<- coeftest(modelC, vcov = vcovHC(modelC, "HC1"))
ans_a


#part g 
modelCM <- lm(y~x+z, data=q3)
summary(modelCM)
ans_g <- coeftest(modelCM, vcov = vcovHC(modelCM, "HC1"))
ans_g
#f-statistic 
ans_f <- waldtest(modelC, modelCM,  vcov = vcovHC(modelCM, "HC1"))
ans_f
#rsquared  robust linear 
rcoef <- c(17.82266 ,-4.24393, 9.85693)
tss = t(y-mean(y))%*%(y-mean(y))
y_hat = rcoef[1]+rcoef[2]*x + rcoef[3]*z
rss = t(y-y_hat)%*%(y-y_hat)
ess = tss-rss 
rr_sq = ess/tss
rr_sq

#part e and f 
print("Part f ")
beta_2 = cov(y,z)/var(z)
print("E[beta_1^hat]")
e_beta1 = cov(y,x)/var(x) + beta_2*(cov(z,x)/var(x))
e_beta1
print("Beta_1")
beta_1 = cov(y,x)/var(x)
beta_1
beta_11 = beta_1 * cov(x,x)/var(x)
ovb = beta_2*(cov(z,x)/var(x))
print("E[beta_1^hat] - beta11 = OVB")
print("OVB")
ovb
beta_0 = mean(y) - (beta_1*mean(x))- (beta_2*mean(z))


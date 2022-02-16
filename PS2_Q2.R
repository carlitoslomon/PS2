setwd("/Users/c-lo/ECON3412/PS2")

library(lmtest) #for coeftest() and bptest().
library(broom) #for glance() and tidy()
library(PoEdata) #for PoE4 datasets
library(car) #for hccm() robust standard errors
library(sandwich)
library(knitr)
library(stargazer)
library(readxl)
library(psych)

q_2 = read_excel("MD.xlsx", sheet=1)
print(q_2)
q_2
# Q2 part a 
summary(q_2)
describe(q_2)

#q2 part b 
q_2$ex1 <- (q_2$sp_tr)-(q_2$riskfree)
q_2$ex2 <- (q_2$hedge)-(q_2$riskfree)
x = q_2$ex1
y = q_2$ex2

#q2 part c 

install.packages("sandwich")
install.packages("lmtest")

m <- lm(y ~ x, data=q_2)
summary(m)
mcoef = m$coefficients


plot(q_2$ex1, q_2$ex2, type="p", xlab="hedge - risk free", ylab="sp_tr - risk free")
abline(m)

plot(q_2$ex2, rstandard(m), ylab="Standardized Residuals", xlab="y")
abline(h=0)
#performs a test on robust linear regression model
robust <- coeftest(m, vcov=vcovHC(m, type = "HC1"))
robust
### CI for Beta 0 
rcoef <- c(0.0021666,0.3691800 )
b0lbc = -0.002867 *1.965
b0lbc
b0ubc = 0.002867 *1.965
b0ubc
#ci for robust 
rb0lbc = -0.0026158*1.965
rb0lbc
rb0ubc = 0.0026158*1.965
rb0ubc
#performs a test on robust linear regression model
robust <- coeftest(m, vcov=vcovHC(m, type = "HC1"))
robust
rcoef <- c(0.0021666,0.3691800 )
#part f 
 t_t = (0.3691800 -1) /0.0817257
 t_t
p_val = 2*pt(q=7.718747, df = 79, lower.tail=FALSE)
p_val
#r^2 for robust linear 
tss = t(y-mean(y))%*%(y-mean(y))
y_hat = rcoef[1]+rcoef[2]*x
rss = t(y-y_hat)%*%(y-y_hat)
ess = tss-rss 
rr_sq = ess/tss
rr_sq
#for the RMSE and the R^2 in robust 
library(Metrics)
rrmse = rmse(y, y_hat)
rrmse
y_hat_reg = mcoef[1] + mcoef[2]*x
regrmse = rmse(y, y_hat_reg)
regrmse

#the alternatives are making the tables such that a comparison 
#can be made between a regular estimation and a rbust estimation 
kable(tidy(m), caption="Regular Standard Errors")



# annual data 
an_b0 = 12*mcoef[1]
an_b0
ran_b0 = 12*rcoef[1]
ran_b0

 



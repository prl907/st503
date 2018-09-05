

## ST503 Fall 2018 Discussion # 2 
## Group: A
## Author: Robin Baldeo, Yuxi Wang, Christopher Angrisani, Wenjin Liu                                    

##########################################################################################################################


set.seed(123)

x<-c(1:20)

y<- x + 2* rnorm(20)


#question 1

m1<-lm(y~x)

#scatter plot with fitted line
plot(x,y)
#abline(m1, col = 1,lty = 2 )
lines( fitted(m1), col=4, lty = 2) 
summary(m1)

m1$coefficients

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.9758 -1.2056 -0.0754  1.0388  3.4671 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.62019    0.92400   0.671    0.511    
# x            0.96791    0.07713  12.548 2.45e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.989 on 18 degrees of freedom
# Multiple R-squared:  0.8974,  Adjusted R-squared:  0.8917 
# F-statistic: 157.5 on 1 and 18 DF,  p-value: 2.45e-10


# Comment:
# I think this model is a good fit of the data. 
# The regression line seems to be a good fit and R2 is very close to 1 (0.8974146 )

##########################################################################################################################

#question 2

# new model
m2<-lm(y~x+I(x^2)+ I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7)+ I(x^8) + I(x^9))
summary(m2)$r.square

#overlay line onto scatter plot in question 1
lines(fitted(m2), col=2, lty = 2) 


# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.4621 -1.0502 -0.3103  1.2668  3.1165 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -5.954e-01  3.062e+01  -0.019    0.985
# x           -1.665e+00  5.922e+01  -0.028    0.978
# I(x^2)       2.850e+00  4.172e+01   0.068    0.947
# I(x^3)      -8.961e-01  1.474e+01  -0.061    0.953
# I(x^4)       1.336e-01  2.974e+00   0.045    0.965
# I(x^5)      -1.117e-02  3.634e-01  -0.031    0.976
# I(x^6)       5.757e-04  2.736e-02   0.021    0.984
# I(x^7)      -1.948e-05  1.239e-03  -0.016    0.988
# I(x^8)       4.255e-07  3.094e-05   0.014    0.989
# I(x^9)      -4.595e-09  3.270e-07  -0.014    0.989
# 
# Residual standard error: 2.409 on 10 degrees of freedom
# Multiple R-squared:  0.9164,  Adjusted R-squared:  0.8412 
# F-statistic: 12.18 on 9 and 10 DF,  p-value: 0.0002708


# Comments: 
# This model seems to be much better. 
# R2 has increased to 0.9164, which is expected with more explanatory variables being included in the regression equation, 
# but also the curve itself seems to better fit the data.

##########################################################################################################################


#question 3
#using the general least square forumla
#with r generic package

#create matrix p = 20 X n = 10
x1<-matrix(, nrow = length(y), ncol = 10)

for(p in 1:nrow(x1)){
  for(n in 1:ncol(x1)){
    #seeding matrix
    x1[p,n] = ifelse(n == 1, 1,p^(n-1))
  }
}

# using formula from class
manaul<-solve(x1%*%t(x1))%*%(t(x1)%*%y)

#det(x1%*%t(x1))


# Comment :
# Calculating the coefficients using the formula throws an error:

# "Error in solve.default(x1 %*% t(x1)) : 
#   system is computationally singular: reciprocal condition number = 1.5363e-27"
# After testing all parts of the equation, I found it failed to calculate inversion of (XTX). 

# Since some numbers of (XTX) are large, 
# the determinant may be overflowing due to scaling issues. Since the determinent of (XTX) is -7.081484e+110



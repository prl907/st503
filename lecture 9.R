##-----------------------------------------------------------------------------
## ST503 Fall 2018 -- R codes for lecture on 10/15/2018
## Author: Ryan Martin (rgmarti3@ncsu.edu)                                      
## Posted: 10/15/2018                                                             
##-----------------------------------------------------------------------------

# Generalized least squares

library(faraway)
data(globwarm)

o <- lm(nhtemp ~ wusa + jasper + westgreen + chesapeake + tornetrask + urals + mongolia + tasman, 
        data=globwarm)
summary(o)
e <- residuals(o)
n <- length(e)
cor(e[-1], e[-n])  

Sigma <- outer(1:n, 1:n, f <- function(i, j) 0.71099**abs(i - j))
Sigma.inv <- solve(Sigma)
X <- model.matrix(o)
y <- globwarm$nhtemp[!is.na(globwarm$nhtemp)]
beta.gls <- solve(t(X) %*% Sigma.inv %*% X) %*% t(X) %*% Sigma.inv %*% y
print(beta.gls)

S <- t(chol(Sigma))
S.inv <- solve(S)
X.prime <- S.inv %*% X
y.prime <- S.inv %*% y
oo <- lm(y.prime ~ . - 1, data=as.data.frame(X.prime))
summary(oo)


# Weighted least squares

data(cars)
o <- lm(dist ~ speed, data=cars)
summary(o)
plot(o$fitted.values, o$residuals); abline(h=0)

plot(o$fitted.values, abs(o$residuals))
abline(a=4, b=0.5, col=2)
oo <- lm(dist ~ speed, data=cars, weights=1 / (4 + 0.5 * speed))
summary(oo)
plot(dist ~ speed, data=cars)
abline(o)
abline(oo, col=2)
abline(a=-11.08, b=3.48, col=3)


# Testing lack of fit

data(corrosion)
plot(loss ~ Fe, data=corrosion)
o <- lm(loss ~ Fe, data=corrosion)
summary(o)
abline(o)
oo <- lm(loss ~ factor(Fe), data=corrosion)
summary(oo)
points(corrosion$Fe, oo$fitted.values, col=2)
anova(o, oo)

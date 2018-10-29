
##-----------------------------------------------------------------------------
## ST503 Fall 2018 -- R codes for lecture on 10/22/2018
## Author: Ryan Martin (rgmarti3@ncsu.edu)                                      
## Posted: 10/22/2018                                                             
##-----------------------------------------------------------------------------

library(faraway) 
library(MASS)
library(splines)

# Transforming the response, via Box-Cox, two examples

data(savings)
o.sav <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data=savings)
plot(o.sav$fitted.values, o.sav$residuals); abline(h=0)
qqnorm(o.sav$residuals); qqline(o.sav$residuals)

boxcox(o.sav, plotit=TRUE, lambda=seq(0.5, 1.5, by=0.1))

data(gala)
o.gal <- lm(Species ~ Area + Elevation + Scruz + Nearest + Adjacent, data=gala)
plot(o.gal$fitted.values, o.gal$residuals); abline(h=0)
qqnorm(o.gal$residuals); qqline(o.gal$residuals)

boxcox(o.gal, plotit=TRUE, lambda=seq(-0.25, 0.75, by=0.05))

o.gal.tr <- lm(I(Species**(1/3)) ~ Area + Elevation + Scruz + Nearest + Adjacent, data=gala)
plot(o.gal.tr$fitted.values, o.gal.tr$residuals); abline(h=0)
qqnorm(o.gal.tr$residuals); qqline(o.gal.tr$residuals)


# Polynomial regression: one x variable

o1 <- lm(sr ~ ddpi, data=savings)
summary(o1)
o2 <- lm(sr ~ ddpi + I(ddpi**2), data=savings)
summary(o2)
o3 <- lm(sr ~ ddpi + I(ddpi**2) + I(ddpi**3), data=savings)
summary(o3)

oo <- lm(sr ~ poly(ddpi, 4), data=savings)
summary(oo)


# Polynomial regression: two x variables

oo2 <- lm(sr ~ polym(pop15, ddpi, degree=2), data=savings)
summary(oo2)

z1 <- seq(20, 50, len=10)
z2 <- seq(0, 20, len=10)
grid <- expand.grid(pop15=z1, ddpi=z2)
out <- matrix(predict(oo2, grid), 10, 10)
persp(z1, z2, out, xlab="pop15", ylab="ddpi", zlab="sr", theta=45, ticktype="detailed", shade=0.25)


# Spline regression

f <- function(x) sin(2 * pi * x**3)**3
x <- seq(0, 1, by=0.01)
y <- f(x) + 0.1 * rnorm(length(x))
plot(x, y)
lines(x, f(x))

op4 <- lm(y ~ poly(x, 4))
op12 <- lm(y ~ poly(x, 12))
lines(x, op4$fitted.values, col=2)
lines(x, op12$fitted.values, col=3)

knots <- c(0, 0, 0, 0, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 1, 1, 1, 1)
bx <- splineDesign(knots, x)
o.splines <- lm(y ~ bx - 1)
lines(x, o.splines$fitted.values, col=4)



##-----------------------------------------------------------------------------
## ST503 Fall 2018 -- R codes for lecture on 10/24/2018
## Author: Ryan Martin (rgmarti3@ncsu.edu)                                      
## Posted: 10/24/2018                                                             
##-----------------------------------------------------------------------------

library(faraway) 

data(state)
?state
statedata <- data.frame(state.x77, row.names=state.abb)
o <- lm(Life.Exp ~ ., data=statedata)
summary(o)

# use "update(o, . ~ . - VARIABLE)" function to do backward selection


# enumerate all models, pick "best" based on AIC

library(leaps)
oo <- regsubsets(Life.Exp ~ ., data=statedata)
oo.sum <- summary(oo)
oo.sum$which
aic <- 50 * log(oo.sum$rss) + 2 * (2:8)
plot(aic ~ I(1:7), xlab="Number of predictors", ylab="AIC")
plot(2:8, oo.sum$adjr2, xlab="Number of parameters", ylab="Adjusted R2")

# there's also a "step" function that does stepwise (forward or backward) using AIC

step(o, direction="backward")
step(o, direction="both")




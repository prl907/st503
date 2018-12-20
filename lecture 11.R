
##-----------------------------------------------------------------------------
## ST503 Fall 2018 -- R codes for lecture on 10/31/2018
## Author: Ryan Martin (rgmarti3@ncsu.edu)                                      
## Posted: 10/31/2018                                                             
##-----------------------------------------------------------------------------

library(faraway) 

data(state)
?state
statedata <- data.frame(state.x77, row.names=state.abb)
head(statedata)
o.full <- lm(Life.Exp ~ ., data=statedata)
summary(o.full)

step(o.full, direction="backward")

# Ridge model fit

library(MASS)
X <- as.matrix(statedata[,-4])
y <- statedata$Life.Exp
lambda.seq <- seq(0, 20, len=25)
o.ridge <- lm.ridge(y ~ X, lambda=lambda.seq)
plot(lambda.seq, 0 * lambda.seq, type="n", xlab=expression(lambda), ylab=expression(hat(beta)), ylim=range(o.ridge$coef))
for(j in 1:7) lines(lambda.seq, o.ridge$coef[j,], col=j)
abline(v=lambda.seq[which.min(o.ridge$GCV)], lwd=2)
coef(o.ridge)[which.min(o.ridge$GCV),-1]

# Lasso model fit

#install.package("lars")
library(lars)
o.lars <- lars(X, y)
plot(o.lars)

set.seed(123)
cv <- cv.lars(X, y)
s <- cv$index[which.min(cv$cv)]; print(s)
predict(o.lars, s=s, type="coef", mode="fraction")



# Large-scale simulation example

n <- 100
p <- 200
signal <- 0.6 * 1:5
s.star <- length(signal)
beta.star <- c(signal, numeric(p - s.star))
S.star <- as.numeric(beta.star != 0)
X <- matrix(rnorm(n * p), nrow=n, ncol=p)
y <- as.numeric(X %*% beta.star) + rnorm(n)
o <- lars(X, y)
set.seed(123)
cv <- cv.lars(X, y, plot.it=FALSE)
beta.lasso <- coef(o, s=cv$index[which.min(cv$cv)], mode="fraction")
op <- par(mfrow=c(2,1))
plot(beta.lasso, type="h", ylim=range(beta.star))
plot(beta.star, type="h")
par(op)



##-----------------------------------------------------------------------------
## ST503 Fall 2018 -- R codes for lecture on 11/05/2018
## Author: Ryan Martin (rgmarti3@ncsu.edu)                                      
## Posted: 11/05/2018                                                             
##-----------------------------------------------------------------------------

library(faraway) 

data(orings)
head(orings)
?orings

plot(damage / 6 ~ temp, xlim=c(25, 85), ylim=c(0,1), xlab="Temp", ylab="Damage prob", data=orings)
o.lm <- lm(damage / 6 ~ temp, data=orings)
abline(o.lm, lty=2)

# Logistic regression 

o.glm <- glm(cbind(damage, 6 - damage) ~ temp, family=binomial, data=orings)
o.glm.sum <- summary(o.glm)
o.glm.sum
x <- 25:85
lines(x, ilogit(o.glm$coefficients[1] + o.glm$coefficients[2] * x))

# Probit regression

oo.glm <- glm(cbind(damage, 6 - damage) ~ temp, family=binomial(link=probit), data=orings)
summary(oo.glm)
lines(x, pnorm(oo.glm$coefficients[1] + oo.glm$coefficients[2] * x), col=2)


# Confidence interval for prob o-ring failure at 32 degrees

# Use monotonicity

x0 <- c(1, 32)
eta.hat <- sum(x0 * oo.glm$coefficients)
p.hat <- ilogit(eta.hat); p.hat
Sigma <- o.glm.sum$cov.unscaled
se <- sqrt( t(x0) %*% Sigma %*% x0 )
ilogit(c(eta.hat - 1.96 * se, eta.hat + 1.96 * se))

# Use delta theorem

dg <- function(z) -exp(-z) / (1 + exp(-z))**2
se.prob <- abs(dg(eta.hat)) * sqrt( t(x0) %*% Sigma %*% x0 )
c(p.hat - 1.96 * se.prob, p.hat + 1.96 * se.prob)


# Direct calculation of MLEs in probit regression via optim()

nloglik <- function(theta) {
  
  a <- theta[1]
  b <- theta[2]
  p <- pnorm(a + b * orings$temp)
  o <- sum(dbinom(orings$damage, size=6, prob=p, log=TRUE))
  return(-o)
  
}
o.optim <- optim(c(0,0), nloglik, method="BFGS")
print(o.optim)















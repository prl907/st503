
##-----------------------------------------------------------------------------
## ST503 Fall 2018 -- R codes for lecture on 11/14/2018
## Author: Ryan Martin (rgmarti3@ncsu.edu)                                      
## Posted: 11/14/2018                                                             
##-----------------------------------------------------------------------------


# Generate exam score by coffee consumption data

n <- 50

int.decaf <- 71
int.caff <- 70
slope.decaf <- 0.4
slope.caff <- 0.7

set.seed(77)
x <- runif(n, 0, 36)
z <- c(rep(0, n/2), rep(1, n/2))
caff <- as.logical(z)
y <- numeric(n)
eps <- rnorm(n, 0, 1)
y[!caff] <- int.decaf + slope.decaf * x[!caff] + eps[!caff]
y[caff] <- int.caff + slope.caff * x[caff] + eps[caff]
type <- ifelse(caff, "regular", "decaf")


# Fitting a linear model with only x

plot(y ~ x)
o.x <- lm(y ~ x)
abline(o.x)


# Fitting a linear model with only type

boxplot(y ~ type)
f.type <- factor(type)
t.test(y ~ f.type, var.equal=TRUE)
o.type <- lm(y ~ f.type)
summary(o.type)


# Fitting a linear model with type and x

plot(y ~ x, col=ifelse(type="regular", 1, 2))
o.both <- lm(y ~ x + f.type)
abline(a=o.both$coef[1], b=o.both$coef[2], col=2)
abline(a=sum(o.both$coef[c(1,3)]), b=o.both$coef[2])
model.matrix(o.both)


# Fitting a linear model with type, x, and their interaction

o.both.int <- lm(y ~ x + f.type + x:f.type)
summary(o.both.int)
plot(y ~ x, col=ifelse(type=="regular", 1, 2))
abline(a=o.both$coef[1], b=o.both$coef[2], col=2)
abline(a=sum(o.both.int$coef[c(1,3)]), b=sum(o.both.int$coef[c(2,4)]))
model.matrix(o.both.int)



##-----------------------------------------------------------------------------
## ST503 Fall 2018 -- R codes for lecture on 11/19/2018
## Author: Ryan Martin (rgmarti3@ncsu.edu)                                      
## Posted: 11/19/2018                                                             
##-----------------------------------------------------------------------------

library(faraway)

# Data with a single factor

data(coagulation)
head(coagulation)
?coagulation

plot(coag ~ diet, data=coagulation)
o <- lm(coag ~ diet, data=coagulation)
summary(o)

X <- model.matrix(o)
print(t(X) %*% X)
plot(o$residuals ~ o$fitted); abline(h=0)
plot(o$residuals ~ jitter(o$fitted)); abline(h=0)
qqnorm(o$residuals); qqline(o$residuals)


# Data with two factors (no replication)

data(composite)
print(composite)
?composite

with(composite, interaction.plot(laser, tape, strength))
with(composite, interaction.plot(tape, laser, strength))

o <- lm(strength ~ laser + tape, data=composite)
summary(o)
plot(o$residuals ~ jitter(o$fitted)); abline(h=0)
qqnorm(o$residuals); qqline(o$residuals)

oo <- lm(strength ~ laser * tape, data=composite)
summary(oo)


# Data with two factors (replication)

data(warpbreaks)
head(warpbreaks)
?warpbreaks

with(warpbreaks, interaction.plot(wool, tension, breaks))

o <- lm(breaks ~ wool * tension, data=warpbreaks)
summary(o)
plot(o$residuals ~ jitter(o$fitted)); abline(h=0)
qqnorm(o$residuals); qqline(o$residuals)

o.sqrt <- lm(I(sqrt(breaks)) ~ wool * tension, data=warpbreaks)
summary(o.sqrt)
plot(o.sqrt$residuals ~ jitter(o.sqrt$fitted)); abline(h=0)
qqnorm(residuals(o.sqrt)); qqline(residuals(o.sqrt))

oo.sqrt <- lm(I(sqrt(breaks)) ~ wool + tension, data=warpbreaks)
summary(oo.sqrt)
anova(oo.sqrt, o.sqrt)



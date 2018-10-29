library(faraway) 

data(teengamb)
o <- lm(gamble ~ sex + status + income + verbal, data=teengamb)
summary(o)
e <- residuals(o)
r <- rstandard(o)
id <- row.names(teengamb)

# partial regression/residual plots

#method 1
d <- residuals(lm(gamble ~ sex + status + verbal, data=teengamb))
g <- residuals(lm(income ~ sex + status + verbal, data=teengamb))
#our inital thought is that income should be included on it's own and no transformation
plot(g, d, xlab="income - effect of others", ylab="gamble - effect of others")

#method 2
p <- e + o$coefficients[4] * teengamb$income
plot(teengamb$income, p, xlab="income", ylab="partial resisuals")
termplot(o, partial.resid=TRUE, terms=3) # centers the income variable first

#both methods come to the same conclusion 

# checking for outliers
#extract the leverages
h <- hatvalues(o)
#half norm plot, 5 tell how many extreme points to label 
#id is the ro numbers
halfnorm(h, 5, labs=id, ylab="Leverages")
qqnorm(r); qqline(r)

# influential cases

D <- cooks.distance(o)
halfnorm(D, 5, labs=id, ylab="Cook's distance")
which.max(p)
which.max(r)

oo <- lm(gamble ~ sex + status + income + verbal, data=teengamb, subset=(D < max(D)))
summary(oo)
cbind(all=o$coefficients, removed=oo$coefficients)

# built-in lm diagnostic plots in R

plot(o)


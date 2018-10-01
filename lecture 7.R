head(savings)

o.sav<-lm(sr~pop15 + pop75 + dpi +ddpi, data = savings)
sav.hat<-o.sav$fitted.values

e.sav<-o.sav$residuals

#standarize the model

r.sav<-rstandard(o.sav)

# the plot has variations and there is no pattern to the spread. So I would conclude constant variance assumption is 
#satisfied

plot(sav.hat, e.sav, xlab = "fitted values", ylab = "residuals")
plot(sav.hat, r.sav, xlab = "fitted values", ylab = "residuals")

abline(h = 0)

#checking normality
#points are relly close tot he line the points at the end point are not peeling away, so this look good

hist(e.sav, freq= FALSE)
qqnorm(e.sav)
qqline(e.sav)

hist(r.sav, freq= FALSE)
qqnorm(r.sav)
qqline(r.sav)








#example #2

head(gala)

o.gal<-lm(Species ~Area + Elevation + Scruz + Nearest + Adjacent, data = gala)

gal.hat<-o.gal$fitted.values
e.gal<-o.gal$residuals
r.gal<-rstandard(o.gal)
#standarize the model

r.gal<-rstandard(o.sav)

# this case is where the consant residual is voilated, notice the funnel shape 
plot(gal.hat, e.gal, xlab = "fitted values", ylab = "residuals")

plot(gal.hat, r.gal, xlab = "fitted values", ylab = "residuals")
abline(h = 0)

hist(r.gal, freq = F)
#here the ends are peeling away , looks like long tail distrubtion, there is non normality
qqnorm(r.gal)
qqline(r.gal)





#refitting model with the sqrt

o.gal<-lm(sqrt(Species) ~Area + Elevation + Scruz + Nearest + Adjacent, data = gala)

gal.hat<-o.gal$fitted.values
e.gal<-o.gal$residuals
r.gal<-rstandard(o.gal)


#by transforming the y value the constant variable is verified
plot(gal.hat, e.gal, xlab = "fitted values", ylab = "residuals")

plot(gal.hat, r.gal, xlab = "fitted values", ylab = "residuals")
abline(h = 0)

hist(r.gal, freq = F)

#here the ends are peeling away , looks like long tail distrubtion, there is non normality
qqnorm(r.gal)
qqline(r.gal)



## Fixing non-normality -- skewed case

set.seed(7)
n <- 50
x <- 5 * runif(n)
y <- 10 + x + (rgamma(n, shape=2) - 2)
plot(x, y)
o <- lm(y ~ x)
abline(o, col=2)
r <- rstandard(o)
plot(x, r); abline(h=0)
hist(r, freq=FALSE, col="gray", border="white")
qqnorm(r); qqline(r)

o.log <- lm(log(y) ~ x)
r.log <- rstandard(o.log)
plot(x, r.log); abline(h=0)
qqnorm(r.log); qqline(r.log)



## Apparent correlation in errors
#test for independence
set.seed(7)
nn <- 25
xx <- seq(1, 5, length=nn)
yy <- xx**2 + rnorm(nn)
plot(xx, yy)
#the saured term is missign so the model looks dependent
oo <- lm(yy ~ xx)
rr <- rstandard(oo)
plot(rr); abline(h=0)





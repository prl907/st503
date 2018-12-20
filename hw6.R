library(faraway)
library(leaps)

library(MASS)


#Question 1 (Exercise 2.2) 
head(wbca)

#(A)
q1<- glm(Class~., data = wbca, family=binomial)
q1.s<- summary(q1)

#deviance
dev<- q1.s$deviance

#degree of freedom
df<- q1.s$df[2]

#determine the fit
pchisq(dev,df, lower.tail = F)


#Using just the deviance ### and the residual degree of freedom ### is not enough to determine the fit we must find the p-value from the chi square distribution. Since the p-value is greater than .05 we fail to reject the null and conclude that the binominal is satisfactory fit. 



#(B)
q2<- step(q1, direction = "backward")
q2.s<- summary(q2)

# the best model with the lowest aic is 
q2.s$call

#(c)
# using the parameters from question
#prediction
pi<- t(as.matrix(q2$coefficients))%*%as.matrix(c(1,1, 1, 3, 1, 1, 4, 1), nrow= 1)
pi
#confidience interval


#using the values from question for prediction
x0<- as.matrix(c(1,1, 1, 3, 1, 1, 4, 1), nrow= 1)

oo<- eval(q2.s$call)

eta.hat <- sum(x0 * oo$coefficients)
p.hat <- ilogit(eta.hat); p.hat
Sigma <- (summary(oo))$cov.unscaled
#calucating the se
se <- sqrt( t(x0) %*% Sigma %*% x0 )

#getting the 95% for the prediction 
ilogit(c(eta.hat - 1.96 * se, eta.hat + 1.96 * se))

#(D)

#function to do comparison
com<- function(o,p){
  v<- as.numeric(rep(0, length(o)))
  for(i in 1:length(o)){
    for(j in 1:length(p)){
      if(o[i] == p[j]){
        v[i]= 0
        break;
      }else{
        v[i]= 1
      }
    }
  }
  return(sum(v))
}

# malignant
pre.m<- which(oo$fitted.values<.5)
or.m<- which(wbca$Class==0)

com(o= or.m, p = pre.m)

#11 misclassied 

#benign
pre.b<- which(oo$fitted.values>.5)
or.b<- which(wbca$Class==1)

com(o= or.b, p = pre.b)

#9 misclassfied

###TODO###

#(E)

# malignant .9
pre.m<- which(oo$fitted.values<.9)
or.m<- which(wbca$Class==0)

com(o= or.m, p = pre.m)

#1 misclassied 

#benign .9
pre.b<- which(oo$fitted.values>.9)
or.b<- which(wbca$Class==1)

com(o= or.b, p = pre.b)

#16 misclassfied

###TODO###

#(F)

#getting the every 3rd index
r<- as.numeric(rep(0, nrow(wbca)))
o<- as.numeric(rep(0, nrow(wbca)))
for(i in 1:nrow(wbca)){
  if(i%%3 == 0){
    r[i]= i
  }else{
    o[i]= i
  }
}
#filter out the 0
r<- r[r>0]
o<- o[o>0]

test<- wbca[r,,drop= FALSE]

train<- wbca[o,,drop= FALSE]


#using training to determine best model with lowest aic

tr<- glm(Class~., data = train, family=binomial)
summary(tr)

tr.1<- step(tr, direction = "backward")
tr.2<- summary(tr.1)

# the best model with the lowest aic is 
tr_r<- eval(tr.2$call, tr)

#comparing models
re<- anova(tr_r, tr)

pchisq(re$Deviance[2],re$Df[2])

#since we have a large p-value greater than alpha then the simpler model is prefered. 

#using the test data to do prediction
predict(tr.1, newdata=test,type="response", se=T)


###TODO###

#question 2

#block the data into groups of 5 X 20 matrix
m<- matrix(discoveries,  ncol  =  20)

#variable to hold the sum of the blocks 
rate<- apply(m, 2, sum)
block<- apply(m, 2, length)
year<- seq(1:20)

mod_p1<- glm(rate~  year , family= poisson)
summary(mod_p1)

mod_p2<- glm(rate~ offset(log(block))+ year , family= poisson)
summary(mod_p2)

plot(mod_p2$fitted.values , ylim = c(0, 20), type= "l", ylab = "Predicted", xlab="Year blocks", main = "Discoveries over the years")

#TODO
#From the plot the rate of discoveries appears to be on the decrease over the years instead of constant. 


#question 3

#(A)
x <- seq(-5, 5, length=10)
y <- as.numeric(x > 0)

plot(x,y)
mod<- glm(y~x-1, family = binomial)

summary(mod)

xx <- seq(-5, 5, len=100)
lines(xx, ilogit(mod$coefficients[1]  * xx), col=2)



#ci for B
confint(mod)


##TODO##

#(B)


nloglik <- function(theta) {
  
  # a <- theta[1]
  b <- theta[1]
  p <- pnorm(b * x)
  o <- sum(dbinom(y, size=10, prob=p, log=TRUE))
  return(-o)
  
}
o.optim <- optim(0, nloglik, method="BFGS")

# #using the 
pl<- function(x){
  return((exp(1)^(o.optim$par*x))/(1 +(exp(1)^(o.optim$par*x))))
}

plot(pl, xlim = c(20,60))
 
#vertical line of b-hat
abline(v=mod$coefficients[1], col = "blue")
















#question 4


modp <- glm(Species ~ .,family=poisson, gala)

summary(modp)

plot(residuals(modp, type="pearson"))

modnb<- glm.nb(Species ~ ., gala)

summary(modnb)

points(residuals(modnb, type="pearson"), col = 2, pch = 2)

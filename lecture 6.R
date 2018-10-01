
library("faraway")

print(head(teengamb))
o.full <- lm(gamble ~ sex + status + income + verbal, data=teengamb)
summary(o.full)




# Bootstrap approximation of beta_1 sampling distribution

B <- 1000
beta1.star <- numeric(B)
e <- o.full$residuals
y.hat <- o.full$fitted.values
n <- length(e)
for(b in 1:B) {
  
  y.star <- y.hat + sample(e, size=n, replace=TRUE)
  oo <- lm(y.star ~ sex + status + income + verbal, data=teengamb)
  beta1.star[b] <- oo$coefficients[2]
  
}
hist(beta1.star, freq=FALSE, col="gray", border="white", xlab=expression(hat(beta)[1]), main="")
boot.ci <- quantile(beta1.star, c(0.025, 0.975))
points(x=boot.ci, y=c(0, 0), pch="|", col=2)
points(x=confint(o.full)[2,], y=c(0, 0), pch="|", col=4)

confint(o.full)




# Prediction 
# body fat example from Section 4.2 in Faraway

data(fat)

head(fat)

o <- lm(brozek ~ age + weight + height + neck + chest + abdom + hip + thigh + knee + ankle + biceps + forearm + wrist, data=fat)
summary(o)

#matrix used to fit the model
X <- model.matrix(o)
x.new <- apply(X, 2, median) # extracts stats for a "typical" man

y.new <- sum(x.new * coef(o))
y.new

predict(o, newdata=data.frame(t(x.new)))

predict(o, newdata=data.frame(t(x.new)), interval="confidence")
predict(o, newdata=data.frame(t(x.new)), interval="prediction")


# Another example to illustrate confidence/prediction intervals

y <- fat$brozek
x <- fat$abdom

plot(x, y)
oo <- lm(y ~ x)
abline(oo, lty=2)

ci <- predict(oo, interval="confidence")
pi <- predict(oo, interval="prediction")

lines(x, ci[,2], col=2)
lines(x, ci[,3], col=2)
lines(x, pi[,2], col=3)
lines(x, pi[,3], col=3)
















##-----------------------------------------------------------------------------
## ST503 Fall 2018 -- R codes for lecture on 09/19/2018
## Author: Ryan Martin (rgmarti3@ncsu.edu)                                      
## Posted: 09/19/2018                                                             
##-----------------------------------------------------------------------------

#install.packages("faraway")  # in case you didn't do this already!

library(faraway) 

# body fat example from Section 4.2 in Faraway

data(fat)

head(fat)

o <- lm(brozek ~ age + weight + height + neck + chest + abdom + hip + thigh + knee + ankle + biceps + forearm + wrist, data=fat)
summary(o)

X <- model.matrix(o)
x.new <- apply(X, 2, median) # extracts stats for a "typical" man

y.new <- sum(x.new * coef(o))
y.new

predict(o, newdata=data.frame(t(x.new)))

predict(o, newdata=data.frame(t(x.new)), interval="confidence")
predict(o, newdata=data.frame(t(x.new)), interval="prediction")


# Another example to illustrate confidence/prediction intervals

y <- fat$brozek
x <- fat$abdom

plot(x, y)
oo <- lm(y ~ x)
abline(oo, lty=2)

ci <- predict(oo, interval="confidence")
pi <- predict(oo, interval="prediction")

lines(x, ci[,2], col=2)
lines(x, ci[,3], col=2)
lines(x, pi[,2], col=3)
lines(x, pi[,3], col=3)





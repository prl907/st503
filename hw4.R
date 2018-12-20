
install.packages("MiKTeX")








install.packages("quantreg")
library("quantreg")
library("MASS")
library("faraway")
library("splines")

#question 8.1

#head(pipeline)

#a
mod<- lm(Lab~Field, data = pipeline)
summary(mod)

plot(mod$fitted.values, mod$residuals,xlab = "fitted", ylab = "residuals" ,col = "Red")
abline(h=0)

#yes, violation of constant variance


#b
i <- order(pipeline$Field) 
npipe <- pipeline[i,] 
ff <- gl(12,9)[-108] 
meanfield <- unlist(lapply(split(npipe$Field,ff),mean)) 
varlab <- unlist(lapply(split(npipe$Lab,ff),var))

#not removing the last obs from varlab and meanfield
modb<-lm(I(log(varlab))~I(log(meanfield)))
summary(modb)

#removing the last obs from both varlab and meanfield
modb<-lm(I(log(varlab[-length(varlab)]))~I(log(meanfield[-length(meanfield)])))
summary(modb)

#c

# applying log to both lab and field appears to correct the non cocnstant varaince voilation
modc<- lm(I(log(Lab))~I(log(Field)), data = pipeline)
summary(modc)

#plot of the log(lab)~log(field)
plot(modc$fitted.values, modc$residuals,xlab = "fitted", ylab = "residuals" ,col = "blue")
abline(h=0)


#question 8.3

head(salmonella)

mod_s<-lm(colonies~log(dose +1), data=salmonella)
summary(mod_s)

mod_c<-lm(colonies~factor(log(dose +1)), data=salmonella)
summary(mod_c)

anova(mod_s, mod_c)


#we favour the simpliermodel or ho because p-value is greater then .05. Therefore we fail to reject the null.

#question 8.5

#a
#least square method

ls<- lm(stack.loss~., data = stackloss)

summary(ls)


#Checking for influential points, then looking at the 5 most influential points
halfnorm(cooks.distance(ls), 5, names(cooks.distance(ls)))


#b
#lad method

lad<- rq(stack.loss~., data = stackloss)
summary(lad)


#c
#hubert method

hub<- rlm(stack.loss~., data = stackloss)
summary(hub)


#d additional part added

# lad weights
w_lad<- 1/abs(lad$residuals)

#hubert weight
w_hub<- hub$w

#plot of the weights of lad and hubert method
plot(c(1:21), w_lad, ylim = c(0, 1.4), pch = 19, col = "red", xlab = "points", ylab = "weight values", main= "Plot of the weights from lad and hubert method")
lines(c(1:21), w_hub, col="blue", type ="o", lty = 0 , pch = 22)
nam<- c("red", "blue")
legend("topright", legend = c("lad", "weight"),col=nam, pch=c(19,22))

#
#based on halfnorm point 21, 1 , 4, 3 and 17 appear to exert the top five influence on the model.
#removing these points

#
stackloss_n<- stackloss[c(-21,-1, -4, -3,-17),]

#

ls_n<- lm(stack.loss~., data = stackloss_n)
summary(ls_n)






#question 9.3

#head(ozone)

mod9<- lm(O3~temp+humidity+ibh, data= ozone)

summary(mod9)

plot(mod9$fitted.values, mod9$residuals)
abline(h=0, col= "red")

#the constant variance assumption appears violated


qqnorm(mod9$residuals)
qqline(mod9$residuals)

boxcox(mod9, plotit = T, lambda=seq(-1,1, by = .1))

#using lambda of of .25 from box cox in response transformation 
mod9_<- lm(I(O3**.25)~temp+humidity+ibh, data= ozone)

plot(mod9_$fitted.values, mod9$residuals)
abline(h=0, col= "red")


qqnorm(mod9_$residuals)
qqline(as.data.frame(mod9_$residuals))



#question 9.8

#a
plot(cars$speed, cars$dist, xlab = "speed", ylab = "distance", main = "distance vs speed")

#b

mod8<- lm(dist~speed, data = cars)
summary(mod8)
abline(mod8, col = "red")


#c

mod8q<- lm(dist~poly(speed, 2), data = cars)
summary(mod8q)
lines(cars$speed, mod8q$fitted.values, col= "blue")

#d

mod8s<- lm(I(sqrt(dist))~speed, data = cars)
summary(mod8s)
abline(mod8s, col = "green")

#e

plot(cars$speed, cars$dist, xlab = "speed", ylab = "distance", main = "distance vs speed")
lines(smooth.spline(cars$speed, cars$dist), col = "yellow" , lty = 5)


library("faraway")

head(teengamb)

o.full<-lm(gamble~ sex+status+ income+verbal, data = teengamb)


summary(o.full)


#this value is the f observed
summary(o.full)$fstatistic

#p-value is small then the hypothesis is implausable, the hypothesis being tested is all the x or beta are equal to zero


#using reduced model removing status and verbal becuase of the estimated std and stard error are low.
#set the values for status and verbal - 0
o.red<-lm(gamble~sex+ income, data= teengamb)
summary(o.red)
#here the variables sex and income are important. 


# ho: status= verbal = 0;
# ha: status= verbal <> 0;
#here we fit two different model
anova(o.red, o.full)

#here the p values is .3345 , here we say that the beta attached to status and verbal are zo, 
#So because the p-value is large we go with the reduced model


o.null<-lm(gamble~1, data=teengamb)
anova(o.null, o.full)

#the anova test shows that the varaibles status and verbal are not contributiong to the quality of the model 
#then we can remove them.


#working with ci
install.packages("ellipse")
library("ellipse")

ci<-confint(o.red)
ci
#vector c(2,3) tells r to pick the sex and income parameter
plot(ellipse(o.red, c(2,3)), type = "l")
abline(v=ci[2,], h=ci[3,], lty=2)

#permutation 
#if h1=..=hp-1=0, then the y are iid becuase the x are iid.



# Without normality, we could try a permutation version of the test.

FF <- numeric(1000)
for(i in 1:1000) {
  
  o <- lm(sample(gamble) ~ sex + status + income + verbal, data=teengamb)
  FF[i] <- summary(o)$fstatistic[1]
  
}

hist(FF, freq=FALSE, col="gray", border="white", xlab="F.perm",xlim= c(0, 20), main="")
curve(df(x, 5-1, length(teengamb$gamble)-5), add=TRUE)
abline(v= summary(o.full)$fstatistic[1], col = "blue")
# this f -values is way down on the x f.perm axis which makes it special meaning that 
# some of the x variables are important or reject the null hypothesis
print(summary(o.full)$fstatistic[1])
p.perm <- mean(FF > summary(o.full)$fstatistic[1])

t
print(p.perm)




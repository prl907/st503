## ST503 Fall 2018 Hw1
## Author: Robin Baldeo  
#######################################################################################################


library("faraway")
library("ellipse")

#question 1 (Exercise 3.1 on page 48{94 in LMR)
#full model
lmod<-lm(lpsa~lcavol+lweight+age+lbph+ svi+lcp+ gleason+ pgg45, data=prostate )

#summary
summary(lmod)

#a
#at the 90% Ci reject Ho because 0 is not within the interval
confint(lmod, "age", level=.90)

#at the 95% Ci fail to reject H0 becuause 0 is within interval
confint(lmod, "age")


#b
lmodR<-lm(lpsa~lcavol+lweight++ svi+lcp+ gleason+ pgg45, data = prostate)
ci<-confint(lmod)
#ellipse
plot(ellipse(lmod, c(4,5), level = .95), type = "l")
#CI
abline(v=ci[4,], h=ci[5,], lty=2)
#marking orgin with red lines
abline(h= 0, v = 0, col = "red")

anova(lmodR, lmod)

#this is a confidence region, here we fail to reject the joint h0:B_age= B_ibph=0 because the orgin falls
#within the ellipse. Therefore these varaibles can be removed from the model because of no siginificant contribution 
#to the quality of the model.


#c

n<- 2000

ttest<-numeric(n)

#looping n times 
for(i in 1:n){
  #storing all t values into the ttest vector
  ttest[i]= (summary(lm(lpsa~lcavol+lweight+sample(age)+lbph+ svi+lcp+ gleason+ pgg45, data=prostate )))$coefficients[4,3]
}


2 * mean(ttest<summary(lmod)$coefficients[4,3])

hist(ttest, freq = F, col = "wheat")
x<-ttest
curve(dt(x, length(x)), add=TRUE, col = "red")
#abline(v= summary(lmod)$coefficients[4,3], col = "blue", lty=2, lwd = 2)

#######################################################################################################

#question 3(Exercise 3.7 on page 50 of LMR)

#a

lmodD<-lm(Distance~RStr + LStr+ RFlex + LFlex, data= punting)
summary(lmodD)$coefficients
#none of the variables in the model are significant at 5% level. 

#b
f<- summary(lmodD)$fstat
f
#
1- pf(f[1], f[2], f[3])

#we reject the null in favour of the alternative where atleast one coefficient is non zero.

#c
lmodDR<-lm(Distance~I(RStr + LStr)+ RFlex + LFlex, data = punting)
summary(lmodDR)
anova(lmodDR, lmodD)

#based on the p-values >.05, we fail to reject h0=B_rstr=B_lstr = 0, where 
#the proposed simplification to the model  may be justified


#d

#using the full modelfrom a
ci <- confint(lmodD)
plot(ellipse(lmodD, c(2,3)), type ="l")
abline(h =ci[3,], v = ci[2,], lty = 2 )
abline(h= 0, v = 0, col = "red")

lmodDR1<-lm(Distance~ RFlex + LFlex, data = punting)
anova(lmodDR1, lmodD)

#######################################################################################################

#question 4
#A

set.seed(123)

nr <- nrow(teengamb)

teen <-teengamb

teen$y <- rnorm(nr)


model<-lm(y~sex + status + income+ verbal, data = teen)
summary(model)


#B

set.seed(12)

B <- 5000
x <- numeric(B)

for(b in 1:B) {
  teen <-teengamb
  teen$y <- rnorm(nr)
  o.full<-lm(y~sex + status + income+ verbal, data = teen)
  x[b]<- summary(o.full)$fstat[1]
 
}


#C

hist(x, freq=F, col= "wheat" )
curve(df(x, 4, 42), add = T, col = "red")

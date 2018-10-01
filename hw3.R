#library("faraway")

#question 4.1

#install.packages("rmarkdown")

lmod<-lm(lpsa~lcavol + lweight + age + lbph  + svi +  lcp + gleason + pgg45 , data= prostate)
summary(lmod)



#creating dataframe for prediction for a 65 year old patient 
df<- data.frame(matrix(c(1.44692, 3.62301, 65.00000, 0.30010, 0.00000, -0.79851, 7.00000,  15.00000), ncol= 8))
colnames(df)<-names(prostate) [names(prostate) !="lpsa" ] 


#individual predction for 65 year old person 
predict(lmod, df, interval="prediction")



#B
#creating dataframe for prediction 
# age 20
df<- data.frame(matrix(c(1.44692, 3.62301, 20.00000, 0.30010, 0.00000, -0.79851, 7.00000,  15.00000), ncol= 8))
colnames(df)<-names(prostate) [names(prostate) !="lpsa" ] 
df 



#individual prediction for a 20 year old
predict(lmod, df, interval="prediction")



#C
lmod
lred<-lm(lpsa~lcavol + lweight + svi  , data= prostate)

summary(lred)

anova(lred, lmod)

#the patial f- test shows a  p-value > .05, meaning that we should fail to reject the null. We see these variables in combinations 
#do not contribute to the overall quality of the model.Therefore the reduced model is the best model.

#retesting using the reduced model
df<- data.frame(matrix(c(1.44692, 3.62301,  0.00000), ncol= 3))
colnames(df)<-c("lcavol","lweight" ,"svi")

#individual prediction
predict(lred, df, interval="prediction")

#i would prefer the precition from the reduced model, where the interval is much narrower. 









#question 4.2



lmod2 <- lm(gamble ~ sex + status + income + verbal, data=teengamb)
summary(lmod2)

des<-model.matrix(lmod2)


#using data we find the mean

#a
male_a<-apply(des, 2, mean)
male_a[2]<-0

predict(lmod2, newdata=data.frame(t(male_a)), interval="confidence")



#b
male_b<-apply(des, 2, max)
male_b[2]<-0


predict(lmod2, newdata=data.frame(t(male_b)), interval="confidence")


#c


lmod2 <- lm(sqrt(gamble) ~ sex + status + income + verbal, data=teengamb)


new<- (predict(lmod2, newdata=data.frame(t(male_a)), interval="confidence"))^2

#d

fem_a<-data.frame(matrix(c(1,20,1,10), ncol = 4))
names(fem_a)<-c("sex", "status", "income", "verbal")



predict(lmod2, newdata=fem_a, interval="confidence")





#question 6.1
head(sat)
lmod6<- lm(total~expend+salary+ratio+ takers,data = sat)

summary(lmod6)

#a check constatnt varaiance assumption

hat<-lmod6$fitted.values

resi<-lmod6$residuals

plot(hat, resi, xlab="fitted", ylab="residuals")
abline(h = 0)

#standarize model

stmod<-rstandard(lmod6)

plot(hat, stmod, xlab="fitted", ylab="residuals")
abline(h = 0)

#b checking normality

#unstandard
hist(resi)
qqnorm(resi)
qqline(resi)


#standardized
hist(stmod)
qqnorm(stmod)
qqline(stmod)




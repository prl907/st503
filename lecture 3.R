library("faraway")
head(teengamb)


o<-lm(teengamb$gamble~teengamb$sex+teengamb$status+teengamb$income+teengamb$verbal)

summary(o)

nrow(teengamb)


# this the sigma squared
(summary(o)$sigma)^2


plot(teengamb$gamble, o$fitted.values)

lines(fitted(o), col=2, lty = 2) 
abline(a=0, b=1)
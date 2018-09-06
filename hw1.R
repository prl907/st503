
## ST503 Fall 2018 Hw1
## Author: Robin Baldeo  


library(faraway)


###########################################################################################################
#Exercise 2.4 in LMR, page 30.

#plotting the r-squared and the resedual error
plotD<-data.frame(residual=double(), points= double())

count <- 1
expvar <- ""
for(j in c("lcavol","lweight","svi","lbph", "age", "lcp" , "pgg45" , "gleason")){
  expvar = paste(expvar, j, sep = "+")
  q4=eval(parse(text=paste("lm(lpsa~" , substring(expvar, 2), ",data= prostate)")))
  plotD[count,1] = summary(q4)$sigma
  plotD[count,2] = count 
  print(paste("Model q4_",count,"(",substring(expvar, 2),")"," has residual standard error = ", summary(q4)$sigma, " and r squared = ", summary(q4)$r.squared, sep=""))
  count = count + 1
}

# [1] "Model q4_1(lcavol) has residual standard error = 0.787499423513711 and r squared = 0.539431908779019"
# [1] "Model q4_2(lcavol+lweight) has residual standard error = 0.750646932552003 and r squared = 0.585934512070213"
# [1] "Model q4_3(lcavol+lweight+svi) has residual standard error = 0.71680938995835 and r squared = 0.626440253553244"
# [1] "Model q4_4(lcavol+lweight+svi+lbph) has residual standard error = 0.710823197727069 and r squared = 0.636603479801418"
# [1] "Model q4_5(lcavol+lweight+svi+lbph+age) has residual standard error = 0.707305372441944 and r squared = 0.644102401261455"
# [1] "Model q4_6(lcavol+lweight+svi+lbph+age+lcp) has residual standard error = 0.710213512046953 and r squared = 0.645112974108872"
# [1] "Model q4_7(lcavol+lweight+svi+lbph+age+lcp+pgg45) has residual standard error = 0.704753265042738 and r squared = 0.65443165616093"
# [1] "Model q4_8(lcavol+lweight+svi+lbph+age+lcp+pgg45+gleason) has residual standard error = 0.708415511834863 and r squared = 0.654754085299708"



#plotting the trends in the two statistics
plot(plotD$points, plotD$residual)


###########################################################################################################

#question 2.6
#part a
head(cheddar)

q6_a<-lm(taste~Acetic+ H2S + Lactic, data = cheddar)

q6_a$coefficients
#regression coefficients:

# (Intercept)      Acetic         H2S      Lactic 
# -28.8767696   0.3277413   3.9118411  19.6705434 

#part b
cor(q6_a$fitted.values, cheddar$taste)^2
# 0.6517747

#the value above appears in the r squared of the regression summary
summary(q6_a)$r.squared
# 0.6517747



###########################################################################################################

#question 4
x <- c(0.032, 0.034, 0.214, 0.263, 0.275, 0.275, 0.450, 0.500, 0.500, 0.630, 0.800, 0.900, 
       0.900, 0.900, 0.900, 1.000, 1.100, 1.100, 1.400, 1.700, 2.000, 2.000, 2.000, 2.000)
y <- c(170, 290, -130,-70, -185, -220, 200, 290, 270, 200, 300, -30,  
       650, 150, 500, 920, 450, 500, 500, 960, 500, 850, 800, 1090)

q4w<-lm(y~x)

plot(x, y)

lines( fitted(q4w), col=1)

summary(q4w)$r.squared
#The r-square value of .623 seems to be a low number indicating the model might not be a good fit. 

q4w$coefficients
###########################################################################################################

## Problem 5. Assessing normality

n <- 15
op <- par(pty="m", mfrow=c(3, 3), mar=c(4.2, 4.2, 1, 1))
for(i in 1:9) {
  
  X <- rnorm(n)
  hist(X, freq=FALSE, col="gray", border="white", main=paste("model",i))
  curve(dnorm(x, mean(X), sd(X)), add=TRUE)
  
  
}
par(op)


#I think all of the plots looks somewhat normal, I think most of them are right or left skewed but overall I think all the plots are normally distributed. 








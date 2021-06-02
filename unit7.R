#1 
y <- c(22,23,18,9,14,20,21,18,16,19)
x <- c(8,10,7,2,4,6,7,6,4,6) 
  #1.1
output1 <- lm(y~x)
output1
  #1.2 
anova(output1)
  #1.3 
summary(output1)
beta1 =  1.717 
c(beta1+qt(0.025,8)*0.235,beta1+qt(0.975,8)*0.235)

c((8*2.541)/qchisq(0.975,8), (8*2.541)/qchisq(0.025,8))

beta0 = 1.717 
beta1 = 7.696
mean(x)
y.hat = beta0+(beta1*6)
y.hat 
CI <- c(y.hat+qt(0.025,8)*sqrt( 2.541*1/10),y.hat+qt(0.975,8)*sqrt( 2.541*1/10))
CI

#2 
x2 <- c(1,1.6,1.8,2,2.6,2.2,3,3,4.5,4,4,4)
y2 <- c(28,22,22,26,18,35,30,38,45,30,40,45)

output2 <- lm(y2~x2)
output2

anova(output2)
summary(output2)

beta2 =  5.763  
c(beta2+qt(0.025,10)*1.763,beta2+qt(0.975,10)*1.763)

c((10* 43.56)/qchisq(0.975,10), (10*43.56)/qchisq(0.025,10))

beta0 = 15.398 
beta1 = 5.763
mean(x2)
y.hat = beta0+(beta1*2.808333)
y.hat 
CI <- c(y.hat+qt(0.025,10)*sqrt( 43.56*1/12),y.hat+qt(0.975,10)*sqrt( 43.56*1/12))
CI

#3 
y3 <- c(15.4,10.2,19.8,9.7,12.3,10.8,16.4,15.8,21,17.5)
x3_1 <- c(3,7.5,4.5,4,5.5,7,5,3.5,3,6)
x3_2 <- c(45,55,43.5,50,47,49,53.5,61,45.5,55)

x <- cbind(x3_1,x3_2)

output3 <- lm(y3~x)
output3

anova(output3)
summary(output3)

beta1 <- -1.26685 
c(beta1+qt(0.025,7)*0.82497,beta1+qt(0.975,7)*0.82497)

beta2 <- -0.07159
c(beta2+qt(0.025,7)*0.23587,beta2+qt(0.975,7)*0.23587)

pred <- predict(lm(y3~x))
res <- residuals(lm(y3~x))
plot(pred,res,pch=16,xlab="Fitted Values",ylab="Residuals")
plot(pred,res)

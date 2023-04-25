rm(list=ls())
library(rio)
library(moments)
library(dplyr)
library(tidyverse)
library(car)
ccc=import("CreditCardTransactions.xlsx",which='Data')
attach(ccc)
colnames(ccc)=tolower(make.names(colnames(ccc)))
colnames(ccc) <- gsub(" ", "", colnames(ccc))
ccc$HighNetWorth<-ifelse(ccc$wealthtag=='HighNetWorth',1,0)
ccc$Affluent=ifelse(ccc$wealthtag=='Affluent',1,0)
ccc$EmergingAffluent=ifelse(ccc$wealthtag=='EmergingAffluent',1,0)
ccc$MassMarket=ifelse(ccc$wealthtag=='MassMarket',1,0)
ccc$spendcategory<-as.factor(ccc$spendcategory)
str(ccc)
ccc$cardtype <- as.factor(ccc$cardtype)
str(cc)
levels(cc$cardtype)
par(mfrow=c(2,2))
#Checking Normality Of Dpendet Variables
hist(ccc$transamount)
hist(ccc$transcount)
hist(log(ccc$transamount))
hist(log(ccc$transcount))
missing_values<-colSums(is.na(ccc))     
missing_values
transamount=na.omit(ccc$transamount)
unique(is.na(transamount)==TRUE)
hist(ccc$transcount*ccc$transcount)

#Checking Correlation Between Transamount and Transcount
cc_cont=ccc[c("transamount","transcount")]
names(cc_cont)
cc_cont
library(corrplot)
rk=cor(cc_cont)
corrplot(rk,method="number")

#Model1 for transaction amount, since our DV is not normally sdistributed and is right skewed i am using exponential transform
#Can't use transcount as Independent Variable for transamount as DV as there is very high corrlation between both of them
ta1=lm(log(transamount+1)~cardtype+revolvingindicator+
         spendcategory+Affluent+HighNetWorth+MassMarket+EmergingAffluent,data=ccc)
summary(ta1)
par(mar=c(2, 2, 2, 2))
par(mfrow=c(2,2))
#Linearity
plot(log(transamount+1),ta1$fitted.values,
     pch=19,main="Actuals v. Fitteds, transamount",cex=0.25,cex.main=0.9)
abline(0,1,lwd=3,col="red")
#Normality
hist(ta1$residuals,col="red")
qqnorm(ta1$residuals,pch=19,
       main="Normality Plot")
qqline(ta1$residuals,lwd=3,col="red")

#Equality Of Variance
plot(ta1$fitted.values,rstandard(ta1),
     pch=19,main="Equality of Variances")
abline(0,0,lwd=3,col="red")
par(mfrow=c(1,1))

#Independence
library(lmtest)
dwtest(ta1)

summary(ta1)

#Model 2
ta2=lm(log(transamount+1)~cardtype+revolvingindicator+
         spendcategory+Affluent+HighNetWorth+MassMarket+EmergingAffluent+ccc$transcount,data=ccc)
summary(ta2)
par(mfrow=c(2,2))
plot(ta2)

#Linearity
plot(log(transamount+1),ta2$fitted.values,
     pch=19,main="Actuals v. Fitteds, transamount",cex=0.25,cex.main=0.9)
abline(0,1,lwd=3,col="red")
#Normality
hist(ta2$residuals,col="red")
qqnorm(ta2$residuals,pch=19,
       main="Normality Plot")
qqline(ta2$residuals,lwd=3,col="red")

#Equality Of Variance
plot(ta2$fitted.values,rstandard(ta1),
     pch=19,main="Equality of Variances")
abline(0,0,lwd=3,col="red")

library(stargazer)
stargazer(ta1, ta2,type='text', single.row=TRUE)


#Models For Transcount
hist(log(ccc$transcount))
hist(log(ccc$transcount*ccc$transcount))
#Creating a new variable which is total amount spent per transaction
ccc$tot=ccc$transamount*ccc$transcount

tc1=lm(log(ccc$transcount*ccc$transcount)~cardtype+revolvingindicator+
             spendcategory+Affluent+HighNetWorth+MassMarket+EmergingAffluent,data=ccc)

summary(tc1)

par(mfrow=c(2,2))
#Linearity
plot(log(ccc$transcount*ccc$transcount),tc1$fitted.values,
     pch=19,main="Actuals v. Fitteds, transcount",cex=0.25,cex.main=0.9)
abline(0,1,lwd=3,col="red")

#Normality
hist(tc1$residuals,col="red")
qqnorm(tc1$residuals,pch=19,
       main="Normality Plot")
qqline(tc1$residuals,lwd=3,col="red")

#Equality Of Variance
plot(tc1$fitted.values,rstandard(tc1),
     pch=19,main="Equality of Variances")
abline(0,0,lwd=3,col="red")


#Independence
dwtest(tc1)

# #Model 2 for transcount
# Including new variable which is total money spend per line item per transaction and just using exponential transform
tc2=lm(log(ccc$transcount)~cardtype+revolvingindicator+
         spendcategory+Affluent+HighNetWorth+MassMarket+ccc$EmergingAffluent+ccc$tot,data=ccc)
summary(tc2)
par(mfrow=c(2,2))
plot(tc2)

#Linearity
plot(log(ccc$transcount),tc2$fitted.values,
     pch=19,main="Actuals v. Fitteds, transcount2",cex=0.25,cex.main=0.9)
abline(0,1,lwd=3,col="red")

#Normality
hist(tc2$residuals,col="red")
qqnorm(tc2$residuals,pch=19,
       main="Normality Plot")
qqline(tc2$residuals,lwd=3,col="red")

#Equality Of Variance
plot(tc2$fitted.values,rstandard(tc2),
     pch=19,main="Equality of Variances")
abline(0,0,lwd=3,col="red")

stargazer(tc1,tc2,type="text",single.row=TRUE,header=FALSE,linebreaks = 1)
par(mfrow=c(1,1))
par(cex.axis = 0.7,las=2)
par(plt = c(0.2, 0.9, 0.3, 0.9))
boxplot(ccc$tot~ccc$cardtype+ccc$wealthtag,xlab=" ",col="red")
ccc$tot
which.max(ccc$tot)-which.min(ccc$tot)
control=lm(log(ccc$transamount)~ccc$spendcategory)
summary(control)
ta5=lm(log(transamount+1)~wealthtag+cardtype+revolvingindicator+
         spendcategory,data=ccc)
summary(ta5)

stargazer(ta1, tc1,type='text', single.row=TRUE)



rm(list=ls())

setwd("C:/MSTC_BD/R/WORKING/DATASETS/BOOK")

Advertising=read.csv("Advertising.csv")
fix(Advertising)

pairs(~ TV + Radio + Newspaper + Sales, Advertising) 

lm.fit=lm(Sales~TV,data=Advertising)
#Simple Linear Regression: Sales ~ TV
attach(Advertising)
lm.fit=lm(Sales~TV)

summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

# Plotting the Regression Line
plot(TV,Sales)
abline(lm.fit,lwd=3,col=2)

# Multiple Linear Regression
# lm.fit=lm(Sales~TV+Radio+Newspaper,data=Advertising)

# ~.-X = all variables but X (as X is not a variable  . it means use ALL !)

lm.fit=lm(Sales~.-X,data=Advertising)
summary(lm.fit)



#Go back to a Simple Linear Regression: Sales ~ TV
lm.fit=lm(Sales~TV)


# R^2 and correlation with predicted values Corr(Y,Y_hat)
summary(lm.fit)
Pred_Sales=predict(lm.fit,data.frame(TV),interval="prediction")
cor(Sales,Pred_Sales)^2

# As it is a simple prediction Check it is also Cor(X,Y) 
cor(Sales,TV)^2

# Other metrics
plot(abs(Sales-Pred_Sales[,1]),xlab="Sales",type='p',pch=12,col='blue')

MAE=sum(abs(Sales-Pred_Sales[,1]))/length(Sales)


# CONFIDENCE and PREDICTION intervals
# 95% confidence interval associated with each TV value
predict(lm.fit,data.frame(TV=c(10,100,200)),interval="confidence")

# prediction interval for predicted Sales
predict(lm.fit,data.frame(TV=c(10,100,200)),interval="prediction")



# INTERACTION TERMS
lm.fit=lm(Sales~TV*Radio,data=Advertising)
summary(lm.fit)

# NON-LINEARITIES
# Residual plots are a useful graphical tool for identifying non-linearity 
# Plotting residualts
plot(predict(lm.fit),residuals(lm.fit),xlab='Fitted values',ylab='Residuals',main='Residual Plot for Advertisement')


# OUTLIERS
# Plotting studentized residuals
plot(predict(lm.fit),rstudent(lm.fit),xlab='Fitted values',ylab='Studentized Residuals',main='Studentized Residual Plot for Advertisement')


# par(mfrow=c(2,2)) divides plot region in 2 x 2
par(mfrow=c(2,2)) 
plot(lm.fit) 


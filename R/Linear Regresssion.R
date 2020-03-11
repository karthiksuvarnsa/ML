library(MASS)
library(ISLR)
### Simple linear regression
## lets see boston data from the islr dataset
View(Boston)
# to view the names of the columns
names(Boston)
# to know more about boston columns
?Boston

# now let us plot a graph between medv and lstat
plot(medv~lstat,Boston)
# fitting a linear model
fit1=lm(medv~lstat,data=Boston)
# gives breif summary
fit1
# to get detailed summary
summary(fit1)
# plotting the linear regression line on the plot
abline(fit1,col="blue")
# to check all the variables in the fit
names(fit1)
# to get confidence interval of the fitted coefficients
confint(fit1)
# predicting the medv for given lstat values along with confidence intervals
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")
### Multiple linear regression
fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)
# fitting all available variables
fit3=lm(medv~.,Boston)
summary(fit3)
# setting multiple subplots in plot area
par(mfrow=c(2,2))
# to plot all the fit plots
plot(fit3)
# removing age and indus which are not significant
fit4=update(fit3,~.-age-indus)
summary(fit4)
### Nonlinear terms and Interactions
fit5=lm(medv~lstat*age,Boston)
summary(fit5)
fit6=lm(medv~lstat +I(lstat^2),Boston); summary(fit6)
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20)
fit7=lm(medv~poly(lstat,4))
points(lstat,fitted(fit7),col="blue",pch=20)
plot(1:20,1:20,pch=1:20,cex=2)
###Qualitative predictors
# to open an editor
fix(Carseats)
names(Carseats)
summary(Carseats)
fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit1)
## how r will code the variable if put in linear variable
contrasts(Carseats$ShelveLoc)
###Writing R functions
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)

# ... used to give unnamed parameters which can be passed later
regplot=function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)





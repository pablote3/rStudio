#working directory
setwd("~/git/rStudio/r-samples")
getwd()

#reading data into dataframe
grades=read.csv(file=file.choose())
grades=read.csv("grades.csv")

#dataframe commands
grades        #show entire data set
head(grades)  #show first 6 rows
tail(grades)  #show last 6 rows
str(grades)   #show variable names and types
names(grades) #show variable names
ls()          #show list of objects available

#descriptive statistics
mean.Grade=mean(grades$Grade)        #computes mean of variable x
median.Income=median(grades$Income)  #computes median of variable x
sd(grades$Income)                    #computes standard deviation of variable x
IQR(grades$Income)                   #computes IRQ of variable x
cor(grades$Income,grades$Grade)      #computes correlation coefficient
cor.test(grades$Income,grades$Grade) #test plus CI for rho
summary(grades$Income)               #computes 5 number summary and mean of variable x
summary(grades)

#graphical displays
hist(grades$Grade)                   #creates a histogram of variable x
boxplot(grades$Grade)                #creates a boxplot for variable x
boxplot(grades$Grade~grades$Income)  #creates side-by-side boxplots
stem(grades$Grade)                   #creates a stem plot for variable x
plot(Grade~Income,grades)            #creates a scatter plot for y versus x
plot(grades)                         #provides a scatter plot matrix
plot(grades$Income,grades$Grade)
abline(h=mean.Grade,col="red")
abline(lm(Grade~Income,data=grades),col="green")
coplot(Grade~Income|Study,panel=panel.smooth,grades)
termplot(model)

#linear regression models
model=lm(grades$Grade~grades$Study)  #fit a regression model
summary(model)                       #results from fitting the regression model
summary(lm(Grade~Income+Study,data=grades))
anova(model)                         #anaysis of variance table
plot(model)                          #four plots of residuals
fits=model$fitted.values             #calculate fitted values
fitted(lm(Grade~Study,grades))
resids=model$residuals               #calculate residual values
coeff=model$coeff[2]                 #calculate slope coefficient
model$coefficients                   #calculate intercept and slope coefficient
confint(model)                       #calculate CIs for all parameters
predict.lm(model, interval="confidence") #make prediction and give confidence interval for mean response
predict.lm(model, interval="prediction") #make prediction and give prediction interval for mean response
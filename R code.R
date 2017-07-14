# Load dataset
data(mtcars)

#check the "mtcars" data
str(mtcars)

# Change from num to factor
mtcars$am[mtcars$am==1]<-"Manual"
mtcars$am[mtcars$am==0]<-"Automatic"
mtcars$am<-as.factor(mtcars$am)


# Focus on MPG and draw a histogram
summary(mtcars$mpg)  # mpg consists of numbers

# Draw a histogram and check the relationship between mpg & transmission
par(mfrow=c(1,2))

hist(mtcars$mpg,breaks=10,main="Freq. of Miles per Gallon",xlab="MPG")
boxplot(mpg~am,mtcars,main="MPG by Transmission",xlab="Transmission",ylab="MPG")


# Define new variable and store new data frame
mtcars_auto<-mtcars[mtcars$am=="Automatic",]
mtcars_manu<-mtcars[mtcars$am=="Manual",]

t.test(mtcars_auto$mpg,mtcars_manu$mpg)


# Using "lm()" function, consider linear regression
fit_mtcars<-lm(mpg~am, mtcars)
summary(fit_mtcars)


par(mfrow=c(1,2))

# plot the linear graph
mtcars_fitted<-fitted(fit_mtcars)

plot(mtcars$mpg,main="MPG Real & Fitted value",xlab="",ylab="MPG",ylim=c(10,34),col=c("blue","red")[mtcars$am])
par(new=T)
plot(mtcars_fitted,type="l",xlab="",ylab="",ylim=c(10,34),col="green",lwd=2)

plot(residuals(fit_mtcars),main="Residuals",Xlab="",ylab="Residuals")
abline(h=0,col="red")


# Check the overall parameters
fit_overall<-lm(mpg~.,mtcars)
summary(fit_overall)

plot(mtcars)

# Exclude the variables sequentially, and compare the R squared value
fit_ex_cyl<-lm(mpg~disp+hp+drat+wt+qsec+vs+am+gear+carb,mtcars)
summary(fit_ex_cyl)
fit_ex_vs<-lm(mpg~disp+hp+drat+wt+qsec+am+gear+carb,mtcars)
summary(fit_ex_vs)
fit_ex_carb<-lm(mpg~disp+hp+drat+wt+qsec+am+gear,mtcars)
summary(fit_ex_carb)
fit_ex_gear<-lm(mpg~disp+hp+drat+wt+qsec+am,mtcars)
summary(fit_ex_gear)
fit_ex_drat<-lm(mpg~disp+hp+wt+qsec+am,mtcars)
summary(fit_ex_drat)

fit_ex_disp<-lm(mpg~hp+wt+qsec+am,mtcars)
summary(fit_ex_disp)
fit_ex_hp<-lm(mpg~wt+qsec+am,mtcars)
summary(fit_ex_hp)


# Use anova function
anova(fit_mtcars,fit_ex_drat)

# plot
par(mfrow=c(2,2))
plot(fit_mtcars)

par(mfrow=c(2,2))
plot(fit_ex_drat)

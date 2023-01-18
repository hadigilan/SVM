#
#	installing package
#


install.packages("e1071")
library(e1071)


#
#	Example 1: iris dataset
#

data(iris)
attach(iris)

dim(iris)
head(iris)
table(Species)

pairs(iris[,1:4], col=Species, pch=16, lower.panel = NULL,  oma=c(3,3,3,15))
par(xpd = TRUE)
legend("bottomright", fill = unique(Species), legend = levels(Species) )

svm.linear <- svm(Species ~ . , data=iris, kernel="linear") 
plot(svm.linear , Petal.Width ~ Petal.Length, data=iris)


svm.radial <- svm(Species ~ ., data=iris, kernel="radial")
plot(svm.radial, data=iris, Petal.Width ~ Petal.Length, slice = list(Sepal.Width=3, Sepal.Length=4) )


#
#	Example 2: An application in finance
#



dps<- read.table("C:/data.txt", header=T)
attach(dps)


head(dps)
dim(dps)
table(year)
length(unique(firm))

dps.svm<- svm( as.factor(DPS_Dummy) ~ risk + growth + mb + size + cash + lev + cr + re + roa + tax + fix + age + liquid, data=dps) 
pred.svm <- predict(dps.svm, dps)

confusion.svm<- table(predicted = pred.svm , actual = dps$DPS_Dummy)
confusion.svm
100*(1-(sum(diag(confusion.svm))/sum(confusion.svm)))



#
#	lasso logistic regression
#

library(glmnet)
x<- as.matrix(dps[ ,4:16])
y<- dps$DPS_Dummy

dps.cv.lasso<- cv.glmnet(x, y, family = "binomial", alpha=1)
bestlam<- dps.cv.lasso$lambda.min

dps.lasso<- glmnet(x, y, family = "binomial", alpha=1, lambda=bestlam)
pred.lasso<- predict(dps.lasso, newx = x, type='class' )

confusion.lasso<- table(predicted=pred.lasso, actual = dps$DPS_Dummy)
confusion.lasso
100*(1-(sum(diag(confusion.lasso))/sum(confusion.lasso)))


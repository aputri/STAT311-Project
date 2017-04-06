#Script 3 - KNN Regression and Lasso

term <- read.csv("TermLife.csv", header=TRUE)
attach(term)

#KNN Regression
library(FNN)
summary(term)
term.complete <- subset(term, FACE!=0, select=c(AGE, EDUCATION, NUMHH,INCOME, FACE))
ln.income <- log(term.complete$INCOME)
term.complete <- data.frame(term.complete, ln.income)
ln.face <- log(term.complete$FACE)
term.complete <- data.frame(term.complete, ln.face)
attach(term.complete)
View(term.complete)

plot(ln.income, ln.face)
knnr <- knn.reg(ln.income, test=NULL, ln.face, k=10)
lord <- order(ln.income)
lines(ln.income[lord], knnr$pred[lord], col="blue", lwd=3)

#Lasso
library(gclus)
library(MASS)
library(glmnet)
b <- model.matrix(FACE~., term[,1:14])
y <- term$FACE
grid <- 10^seq(10, -2, length=100)
lassob <- cv.glmnet(b, y, alpha=1, lambda=grid)
plot(lassob$glmnet.fit, label=TRUE)
plot(lassob)
lam <- lassob$lambda.min
lamsm <- lassob$lambda.1se
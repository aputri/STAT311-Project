#Script 2 - Multiple Linear Regression and Decison Trees

library(tree)
library(randomForest)
term <- read.csv("TermLife.csv", header=TRUE)
termval <- term[0:14]

#Multiple Linear Regression
fit1<-lm(FACE~GENDER+AGE+MARSTAT+EDUCATION+ETHNICITY+SMARSTAT+SGENDER+SAGE+SEDUCATION+NUMHH+INCOME,data=termval)
summary(fit1)

fitfull<-lm(FACE~.,data=termval)
summary(fitfull)
plot(FACE~.,data=termval)
abline(lm(fitfull), col="red")

fit0<-lm(FACE~1,data=termval)
summary(fit0) 

back<-step(fitfull, scope=list(lower=fit0,upper=fitfull),direction="backward",trace=F)
summary(back)

#Decision Trees

facetree<-tree(FACE~.,data=termval)
plot(facetree)
text(facetree,pretty=0)
summary(facetree)



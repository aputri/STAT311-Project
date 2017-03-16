library(tree)
library(randomForest)
term <- read.csv("TermLife.csv", header=TRUE)
head(term)
attach(term)
fit1<-lm(INCOME~GENDER+EDUCATION+FACECVLIFEPOLICIES,data=term)
summary(fit1)
fitfull<-lm(INCOME~.,data=term)
summary(fitfull)

fit0<-lm(INCOME~1,data=term)

select.backward<-step(fitfull, scope=list(lower=fit0,upper=fitfull),direction="backward",trace=F)
select.backward$anova
summary(select.backward)

regfull<-lm(FACE~.,data=term)
reg0<-lm(FACE~1,data=term)
summary(reg) 
back<-step(regfull, scope=list(lower=reg0,upper=regfull),direction="backward",trace=F)
summary(back)

regtree <- tree(TOTINCOME~., data=term)
plot(regtree)
text(regtree,pretty=0)

facetree<-tree(FACE~.,data=term)
plot(facetree)
text(facetree,pretty=0)

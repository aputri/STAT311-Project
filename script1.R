#Script 1 - Principal Component Analysis and Neural Network

term <- read.csv("TermLife.csv", header=TRUE)
termval <- term[0:14]

#Principal Component Analysis
pcaterm <- prcomp(termval, scale.=TRUE)
names(pcaterm)
pcaterm$center
pcaterm$scale
pcaterm$rotation
pcavar <- pcaterm$sdev^2
pcave <- pcavar/sum(pcavar)
plot(pcave, xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b')
summary(pcaterm)
round(pcaterm$rotation[,1:2], 4)
biplot(pcaterm)

#Neural Network
set.seed(500)
library(MASS)

apply(termval,2,function(x) sum(is.na(x)))
index <- sample(1:nrow(termval),round(0.75*nrow(termval)))

train <- termval[index,]
test <- termval[-index,]
lm.fit <- glm(FACE~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

maxs <- apply(termval, 2, max) 
mins <- apply(termval, 2, min)

scaled <- as.data.frame(scale(termval, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("FACE ~", paste(n[!n %in% "FACE"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
plot(nn)
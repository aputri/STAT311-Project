library(neuralnet)
term <- read.csv("TermLife.csv", header=TRUE)
termval <- term[0:14]

#Principal Component Analysis
pcaterm <- prcomp(termval, scale.=TRUE)
pcaterm$rotation
summary(pcaterm)
biplot(pcaterm)
plot(pcaterm, type="lines")
abline(a=1, b=0, col="red", lwd=3)

#Neural Network
scar <- apply(termval, 2, function(v) (v-min(v))/max(v)-min(v))
set.seed(4521)
nn <- neuralnet(FACE~GENDER+AGE+MARSTAT+EDUCATION+ETHNICITY+SMARSTAT+SGENDER+SAGE+SEDUCATION+NUMHH+INCOME+TOTINCOME+CHARITY, data=scar, hidden=10, stepmax=1e6)
plot(nn)
#Training and Testing
set.seed(217)
ind <- sample(1:nrow(scar), 41)
train <- scar[ind,]
test <- scar[-ind,]
nntrain <- neuralnet(FACE~GENDER+AGE+MARSTAT+EDUCATION+ETHNICITY+SMARSTAT+SGENDER+SAGE+SEDUCATION+NUMHH+INCOME+TOTINCOME+CHARITY, data=train, hidden=10, stepmax=1e6)
nntest <- neuralnet(FACE~GENDER+AGE+MARSTAT+EDUCATION+ETHNICITY+SMARSTAT+SGENDER+SAGE+SEDUCATION+NUMHH+INCOME+TOTINCOME+CHARITY, data=test, hidden=10, stepmax=1e6)
plot(nntrain)
plot(nntest)

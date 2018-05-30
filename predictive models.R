completeDB <- read.csv("completeDB.csv")




# Explanatory Models
firstInteractions = model.matrix(~(-1+rocky1+rocky2+rocky3+rocky4),completeDB)
secondInteractions = model.matrix(~(-1+rocky1+rocky2+rocky3+rocky4)^2,completeDB)
thirdInteractions = model.matrix(~(-1+rocky1+rocky2+rocky3+rocky4)^3,completeDB)
fourthInteractions = model.matrix(~(-1+rocky1+rocky2+rocky3+rocky4)^4,completeDB)
# Run and store a linear regression for each of the above sets of predictor variables.
lr1 = lm(rocky5~firstInteractions, completeDB)
lr2 = lm(rocky5~secondInteractions, completeDB)
lr3 = lm(rocky5~thirdInteractions, completeDB)
lr4 = lm(rocky5~fourthInteractions, completeDB)
# Calculate AIC, and BIC for each of these linear regressions. 
install.packages('leaps')
library(leaps)
leapsModels1 = regsubsets(x=firstInteractions, y=completeDB$rocky5, nbest=1,nvmax=ncol(firstInteractions))
subsetSummary1 = summary(leapsModels1)
bestAIC1 = which.min(subsetSummary1$cp)
bestBIC1 = which.min(subsetSummary1$bic)

leapsModels2 = regsubsets(x=secondInteractions, y=completeDB$rocky5, nbest=1,nvmax=ncol(secondInteractions))
subsetSummary2 = summary(leapsModels2)
bestAIC2 = which.min(subsetSummary2$cp)
bestBIC2 = which.min(subsetSummary2$bic)

leapsModels3 = regsubsets(x=thirdInteractions, y=completeDB$rocky5, nbest=1,nvmax=ncol(thirdInteractions))
subsetSummary3 = summary(leapsModels3)
bestAIC3 = which.min(subsetSummary3$cp)
bestBIC3 = which.min(subsetSummary3$bic)

leapsModels4 = regsubsets(x=fourthInteractions, y=completeDB$rocky5, nbest=1,nvmax=ncol(fourthInteractions))
subsetSummary4 = summary(leapsModels4)
bestAIC4 = which.min(subsetSummary4$cp)
bestBIC4 = which.min(subsetSummary4$bic)

# Estimate the lasso model on the fourthInteractions data using the glmnet function. Use the
# predict function to extract the coefficients in the case of s = 0.05 and s = 0.5.
install.packages("glmnet")
library(glmnet)
lassoFit = glmnet(fourthInteractions, y=completeDB$rocky5, alpha=1)
predict(lassoFit, s=.5, type='coefficients')
predict(lassoFit, s=.05, type='coefficients')
# calculate an optimal penalty parameter using cross validation. 
set.seed(210)
lassoFit_Best = cv.glmnet(fourthInteractions, y=completeDB$rocky5, alpha=1)
lassoFit_Best
plot(lassoFit_Best)
# $lambda.min
# [1] 0.0008260556
# implement a ridge estimator using fourthInteractions. 
ridgeFit = cv.glmnet(fourthInteractions, y=completeDB$rocky5, alpha=0)
ridgeFit
plot(ridgeFit)
# $lambda.min
# [1] 0.08857523
# Extract the coefficients from the lasso and the ridge regression. 
predict(lassoFit_Best, s=lassoFit_Best$lambda.min, type = 'coefficients')
predict(ridgeFit, s=ridgeFit$lambda.min, type = 'coefficients')









# Predictive Modelling
# Create at least two training and validation data sets. 
set.seed(111)
nFold = 10
valNum = floor(runif(nrow(completeDB))*nFold)+1
for(fold in 1:nFold){
  trainingData = subset(completeDB,valNum!=fold)
  validationData = subset(completeDB,valNum==fold)}
# Predict the ratings of rocky5 using a number of different models

# Linear Regression, using the 'lm' function
completeDB <- read.csv("completeDB.csv")
# 15 Combinations with fourth interaction btw 4 variables
InteractionComb = colnames(model.matrix( ~ (-1 + rocky1 + rocky2 + rocky3 + rocky4)^4, completeDB))
length(InteractionComb) # 15 Combinations with fourth interaction btw 4 variables
# List Matrix of all Combination of 15 variable selection
Matrix = expand.grid(c(T,F),c(T,F),c(T,F),c(T,F),c(T,F),
                     c(T,F),c(T,F),c(T,F),c(T,F),c(T,F),
                     c(T,F),c(T,F),c(T,F),c(T,F),c(T,F))
Matrix = Matrix[- nrow(Matrix),] # delete last row with all F             
# Create a list of functions with all subset + interaction
FunctionList = apply(Matrix, 1, function(i) 
  as.formula(paste("rocky5~", paste(InteractionComb[i], collapse = "+"))))
# K-Fold Cross Validation
set.seed(111) 
nFold = 5
valNum = floor(runif(nrow(completeDB))*nFold)+1
# Create a matrix where we store prediction error 
modelPerformance = matrix(NA,nrow = nFold,ncol = length(FunctionList))
# Run each model for each K-Fold
for(i in 1:nFold){
  trainingData = subset(completeDB,valNum!=i)
  validationData = subset(completeDB,valNum==i)
  for (j in 1:length(FunctionList)){
    lmFit = lm(FunctionList[[j]], data = trainingData)
    lmMSE = mean((validationData$rocky5 - predict(lmFit,validationData))^2)
    modelPerformance[i, j] = lmMSE
  }
}


# Check Average Model Performance
ColMSE = colMeans(modelPerformance)
ColMSE[which.min(ColMSE)] # smallest MSE
FunctionList[which.min(ColMSE)]
# Best Model is : rocky5 ~ rocky2 + rocky3 + rocky1:rocky2 + rocky1:rocky4 + rocky2:rocky3 + 
# rocky2:rocky4 + rocky3:rocky4 + rocky1:rocky2:rocky4 + rocky1:rocky2:rocky3:rocky4


# MARS, using the 'earth' package.
install.packages("earth")
library(earth)
earthMSE = c()
for (i in c(0,0.3,0.5,1,2,3,4,5)){
  for (j in seq(from=0, to=0.2, by=.01)){
    for (k in 1:5){
      modelPerformance = matrix(NA,nFold,1)
      for(fold in 1:nFold){
        trainingData = subset(completeDB,valNum!=fold)
        validationData = subset(completeDB,valNum==fold)
        earthFit = earth(rocky5~rocky1+rocky2+rocky3+rocky4,data = trainingData, trace=i,thres=j,degree =k)
        valid = mean((validationData$rocky5 - predict(earthFit,validationData))^2)
        modelPerformance[fold,] = c(valid)
      }
      earthMSE[length(earthMSE)+1] = colMeans(modelPerformance)}}}
length(earthMSE)
i = c(0,0.3,0.5,1,2,3,4,5)
j = seq(from=0, to=0.2, by=.01)
k = c(1:5)
d = expand.grid(k,j,i)
colnames(d) <- c("degree", "thre","trace")
earthResult = cbind(d,earthMSE)


# Neural networks, using the 'nnet' package.
install.packages("nnet")
library("nnet")
nnetMSE = c()
for (i in 1:20){
  for (j in c("True", "False")){
    modelPerformance = matrix(NA,nFold,1)
    for(fold in 1:nFold){
      trainingData = subset(completeDB,valNum!=fold)
      validationData = subset(completeDB,valNum==fold)
      nnetFit = nnet(rocky5~rocky1+rocky2+rocky3+rocky4,data = trainingData, linout=1,size = i,maxit = 10000,skip = j)
      valid = mean((validationData$rocky5 - predict(nnetFit,validationData))^2)
      modelPerformance[fold,] = c(valid)}
    nnetMSE[length(nnetMSE)+1] = colMeans(modelPerformance)}}
nnetMSE
i = c(1:20)
j = c("True", "False")
d = expand.grid(j,i)
colnames(d) <- c("skip", "size")
nnetResult = cgcbind(d,nnetMSE)


# K-Nearest Neighbour, using the 'kknn' package. 
install.packages('kknn')
library('kknn')
kknnMSE = c()
for (i in 1:200){
  modelPerformance = matrix(NA,nFold,1)
  for(fold in 1:nFold){
    trainingData = subset(completeDB,valNum!=fold)
    validationData = subset(completeDB,valNum==fold)
    kknnModel = kknn(rocky5~rocky1+rocky2+rocky3+rocky4,trainingData, validationData, k = i)
    valid= mean((validationData$rocky5 - kknnModel$fitted.values)^2)
    modelPerformance[fold,] = c(valid)}
  kknnMSE[length(kknnMSE)+1] = colMeans(modelPerformance)}
kknnMSE
i = c(1:200)
d = expand.grid(i)
colnames(d) <- c("k")
kknnResult = cbind(d,kknnMSE)


# Find the best predictive model of each type
# Best model, for the 'lm' package.
ColMSE = colMeans(modelPerformance)
ColMSE[which.min(ColMSE)] # smallest MSE
FunctionList[which.min(ColMSE)]
# Best Model is : rocky5 ~ rocky2 + rocky3 + rocky1:rocky2 + rocky1:rocky4 + rocky2:rocky3 + 
# rocky2:rocky4 + rocky3:rocky4 + rocky1:rocky2:rocky4 + rocky1:rocky2:rocky3:rocky4

# Best model, for the 'earth' package.
bestEarthI = earthResult$trace[which.min(earthResult$earthMSE)]
bestEarthJ = earthResult$thre[which.min(earthResult$earthMSE)]
bestEarthK = earthResult$degree[which.min(earthResult$earthMSE)]
minEarthMSE=min(earthResult$earthMSE) # smallest MSE
bestEarthI # trace= 0
bestEarthJ # thres = 0
bestEarthK # degree = 2

# Best model, for the 'nnet' package.
bestNnetI = nnetResult$size[which.min(nnetResult$nnetMSE)]
bestNnetJ = nnetResult$skip[which.min(nnetResult$nnetMSE)]
minNnetMSE = min(nnetResult$nnetMSE)
bestNnetI # 5
bestNnetJ # True 
minNnetMSE

# Best model, for the 'kknn' package.
bestKknnI = kknnResult$k[which.min(kknnResult$kknnMSE)]
minKnnMSE = min(kknnResult$kknnMSE)
bestKknnI # 78
minKnnMSE


# Using the entire data set, re-estimate each of the best models found
df_test = read.csv('Test Set.csv')
# Restimation, for linear regression
lm_best = lm(rocky5~rocky2+rocky3
             +rocky1:rocky2+rocky1:rocky4+rocky2:rocky3+rocky2:rocky4+rocky3:rocky4
             +rocky1:rocky2:rocky4
             +rocky1:rocky2:rocky3:rocky4, data=completeDB)
# Restimation, for mars
earth_best = earth(rocky5~rocky1+rocky2+rocky3+rocky4,data = completeDB, trace=0, thres=0, degree=2)
# Restimation, for nnet
nnet_best = nnet(rocky5~rocky1+rocky2+rocky3+rocky4,data = completeDB, linout=1, size = 5, maxit = 10000, skip = 'True')
# Restimation, for kknn
set.seed(111)
isTraining = runif(nrow(completeDB)) < .8
#Split into training and validationData
trainingData1 = subset(completeDB,isTraining)
validationData1 = subset(completeDB,!isTraining)
df_test = read.csv('Test Set.csv')
kknn_best = kknn(rocky5~rocky1+rocky2+rocky3+rocky4,trainingData1, df_test, k = 78) 



# Put predictions of all models in a dataframe
df_test = read.csv('Test Set.csv')
rowNum = nrow(df_test)
df_prediction <- data.frame(matrix(ncol = 4, nrow = rowNum))
columns <- c("lm", "mars", "nnet","kknn")
colnames(df_prediction) <- columns

df_prediction$lm= predict(lm_best,df_test)
df_prediction$mars= predict(earth_best,df_test)
df_prediction$nnet= predict(nnet_best,df_test)
df_prediction$kknn= kknn_best$fitted.values

write.table(df_prediction, file = "bestPredictions.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")





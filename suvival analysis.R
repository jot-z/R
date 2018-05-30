rm(list=ls())
# load survival and load data
### fit the model and predict 
library('survival')
library('survminer')
library("ggplot2")
library("prodlim")
library("riskRegression")
library("pec")
library("ggfortify")

all_data = read.csv("Final Data 5.1.csv")
all_data = subset(all_data, ProblemOccured ==1)
all_data$Page.to.Failure = (all_data$Page.to.Failure/all_data$Survival.Day)
all_data$Page.30D = (all_data$Page.30D/30)
all_data$Page.15D = (all_data$Page.15D/15)
all_data$Page.7D = (all_data$Page.7D/7)
all_data$Page.2D = (all_data$Page.2D/2)
##### check the correlation matrix
library('corrplot')
cor(all_data[,c(7,8,11,12,13,14)])
corrplot(cor(all_data[,c(7,8,11,12,13,14)]),method='circle')


#####scaled dataset
all_data$CumPage = (all_data$CumPage/10000)
all_data$Page.to.Failure = (all_data$Page.to.Failure/100000)
all_data$Page.30D = (all_data$Page.30D/100000)
all_data$Page.15D = (all_data$Page.15D/100000)
all_data$Page.7D = (all_data$Page.7D/100000)
all_data$Page.2D = (all_data$Page.2D/100000)

all_data[is.na(all_data)] = 0

### split data into training and test dataset
set.seed(123)
sample = floor(0.8 * nrow(all_data))
train_ind <- sample(seq_len(nrow(all_data)), size = sample)
train <- all_data[train_ind, ]
test <- all_data[-train_ind, ]



### input new asset
newAsset = data.frame(age = c(50, 50), 
                      Label = c(2,2),
                      lastmonthvolume = c(90000, 90000), 
                      last15dayvolume = c(50000, 50000),
                      yesterdayvolume = c(100, 100))
### Cause-Specific Cox
CSCmodel = CSC(Hist(Survival.Day, ProblemTypeName) ~ age+factor(Label)+
                 lastmonthvolume+last15dayvolume+yesterdayvolume, 
            data = train)
summary(CSCmodel)
predict(CSCmodel, newAsset, type = 'expected', cause = 'Paper Jam', times = c(7,30))


### Cox
cox_model <- coxph(Surv(Survival.Day, ProblemOccured) ~ factor(Label)+Page.to.Failure+
                     Page.30D+Page.15D+Page.7D+Page.2D, 
                   data = train, x=TRUE)
summary(cox_model)
cox.zph(cox_model)

summary(coxph(Surv(Survival.Day, ProblemOccured) ~ factor(Label)+Page.to.Failure+Page.2D,data = train, x=TRUE))

predictCox(cox_model, newAsset, type = 'survival', times = c(7,30))

PredError <- pec(list("CoxModel"=cox_model), Surv(Survival.Day, ProblemOccured) ~ 1, data=test)
crps(PredError)
ibs(PredError)
print(PredError, times = seq(5,30,5))

plot(PredError,xlim=c(0,150),ylim = c(0, 0.3), legend = 'right')


which.max(PredError$AppErr$Reference)
max(PredError$AppErr$Reference)

which.max(PredError$AppErr$Cox)
max(PredError$AppErr$Cox)


##### based on volume
volume_model = survfit(Surv(Page.to.Failure, ProblemOccured)~Label, data = train)
summary(volume_model)
autoplot(volume_model, xlim=c(0,50000))

##### test
test_cox_model <- coxph(Surv(Survival.Day, ProblemOccured) ~ ridge(factor(Label),CumPage,
                     Page.30D,Page.15D,Page.7D,Page.2D, theta = 5), 
                   data = train, x=TRUE)
print(test_cox_model$coefficients)

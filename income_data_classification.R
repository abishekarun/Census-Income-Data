#load packages & data
library(data.table)
library(ggplot2)

train <- fread("train.csv",na.strings = c(""," ","?","NA",NA))
test <- fread("test.csv",na.strings = c(""," ","?","NA",NA))

#look at data
dim(train)
str (train)

dim(test) 
str (test)

train[1:5]
test[1:5]

#check target variables
unique(train$income_level)
unique(test$income_level)

#encode target variables
train[,income_level := ifelse(income_level == -50000,0,1)]
test[,income_level := ifelse(income_level == -50000,0,1)]

round(prop.table(table(train$income_level))*100)

#set column classes
factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols)

train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

#subset categorical variables
cat_train <- train[,factcols, with=FALSE]
cat_test <- test[,factcols,with=FALSE]

#subset numerical variables
num_train <- train[,numcols,with=FALSE]
num_test <- test[,numcols,with=FALSE] 

rm(train,test) #to save memory

#load libraries
library(ggplot2)
library(plotly)

#write a plot function
tr <- function(a){
ggplot(data = num_train, aes(x= a, y=..density..)) + 
geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + 
geom_density()
ggplotly()
}

tr(num_train$age)
tr(num_train$capital_losses)

num_train[,income_level := cat_train$income_level]

#create a scatter plot
ggplot(data=num_train,aes(x = age, y=wage_per_hour))+
geom_point(aes(colour=income_level))+
scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))

#dodged bar chart
all_bar <- function(i){
ggplot(cat_train,aes(x=i,fill=income_level))+
geom_bar(position = "dodge",color="black")+
scale_fill_brewer(palette = "Pastel1")+
theme(axis.text.x =element_text(angle= 60,hjust = 1,size=10))
}

#variable class_of_worker
all_bar(cat_train$class_of_worker)

#variable education
all_bar(cat_train$education)

prop.table(table(cat_train$marital_status,cat_train$income_level),1)
prop.table(table(cat_train$class_of_worker,cat_train$income_level),1)

#check missing values in numerical data
table(apply(num_train, 2, function(x) any(is.na(x))))
table(apply(num_test, 2, function(x) any(is.na(x))))

num_train$income_level<- NULL
library(caret)

#set threshold as 0.7
ax <-findCorrelation(x = cor(num_train), cutoff = 0.7)

num_train <- num_train[,-ax,with=FALSE] 
num_test[,weeks_worked_in_year := NULL]

#check missing values per columns
mvtr <- sapply(cat_train, function(x){sum(is.na(x))/length(x)})*100
mvte <- sapply(cat_test, function(x){sum(is.na(x)/length(x))}*100)
mvtr
mvte

#select columns with missing value less than 5%
cat_train <- subset(cat_train, select = mvtr < 5 )
cat_test <- subset(cat_test, select = mvte < 5)

#set NA as Unavailable - train data
#convert to characters
cat_train <- cat_train[,names(cat_train) := lapply(.SD, as.character),.SDcols = names(cat_train)]
for (i in seq_along(cat_train)) set(cat_train, i=which(is.na(cat_train[[i]])), j=i, value="Unavailable")
#convert back to factors
cat_train <- cat_train[, names(cat_train) := lapply(.SD,factor), .SDcols = names(cat_train)]

#set NA as Unavailable - test data
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, as.character), .SDcols = names(cat_test)]
for (i in seq_along(cat_test)) set(cat_test, i=which(is.na(cat_test[[i]])), j=i, value="Unavailable")
#convert back to factors
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, factor), .SDcols = names(cat_test)]

#combine factor levels with less than 5% values
#train
for(i in names(cat_train)){
p <- 5/100
ld <- names(which(prop.table(table(cat_train[[i]])) < p))
levels(cat_train[[i]])[levels(cat_train[[i]]) %in% ld] <- "Other"
}

#test
for(i in names(cat_test)){
p <- 5/100
ld <- names(which(prop.table(table(cat_test[[i]])) < p))
levels(cat_test[[i]])[levels(cat_test[[i]]) %in% ld] <- "Other"
}

#check columns with unequal levels 
library(mlr)
summarizeColumns(cat_train)[,"nlevs"]
summarizeColumns(cat_test)[,"nlevs"]

num_train[,.N,age][order(age)]
num_train[,.N,wage_per_hour][order(-N)]

#bin age variable 0-30 31-60 61 - 90
num_train[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_train[,age := factor(age)]

num_test[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_test[,age := factor(age)]

#Bin numeric variables with Zero and MoreThanZero
num_train[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_train[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_train[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_train[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

num_test[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_test[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_test[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_test[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

#combine data and make test & train files
d_train <- cbind(num_train,cat_train)
d_test <- cbind(num_test,cat_test)

#remove unwanted files
rm(num_train,num_test,cat_train,cat_test) #save memory

#load library for machine learning
library(mlr)

d_train<-as.data.frame(d_train)
d_test<-as.data.frame(d_test)

#create task
train.task <- makeClassifTask(data = d_train,target = "income_level")
test.task <- makeClassifTask(data=d_test,target = "income_level")

#remove zero variance features
train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)

#get variable importance chart
var_imp <- generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)

#undersampling 
train.under <- undersample(train.task,rate = 0.1) #keep only 10% of majority class
table(getTaskTargets(train.under))

#oversampling
train.over <- oversample(train.task,rate=15) #make minority class 15 times
table(getTaskTargets(train.over))

#SMOTE
system.time(
train.smote <- smote(train.task,rate = 10,nn = 3) 
)

table(getTaskTargets(train.smote))

#lets see which algorithms are available
a<-listLearners("classif","twoclass")[c("class","package")]

#naive Bayes
naive_learner <- makeLearner("classif.naiveBayes",predict.type = "response")
naive_learner$par.vals <- list(laplace = 1)

#10fold CV - stratified
folds <- makeResampleDesc("CV",iters=10,stratify = TRUE)

#cross validation function
fun_cv <- function(a){
crv_val <- resample(naive_learner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
crv_val$aggr
}

fun_cv (train.task)
fun_cv(train.under) 
fun_cv(train.over)
fun_cv(train.smote)

#train and predict
nB_model <- train(naive_learner, train.smote)
nB_predict <- predict(nB_model,test.task)

#evaluate
nB_prediction <- nB_predict$data$response
dCM <- confusionMatrix(d_test$income_level,nB_prediction)

#calculate F measure
precision <- dCM$byClass['Pos Pred Value']
recall <- dCM$byClass['Sensitivity']
f_measure <- 2*((precision*recall)/(precision+recall))
f_measure

library(Matrix)
sparse_matrix <- sparse.model.matrix(income_level ~ .-1, data = d_train)
dtrain<-as.data.frame(as.matrix(sparse_matrix))
sparse_matrix <- sparse.model.matrix(income_level ~ .-1, data = d_test)
dtest<-as.data.frame(as.matrix(sparse_matrix))
names(dtrain) <- make.names(names(dtrain))
names(dtest) <- make.names(names(dtest))
dtrain$income_level<-d_train$income_level
dtest$income_level<-d_test$income_level

#create task
train.task <- makeClassifTask(data = dtrain,target = "income_level")
test.task <- makeClassifTask(data=dtest,target = "income_level")

# XGBoost Model
library(xgboost)
set.seed(2002)
xgb_learner <- makeLearner("classif.xgboost",predict.type = "response")
xgb_learner$par.vals <- list(
objective = "binary:logistic",
eval_metric = "error",
nrounds = 100,
print_every_n = 50
)

#define hyperparameters for tuning
xg_ps <- makeParamSet( 
makeIntegerParam("max_depth",lower=3,upper=10),
makeNumericParam("lambda",lower=0.05,upper=0.5),
makeNumericParam("eta", lower = 0.01, upper = 0.5),
makeNumericParam("subsample", lower = 0.50, upper = 1),
makeNumericParam("min_child_weight",lower=2,upper=10),
makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80)
)

#define search function
rancontrol <- makeTuneControlRandom(maxit = 10L) #do 10 iterations

#10 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 10L,stratify = TRUE)

#tune parameters
xgb_tune <- tuneParams(learner = xgb_learner, task = train.task, resampling = set_cv, measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = xg_ps, control = rancontrol)

#set optimal parameters
xgb_new <- setHyperPars(learner = xgb_learner, par.vals = xgb_tune$x)

#train model
xgmodel <- train(xgb_new, train.task)

#test model
predict.xg <- predict(xgmodel, test.task)

#make prediction
xg_prediction <- predict.xg$data$response

#make confusion matrix
xg_confused <- confusionMatrix(d_test$income_level,xg_prediction)

precision <- xg_confused$byClass['Pos Pred Value']
recall <- xg_confused$byClass['Sensitivity']

f_measure <- 2*((precision*recall)/(precision+recall))
f_measure

#top 20 features
filtered.data <- filterFeatures(train.task,method = "information.gain",abs = 20)
#train
xgb_boost <- train(xgb_new,filtered.data)

#test model
predict.xg <- predict(xgb_boost, test.task)

#make prediction
xg_prediction <- predict.xg$data$response

#make confusion matrix
xg_confused <- confusionMatrix(d_test$income_level,xg_prediction)

precision <- xg_confused$byClass['Pos Pred Value']
recall <- xg_confused$byClass['Sensitivity']

f_measure <- 2*((precision*recall)/(precision+recall))
f_measure

#xgboost AUC 
xgb_prob <- setPredictType(learner = xgb_new,predict.type = "prob")


trainxgb <- function(data,threshold){
  #train model
  xgmodel_prob <- train(xgb_prob,data)
  
  #predict
  predict.xgprob <- predict(xgmodel_prob,test.task)
  
  #predicted probabilities
  predict.xgprob$data[1:10,]
  
  #set threshold
  pred3 <- setThreshold(predict.xgprob,threshold)
  confusionMatrix(d_test$income_level,pred3$data$response)
  
}
trainxgb(train.task,0.3)
trainxgb(train.over,0.3)
trainxgb(train.under,0.3)
trainxgb(train.smote,0.3)

# SVM Model
getParamSet("classif.svm")
svm_learner <- makeLearner("classif.svm",predict.type = "response")
svm_learner$par.vals<- list(class.weights = c("0"=1,"1"=10),kernel="radial")

svm_param <- makeParamSet(
  makeDiscreteParam("cost",values=c(.001,0.01,1,5,10,100)), 
  makeIntegerParam("gamma",lower= 0.5,upper = 2)
)

#random search
set_search <- makeTuneControlRandom(maxit = 5L) #5 times

#cross validation #10L seem to take forever
set_cv <- makeResampleDesc("CV",iters=5L,stratify = TRUE)

#tune Params
svm_tune <- tuneParams(learner = svm_learner,task = train.task,measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = svm_param,control = set_search,resampling = set_cv)

#set hyperparameters
svm_new <- setHyperPars(learner = svm_learner, par.vals = svm_tune$x)

train_svm <- function(data,threshold){
  #train model
  svm_model <- train(svm_new,data)
  
  #test model
  predict_svm <- predict(svm_model,test.task)
  pred3 <- setThreshold(predict_svm,threshold)
  confusionMatrix(d_test$income_level,pred3$data$response)
  
}

train_svm(train.task,0.3)
train_svm(train.under,0.3)
train_svm(train.over,0.3)
train_svm(train.smote,0.3)

# Randomforest Model
getParamSet("classif.randomForest")
rf_learner <- makeLearner("classif.randomForest",predict.type = "response")
rf_learner$par.vals<- list(class.weights = c("0"=100,"1"=1),kernel="radial")

rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower= 50,upper = 500), 
  makeIntegerParam("mtry",lower= 0.5,upper = 2)
)

#random search
set_search <- makeTuneControlRandom(maxit = 5L) #5 times

#cross validation #10L seem to take forever
set_cv <- makeResampleDesc("CV",iters=5L,stratify = TRUE)

#tune Params
rf_tune <- tuneParams(learner = rf_learner,task = train.task,measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = rf_param,control = set_search,resampling = set_cv)

#set hyperparameters
rf_new <- setHyperPars(learner = rf_learner, par.vals = rf_tune$x)

train_rf <- function(data,threshold){
  #train model
  rf_model <- train(rf_new,data)
  
  #test model
  predict_rf <- predict(rf_model,test.task)
  pred3 <- setThreshold(predict_rf,threshold)
  confusionMatrix(d_test$income_level,pred3$data$response)
}

train_rf(train.task,0.3)
train_rf(train.under,0.3)
train_rf(train.over,0.3)
train_rf(train.smote,0.3)


# Stacked learner

base = c("classif.randomForest", "classif.xgboost", "classif.svm")
lrns = lapply(base, makeLearner)
lrns = lapply(lrns, setPredictType, "prob")
m = makeStackedLearner(base.learners = lrns,
                       predict.type = "prob", method = "hill.climb")
tmp = train(m, train.task)
res = predict(tmp, test.task)
setwd("/mnt/A/long_covid_jiayi/jiayi/operation/longcovid_prediction")
library(data.table)
library(tableone)
require(caret)
library(pROC)
library(ROCR)

## age<45 -----
## XGBoost -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba_18-44/XGBoost.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba_18-44/XGBoost.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.05%
  pred_.boot0.05 <- as.factor(ifelse(new.data$predicted.test>0.0005,"1","0"))
  perf.boot0.05 <- confusionMatrix(pred_.boot0.05, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.05$byClass) <- paste("thre_0.05%",names(perf.boot0.05$byClass))
  #sen, spe, ppv, npv by 0.1%
  pred_.boot0.1 <- as.factor(ifelse(new.data$predicted.test>0.001,"1","0"))
  perf.boot0.1 <- confusionMatrix(pred_.boot0.1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.1$byClass) <- paste("thre_0.1%",names(perf.boot0.1$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.05$byClass,perf.boot0.1$byClass,perf.boot0.2$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("age18-44.XGBoost.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## LightGBM -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba_18-44/LightGBM.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba_18-44/LightGBM.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.05%
  pred_.boot0.05 <- as.factor(ifelse(new.data$predicted.test>0.0005,"1","0"))
  perf.boot0.05 <- confusionMatrix(pred_.boot0.05, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.05$byClass) <- paste("thre_0.05%",names(perf.boot0.05$byClass))
  #sen, spe, ppv, npv by 0.1%
  pred_.boot0.1 <- as.factor(ifelse(new.data$predicted.test>0.001,"1","0"))
  perf.boot0.1 <- confusionMatrix(pred_.boot0.1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.1$byClass) <- paste("thre_0.1%",names(perf.boot0.1$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.05$byClass,perf.boot0.1$byClass,perf.boot0.2$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("age18-44.LightGBM.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Random Forest -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba_18-44/Random Forest.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba_18-44/Random Forest.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.05%
  pred_.boot0.05 <- as.factor(ifelse(new.data$predicted.test>0.0005,"1","0"))
  perf.boot0.05 <- confusionMatrix(pred_.boot0.05, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.05$byClass) <- paste("thre_0.05%",names(perf.boot0.05$byClass))
  #sen, spe, ppv, npv by 0.1%
  pred_.boot0.1 <- as.factor(ifelse(new.data$predicted.test>0.001,"1","0"))
  perf.boot0.1 <- confusionMatrix(pred_.boot0.1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.1$byClass) <- paste("thre_0.1%",names(perf.boot0.1$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.05$byClass,perf.boot0.1$byClass,perf.boot0.2$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("age18-44.Random Forest.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Lasso -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba_18-44/Lasso.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba_18-44/Lasso.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.05%
  pred_.boot0.05 <- as.factor(ifelse(new.data$predicted.test>0.0005,"1","0"))
  perf.boot0.05 <- confusionMatrix(pred_.boot0.05, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.05$byClass) <- paste("thre_0.05%",names(perf.boot0.05$byClass))
  #sen, spe, ppv, npv by 0.1%
  pred_.boot0.1 <- as.factor(ifelse(new.data$predicted.test>0.001,"1","0"))
  perf.boot0.1 <- confusionMatrix(pred_.boot0.1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.1$byClass) <- paste("thre_0.1%",names(perf.boot0.1$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.05$byClass,perf.boot0.1$byClass,perf.boot0.2$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("age18-44.Lasso.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## age 45-64 -----
## XGBoost -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba_45-64/XGBoost.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba_45-64/XGBoost.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #sen, spe, ppv, npv by 0.5%
  pred_.boot0.5 <- as.factor(ifelse(new.data$predicted.test>0.005,"1","0"))
  perf.boot0.5 <- confusionMatrix(pred_.boot0.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.5$byClass) <- paste("thre_0.5%",names(perf.boot0.5$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.2$byClass,perf.boot0.5$byClass,perf.boot1$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("age45-64.XGBoost.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## LightGBM -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba_45-64/LightGBM.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba_45-64/LightGBM.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #sen, spe, ppv, npv by 0.5%
  pred_.boot0.5 <- as.factor(ifelse(new.data$predicted.test>0.005,"1","0"))
  perf.boot0.5 <- confusionMatrix(pred_.boot0.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.5$byClass) <- paste("thre_0.5%",names(perf.boot0.5$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.2$byClass,perf.boot0.5$byClass,perf.boot1$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("age45-64.LightGBM.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Random Forest -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba_45-64/Random Forest.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba_45-64/Random Forest.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #sen, spe, ppv, npv by 0.5%
  pred_.boot0.5 <- as.factor(ifelse(new.data$predicted.test>0.005,"1","0"))
  perf.boot0.5 <- confusionMatrix(pred_.boot0.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.5$byClass) <- paste("thre_0.5%",names(perf.boot0.5$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.2$byClass,perf.boot0.5$byClass,perf.boot1$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("age45-64.Random Forest.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Lasso -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba_45-64/Lasso.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba_45-64/Lasso.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #sen, spe, ppv, npv by 0.5%
  pred_.boot0.5 <- as.factor(ifelse(new.data$predicted.test>0.005,"1","0"))
  perf.boot0.5 <- confusionMatrix(pred_.boot0.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.5$byClass) <- paste("thre_0.5%",names(perf.boot0.5$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.2$byClass,perf.boot0.5$byClass,perf.boot1$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("age45-64.Lasso.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## age 65+ -----
## XGBoost -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba_65/XGBoost_age^2.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba_65/XGBoost_age^2.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 2%
  pred_.boot2 <- as.factor(ifelse(new.data$predicted.test>0.02,"1","0"))
  perf.boot2 <- confusionMatrix(pred_.boot2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot2$byClass) <- paste("thre_2%",names(perf.boot2$byClass))
  #sen, spe, ppv, npv by 4%
  pred_.boot4 <- as.factor(ifelse(new.data$predicted.test>0.04,"1","0"))
  perf.boot4 <- confusionMatrix(pred_.boot4, as.factor(new.data$death_1y),positive="1")
  names(perf.boot4$byClass) <- paste("thre_4%",names(perf.boot4$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot2$byClass,perf.boot4$byClass,perf.boot5$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("age65.XGBoost.age2.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## LightGBM -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba_65/LightGBM_age^2.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba_65/LightGBM_age^2.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 2%
  pred_.boot2 <- as.factor(ifelse(new.data$predicted.test>0.02,"1","0"))
  perf.boot2 <- confusionMatrix(pred_.boot2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot2$byClass) <- paste("thre_2%",names(perf.boot2$byClass))
  #sen, spe, ppv, npv by 4%
  pred_.boot4 <- as.factor(ifelse(new.data$predicted.test>0.04,"1","0"))
  perf.boot4 <- confusionMatrix(pred_.boot4, as.factor(new.data$death_1y),positive="1")
  names(perf.boot4$byClass) <- paste("thre_4%",names(perf.boot4$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot2$byClass,perf.boot4$byClass,perf.boot5$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  if(boot.perf$t0[i]>0&boot.perf$t0[i]<1&!is.na(boot.perf$t0[i])){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  }
  else{value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],c("na","na"))}
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("age65.LightGBM.age2.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Random Forest -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba_65/Random Forest_age^2.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba_65/Random Forest_age^2.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 2%
  pred_.boot2 <- as.factor(ifelse(new.data$predicted.test>0.02,"1","0"))
  perf.boot2 <- confusionMatrix(pred_.boot2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot2$byClass) <- paste("thre_2%",names(perf.boot2$byClass))
  #sen, spe, ppv, npv by 4%
  pred_.boot4 <- as.factor(ifelse(new.data$predicted.test>0.04,"1","0"))
  perf.boot4 <- confusionMatrix(pred_.boot4, as.factor(new.data$death_1y),positive="1")
  names(perf.boot4$byClass) <- paste("thre_4%",names(perf.boot4$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot2$byClass,perf.boot4$byClass,perf.boot5$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("age65.Random Forest.age2.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Lasso -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba_65/Lasso_age^2.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba_65/Lasso_age^2.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 2%
  pred_.boot2 <- as.factor(ifelse(new.data$predicted.test>0.02,"1","0"))
  perf.boot2 <- confusionMatrix(pred_.boot2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot2$byClass) <- paste("thre_2%",names(perf.boot2$byClass))
  #sen, spe, ppv, npv by 4%
  pred_.boot4 <- as.factor(ifelse(new.data$predicted.test>0.04,"1","0"))
  perf.boot4 <- confusionMatrix(pred_.boot4, as.factor(new.data$death_1y),positive="1")
  names(perf.boot4$byClass) <- paste("thre_4%",names(perf.boot4$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot2$byClass,perf.boot4$byClass,perf.boot5$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("age65.Lasso.age2.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

### all age ---------
## XGBoost -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba/XGBoost_27.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba/XGBoost_27.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #sen, spe, ppv, npv by 1.5%
  pred_.boot1.5 <- as.factor(ifelse(new.data$predicted.test>0.015,"1","0"))
  perf.boot1.5 <- confusionMatrix(pred_.boot1.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1.5$byClass) <- paste("thre_1.5%",names(perf.boot1.5$byClass))
  #sen, spe, ppv, npv by 2%
  pred_.boot2 <- as.factor(ifelse(new.data$predicted.test>0.02,"1","0"))
  perf.boot2 <- confusionMatrix(pred_.boot2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot2$byClass) <- paste("thre_2%",names(perf.boot2$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot1$byClass,perf.boot1.5$byClass,perf.boot2$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("all.XGBoost27.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## LightGBM -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba/LightGBM_27.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba/LightGBM_27.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #sen, spe, ppv, npv by 1.5%
  pred_.boot1.5 <- as.factor(ifelse(new.data$predicted.test>0.015,"1","0"))
  perf.boot1.5 <- confusionMatrix(pred_.boot1.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1.5$byClass) <- paste("thre_1.5%",names(perf.boot1.5$byClass))
  #sen, spe, ppv, npv by 2%
  pred_.boot2 <- as.factor(ifelse(new.data$predicted.test>0.02,"1","0"))
  perf.boot2 <- confusionMatrix(pred_.boot2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot2$byClass) <- paste("thre_2%",names(perf.boot2$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot1$byClass,perf.boot1.5$byClass,perf.boot2$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("all.LightGBM27.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Random Forest -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba/Random Forest_27.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba/Random Forest_27.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #sen, spe, ppv, npv by 1.5%
  pred_.boot1.5 <- as.factor(ifelse(new.data$predicted.test>0.015,"1","0"))
  perf.boot1.5 <- confusionMatrix(pred_.boot1.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1.5$byClass) <- paste("thre_1.5%",names(perf.boot1.5$byClass))
  #sen, spe, ppv, npv by 2%
  pred_.boot2 <- as.factor(ifelse(new.data$predicted.test>0.02,"1","0"))
  perf.boot2 <- confusionMatrix(pred_.boot2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot2$byClass) <- paste("thre_2%",names(perf.boot2$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot1$byClass,perf.boot1.5$byClass,perf.boot2$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("all.Random Forest27.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Lasso -----
train <- setDT(read.csv("predicted_proba_train/predicted_proba/Lasso_27.csv"))
test <- setDT(read.csv("predicted_proba_test/predicted_proba/Lasso_27.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #sen, spe, ppv, npv by 1.5%
  pred_.boot1.5 <- as.factor(ifelse(new.data$predicted.test>0.015,"1","0"))
  perf.boot1.5 <- confusionMatrix(pred_.boot1.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1.5$byClass) <- paste("thre_1.5%",names(perf.boot1.5$byClass))
  #sen, spe, ppv, npv by 2%
  pred_.boot2 <- as.factor(ifelse(new.data$predicted.test>0.02,"1","0"))
  perf.boot2 <- confusionMatrix(pred_.boot2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot2$byClass) <- paste("thre_2%",names(perf.boot2$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot1$byClass,perf.boot1.5$byClass,perf.boot2$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("all.Lasso27.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()


### inpatient 18-44 ---------
## XGBoost -----
train <- setDT(read.csv("predicted_proba_train/inpatient/predicted_proba_18-44/XGBoost.csv"))
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/XGBoost.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #sen, spe, ppv, npv by 0.5%
  pred_.boot0.5 <- as.factor(ifelse(new.data$predicted.test>0.005,"1","0"))
  perf.boot0.5 <- confusionMatrix(pred_.boot0.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.5$byClass) <- paste("thre_0.5%",names(perf.boot0.5$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.2$byClass,perf.boot0.5$byClass,perf.boot1$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("inpatient.18-44.XGBoost.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## LightGBM -----
train <- setDT(read.csv("predicted_proba_train/inpatient/predicted_proba_18-44/LightGBM.csv"))
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/LightGBM.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #sen, spe, ppv, npv by 0.5%
  pred_.boot0.5 <- as.factor(ifelse(new.data$predicted.test>0.005,"1","0"))
  perf.boot0.5 <- confusionMatrix(pred_.boot0.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.5$byClass) <- paste("thre_0.5%",names(perf.boot0.5$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.2$byClass,perf.boot0.5$byClass,perf.boot1$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("inpatient.18-44.LightGBM.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Random Forest -----
train <- setDT(read.csv("predicted_proba_train/inpatient/predicted_proba_18-44/Random Forest.csv"))
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/Random Forest.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #sen, spe, ppv, npv by 0.5%
  pred_.boot0.5 <- as.factor(ifelse(new.data$predicted.test>0.005,"1","0"))
  perf.boot0.5 <- confusionMatrix(pred_.boot0.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.5$byClass) <- paste("thre_0.5%",names(perf.boot0.5$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.2$byClass,perf.boot0.5$byClass,perf.boot1$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("inpatient.18-44.Random Forest.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Lasso -----
train <- setDT(read.csv("predicted_proba_train/inpatient/predicted_proba_18-44/Lasso.csv"))
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/Lasso.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #sen, spe, ppv, npv by 0.5%
  pred_.boot0.5 <- as.factor(ifelse(new.data$predicted.test>0.005,"1","0"))
  perf.boot0.5 <- confusionMatrix(pred_.boot0.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.5$byClass) <- paste("thre_0.5%",names(perf.boot0.5$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.2$byClass,perf.boot0.5$byClass,perf.boot1$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("inpatient.18-44.Lasso.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

### inpatient 45-64 ---------
## XGBoost -----
train <- setDT(read.csv("predicted_proba_train/inpatient/predicted_proba_45-64/XGBoost.csv"))
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/XGBoost.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #sen, spe, ppv, npv by 8%
  pred_.boot8 <- as.factor(ifelse(new.data$predicted.test>0.08,"1","0"))
  perf.boot8 <- confusionMatrix(pred_.boot8, as.factor(new.data$death_1y),positive="1")
  names(perf.boot8$byClass) <- paste("thre_8%",names(perf.boot8$byClass))
  #sen, spe, ppv, npv by 10%
  pred_.boot10 <- as.factor(ifelse(new.data$predicted.test>0.1,"1","0"))
  perf.boot10 <- confusionMatrix(pred_.boot10, as.factor(new.data$death_1y),positive="1")
  names(perf.boot10$byClass) <- paste("thre_10%",names(perf.boot10$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot5$byClass,perf.boot8$byClass,perf.boot10$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("inpatient.45-64.XGBoost.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## LightGBM -----
train <- setDT(read.csv("predicted_proba_train/inpatient/predicted_proba_45-64/LightGBM.csv"))
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/LightGBM.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #sen, spe, ppv, npv by 8%
  pred_.boot8 <- as.factor(ifelse(new.data$predicted.test>0.08,"1","0"))
  perf.boot8 <- confusionMatrix(pred_.boot8, as.factor(new.data$death_1y),positive="1")
  names(perf.boot8$byClass) <- paste("thre_8%",names(perf.boot8$byClass))
  #sen, spe, ppv, npv by 10%
  pred_.boot10 <- as.factor(ifelse(new.data$predicted.test>0.1,"1","0"))
  perf.boot10 <- confusionMatrix(pred_.boot10, as.factor(new.data$death_1y),positive="1")
  names(perf.boot10$byClass) <- paste("thre_10%",names(perf.boot10$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot5$byClass,perf.boot8$byClass,perf.boot10$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("inpatient.45-64.LightGBM.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Random Forest -----
train <- setDT(read.csv("predicted_proba_train/inpatient/predicted_proba_45-64/Random Forest.csv"))
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/Random Forest.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #sen, spe, ppv, npv by 8%
  pred_.boot8 <- as.factor(ifelse(new.data$predicted.test>0.08,"1","0"))
  perf.boot8 <- confusionMatrix(pred_.boot8, as.factor(new.data$death_1y),positive="1")
  names(perf.boot8$byClass) <- paste("thre_8%",names(perf.boot8$byClass))
  #sen, spe, ppv, npv by 10%
  pred_.boot10 <- as.factor(ifelse(new.data$predicted.test>0.1,"1","0"))
  perf.boot10 <- confusionMatrix(pred_.boot10, as.factor(new.data$death_1y),positive="1")
  names(perf.boot10$byClass) <- paste("thre_10%",names(perf.boot10$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot5$byClass,perf.boot8$byClass,perf.boot10$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("inpatient.45-64.Random Forest.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Lasso -----
train <- setDT(read.csv("predicted_proba_train/inpatient/predicted_proba_45-64/Lasso.csv"))
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/Lasso.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #sen, spe, ppv, npv by 8%
  pred_.boot8 <- as.factor(ifelse(new.data$predicted.test>0.08,"1","0"))
  perf.boot8 <- confusionMatrix(pred_.boot8, as.factor(new.data$death_1y),positive="1")
  names(perf.boot8$byClass) <- paste("thre_8%",names(perf.boot8$byClass))
  #sen, spe, ppv, npv by 10%
  pred_.boot10 <- as.factor(ifelse(new.data$predicted.test>0.1,"1","0"))
  perf.boot10 <- confusionMatrix(pred_.boot10, as.factor(new.data$death_1y),positive="1")
  names(perf.boot10$byClass) <- paste("thre_10%",names(perf.boot10$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot5$byClass,perf.boot8$byClass,perf.boot10$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("inpatient.45-64.Lasso.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

### inpatient 65+ ---------
## XGBoost -----
train <- setDT(read.csv("predicted_proba_train/inpatient/predicted_proba_65/XGBoost.csv"))
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/XGBoost.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #sen, spe, ppv, npv by 8%
  pred_.boot8 <- as.factor(ifelse(new.data$predicted.test>0.08,"1","0"))
  perf.boot8 <- confusionMatrix(pred_.boot8, as.factor(new.data$death_1y),positive="1")
  names(perf.boot8$byClass) <- paste("thre_8%",names(perf.boot8$byClass))
  #sen, spe, ppv, npv by 10%
  pred_.boot10 <- as.factor(ifelse(new.data$predicted.test>0.1,"1","0"))
  perf.boot10 <- confusionMatrix(pred_.boot10, as.factor(new.data$death_1y),positive="1")
  names(perf.boot10$byClass) <- paste("thre_10%",names(perf.boot10$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot5$byClass,perf.boot8$byClass,perf.boot10$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("inpatient.65.XGBoost.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## LightGBM -----
train <- setDT(read.csv("predicted_proba_train/inpatient/predicted_proba_65/LightGBM.csv"))
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/LightGBM.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #sen, spe, ppv, npv by 8%
  pred_.boot8 <- as.factor(ifelse(new.data$predicted.test>0.08,"1","0"))
  perf.boot8 <- confusionMatrix(pred_.boot8, as.factor(new.data$death_1y),positive="1")
  names(perf.boot8$byClass) <- paste("thre_8%",names(perf.boot8$byClass))
  #sen, spe, ppv, npv by 10%
  pred_.boot10 <- as.factor(ifelse(new.data$predicted.test>0.1,"1","0"))
  perf.boot10 <- confusionMatrix(pred_.boot10, as.factor(new.data$death_1y),positive="1")
  names(perf.boot10$byClass) <- paste("thre_10%",names(perf.boot10$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot5$byClass,perf.boot8$byClass,perf.boot10$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("inpatient.65.LightGBM.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Random Forest -----
train <- setDT(read.csv("predicted_proba_train/inpatient/predicted_proba_65/Random Forest.csv"))
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/Random Forest.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #sen, spe, ppv, npv by 8%
  pred_.boot8 <- as.factor(ifelse(new.data$predicted.test>0.08,"1","0"))
  perf.boot8 <- confusionMatrix(pred_.boot8, as.factor(new.data$death_1y),positive="1")
  names(perf.boot8$byClass) <- paste("thre_8%",names(perf.boot8$byClass))
  #sen, spe, ppv, npv by 10%
  pred_.boot10 <- as.factor(ifelse(new.data$predicted.test>0.1,"1","0"))
  perf.boot10 <- confusionMatrix(pred_.boot10, as.factor(new.data$death_1y),positive="1")
  names(perf.boot10$byClass) <- paste("thre_10%",names(perf.boot10$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot5$byClass,perf.boot8$byClass,perf.boot10$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("inpatient.65.Random Forest.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Lasso -----
train <- setDT(read.csv("predicted_proba_train/inpatient/predicted_proba_65/Lasso.csv"))
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/Lasso.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #sen, spe, ppv, npv by 8%
  pred_.boot8 <- as.factor(ifelse(new.data$predicted.test>0.08,"1","0"))
  perf.boot8 <- confusionMatrix(pred_.boot8, as.factor(new.data$death_1y),positive="1")
  names(perf.boot8$byClass) <- paste("thre_8%",names(perf.boot8$byClass))
  #sen, spe, ppv, npv by 10%
  pred_.boot10 <- as.factor(ifelse(new.data$predicted.test>0.1,"1","0"))
  perf.boot10 <- confusionMatrix(pred_.boot10, as.factor(new.data$death_1y),positive="1")
  names(perf.boot10$byClass) <- paste("thre_10%",names(perf.boot10$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot5$byClass,perf.boot8$byClass,perf.boot10$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("inpatient.65.Lasso.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## pcr age<45 -----
## XGBoost -----
train <- setDT(read.csv("predicted_proba_train/pcr/predicted_proba_18-44/XGBoost.csv"))
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/XGBoost.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.05%
  pred_.boot0.05 <- as.factor(ifelse(new.data$predicted.test>0.0005,"1","0"))
  perf.boot0.05 <- confusionMatrix(pred_.boot0.05, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.05$byClass) <- paste("thre_0.05%",names(perf.boot0.05$byClass))
  #sen, spe, ppv, npv by 0.1%
  pred_.boot0.1 <- as.factor(ifelse(new.data$predicted.test>0.001,"1","0"))
  perf.boot0.1 <- confusionMatrix(pred_.boot0.1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.1$byClass) <- paste("thre_0.1%",names(perf.boot0.1$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.05$byClass,perf.boot0.1$byClass,perf.boot0.2$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("pcr.age18-44.XGBoost.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## LightGBM -----
train <- setDT(read.csv("predicted_proba_train/pcr/predicted_proba_18-44/LightGBM.csv"))
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/LightGBM.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.05%
  pred_.boot0.05 <- as.factor(ifelse(new.data$predicted.test>0.0005,"1","0"))
  perf.boot0.05 <- confusionMatrix(pred_.boot0.05, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.05$byClass) <- paste("thre_0.05%",names(perf.boot0.05$byClass))
  #sen, spe, ppv, npv by 0.1%
  pred_.boot0.1 <- as.factor(ifelse(new.data$predicted.test>0.001,"1","0"))
  perf.boot0.1 <- confusionMatrix(pred_.boot0.1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.1$byClass) <- paste("thre_0.1%",names(perf.boot0.1$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.05$byClass,perf.boot0.1$byClass,perf.boot0.2$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("pcr.age18-44.LightGBM.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Random Forest -----
train <- setDT(read.csv("predicted_proba_train/pcr/predicted_proba_18-44/Random Forest.csv"))
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/Random Forest.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.05%
  pred_.boot0.05 <- as.factor(ifelse(new.data$predicted.test>0.0005,"1","0"))
  perf.boot0.05 <- confusionMatrix(pred_.boot0.05, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.05$byClass) <- paste("thre_0.05%",names(perf.boot0.05$byClass))
  #sen, spe, ppv, npv by 0.1%
  pred_.boot0.1 <- as.factor(ifelse(new.data$predicted.test>0.001,"1","0"))
  perf.boot0.1 <- confusionMatrix(pred_.boot0.1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.1$byClass) <- paste("thre_0.1%",names(perf.boot0.1$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.05$byClass,perf.boot0.1$byClass,perf.boot0.2$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("pcr.age18-44.Random Forest.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Lasso -----
train <- setDT(read.csv("predicted_proba_train/pcr/predicted_proba_18-44/Lasso.csv"))
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/Lasso.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.05%
  pred_.boot0.05 <- as.factor(ifelse(new.data$predicted.test>0.0005,"1","0"))
  perf.boot0.05 <- confusionMatrix(pred_.boot0.05, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.05$byClass) <- paste("thre_0.05%",names(perf.boot0.05$byClass))
  #sen, spe, ppv, npv by 0.1%
  pred_.boot0.1 <- as.factor(ifelse(new.data$predicted.test>0.001,"1","0"))
  perf.boot0.1 <- confusionMatrix(pred_.boot0.1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.1$byClass) <- paste("thre_0.1%",names(perf.boot0.1$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.05$byClass,perf.boot0.1$byClass,perf.boot0.2$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("pcr.age18-44.Lasso.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## pcr age 45-64 -----
## XGBoost -----
train <- setDT(read.csv("predicted_proba_train/pcr/predicted_proba_45-64/XGBoost.csv"))
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/XGBoost.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #sen, spe, ppv, npv by 0.5%
  pred_.boot0.5 <- as.factor(ifelse(new.data$predicted.test>0.005,"1","0"))
  perf.boot0.5 <- confusionMatrix(pred_.boot0.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.5$byClass) <- paste("thre_0.5%",names(perf.boot0.5$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.2$byClass,perf.boot0.5$byClass,perf.boot1$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("pcr.age45-64.XGBoost.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## LightGBM -----
train <- setDT(read.csv("predicted_proba_train/pcr/predicted_proba_45-64/LightGBM.csv"))
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/LightGBM.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #sen, spe, ppv, npv by 0.5%
  pred_.boot0.5 <- as.factor(ifelse(new.data$predicted.test>0.005,"1","0"))
  perf.boot0.5 <- confusionMatrix(pred_.boot0.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.5$byClass) <- paste("thre_0.5%",names(perf.boot0.5$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.2$byClass,perf.boot0.5$byClass,perf.boot1$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("pcr.age45-64.LightGBM.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Random Forest -----
train <- setDT(read.csv("predicted_proba_train/pcr/predicted_proba_45-64/Random Forest.csv"))
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/Random Forest.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #sen, spe, ppv, npv by 0.5%
  pred_.boot0.5 <- as.factor(ifelse(new.data$predicted.test>0.005,"1","0"))
  perf.boot0.5 <- confusionMatrix(pred_.boot0.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.5$byClass) <- paste("thre_0.5%",names(perf.boot0.5$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.2$byClass,perf.boot0.5$byClass,perf.boot1$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("pcr.age45-64.Random Forest.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Lasso -----
train <- setDT(read.csv("predicted_proba_train/pcr/predicted_proba_45-64/Lasso.csv"))
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/Lasso.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(new.data$predicted.test>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #sen, spe, ppv, npv by 0.5%
  pred_.boot0.5 <- as.factor(ifelse(new.data$predicted.test>0.005,"1","0"))
  perf.boot0.5 <- confusionMatrix(pred_.boot0.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.5$byClass) <- paste("thre_0.5%",names(perf.boot0.5$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(new.data$predicted.test>0.01,"1","0"))
  perf.boot1 <- confusionMatrix(pred_.boot1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot1$byClass) <- paste("thre_1%",names(perf.boot1$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot0.2$byClass,perf.boot0.5$byClass,perf.boot1$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("pcr.age45-64.Lasso.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## pcr age 65+ -----
## XGBoost -----
train <- setDT(read.csv("predicted_proba_train/pcr/predicted_proba_65/XGBoost.csv"))
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/XGBoost.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 2%
  pred_.boot2 <- as.factor(ifelse(new.data$predicted.test>0.02,"1","0"))
  perf.boot2 <- confusionMatrix(pred_.boot2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot2$byClass) <- paste("thre_2%",names(perf.boot2$byClass))
  #sen, spe, ppv, npv by 4%
  pred_.boot4 <- as.factor(ifelse(new.data$predicted.test>0.04,"1","0"))
  perf.boot4 <- confusionMatrix(pred_.boot4, as.factor(new.data$death_1y),positive="1")
  names(perf.boot4$byClass) <- paste("thre_4%",names(perf.boot4$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot2$byClass,perf.boot4$byClass,perf.boot5$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("pcr.age65.XGBoost.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## LightGBM -----
train <- setDT(read.csv("predicted_proba_train/pcr/predicted_proba_65/LightGBM.csv"))
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/LightGBM.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 2%
  pred_.boot2 <- as.factor(ifelse(new.data$predicted.test>0.02,"1","0"))
  perf.boot2 <- confusionMatrix(pred_.boot2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot2$byClass) <- paste("thre_2%",names(perf.boot2$byClass))
  #sen, spe, ppv, npv by 4%
  pred_.boot4 <- as.factor(ifelse(new.data$predicted.test>0.04,"1","0"))
  perf.boot4 <- confusionMatrix(pred_.boot4, as.factor(new.data$death_1y),positive="1")
  names(perf.boot4$byClass) <- paste("thre_4%",names(perf.boot4$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot2$byClass,perf.boot4$byClass,perf.boot5$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("pcr.age65.LightGBM.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Random Forest -----
train <- setDT(read.csv("predicted_proba_train/pcr/predicted_proba_65/Random Forest.csv"))
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/Random Forest.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 2%
  pred_.boot2 <- as.factor(ifelse(new.data$predicted.test>0.02,"1","0"))
  perf.boot2 <- confusionMatrix(pred_.boot2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot2$byClass) <- paste("thre_2%",names(perf.boot2$byClass))
  #sen, spe, ppv, npv by 4%
  pred_.boot4 <- as.factor(ifelse(new.data$predicted.test>0.04,"1","0"))
  perf.boot4 <- confusionMatrix(pred_.boot4, as.factor(new.data$death_1y),positive="1")
  names(perf.boot4$byClass) <- paste("thre_4%",names(perf.boot4$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot2$byClass,perf.boot4$byClass,perf.boot5$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("pcr.age65.Random Forest.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

## Lasso -----
train <- setDT(read.csv("predicted_proba_train/pcr/predicted_proba_65/Lasso.csv"))
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/Lasso.csv"))
colnames(train) <- c("death_1y","predicted.train")
colnames(test) <- c("death_1y","predicted.test")
roc.train <- roc(train$death_1y,train$predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(test$predicted.test>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
roc.test <- roc(test$death_1y,test$predicted.test)

# area under precision-recall plot 
library(PRROC)
pr.test <- pr.curve(scores.class0=test[death_1y==1]$predicted.test, scores.class1=test[death_1y==0]$predicted.test,curve=T,
                    max.compute = T, min.compute = T, rand.compute = T)

pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr,"aucpr")

## bootstrap CI for multiple measures
library(boot)
perf.f <- function(data, indices){
  new.data <- data[indices,]
  #sen, spe, ppv, npv by best threshold
  pred_.boot <- as.factor(ifelse(new.data$predicted.test>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 2%
  pred_.boot2 <- as.factor(ifelse(new.data$predicted.test>0.02,"1","0"))
  perf.boot2 <- confusionMatrix(pred_.boot2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot2$byClass) <- paste("thre_2%",names(perf.boot2$byClass))
  #sen, spe, ppv, npv by 4%
  pred_.boot4 <- as.factor(ifelse(new.data$predicted.test>0.04,"1","0"))
  perf.boot4 <- confusionMatrix(pred_.boot4, as.factor(new.data$death_1y),positive="1")
  names(perf.boot4$byClass) <- paste("thre_4%",names(perf.boot4$byClass))
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(new.data$predicted.test>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot2$byClass,perf.boot4$byClass,perf.boot5$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=perf.f, R = 1000)
names(boot.perf$t0) <- c(names(boot.perf$t0)[1:(length(names(boot.perf$t0))-2)],"auc.roc","auc.pr")
perf.ci <- c()
for(i in 1:length(names(boot.perf$t0))){
  value.ci <- c(names(boot.perf$t0[i]),boot.perf$t0[i],boot.ci(boot.perf, type="perc",index=i)$percent[,4:5])
  perf.ci <- rbind(perf.ci,value.ci)
}
perf.ci

sink("pcr.age65.Lasso.txt")
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()
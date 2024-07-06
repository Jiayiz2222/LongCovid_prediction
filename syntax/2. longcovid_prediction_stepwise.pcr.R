setwd("/mnt/A/long_covid_jiayi/jiayi/operation/longcovid_prediction")
library(data.table)
library(tableone)
require(caret)
library(StepReg)
library(pROC)
library(ROCR)

##age<45
train <- readRDS("train.pcr_18-44.RDS")
test <- readRDS("test.pcr_18-44.RDS")
summary(train)
train <- train[,-c("COVID.test","inpatient","ild","major","ards","pcr","ageg","pd","bp","anxiety","htn","cbc.wbc","cbc.neuphil","cbc.plt","inflam.crp","inflam.ldh")]

var <- names(train)
var <- var[!var %in% c("id","death_1y")]
model.train <- stepwiseLogit(as.formula(paste0("death_1y~",paste(var,collapse = "+"))),data = train,
                             selection = "backward",select = "SBC")
save(model.train,file="pcr.age18-44.BW.Rdata")
# load("pcr.age18-44.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
or <- exp(cbind(coef(model.OR),confint.default(model.OR)))
predicted.train <- predict(model.OR,train, type="response")
roc.train <- roc(train$death_1y,predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(predict(model.OR, test, type="response")>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
predicted.test <-  predict(model.OR,test, type="response")
test$predicted.test <-  predict(model.OR,test, type="response")
roc.test <- roc(test$death_1y,test$predicted.test)

## check collinearity
library(performance)
check_collinearity(model.OR)

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
  pred_.boot <- as.factor(ifelse(predict(model.OR, new.data, type="response")>thre[1,],"1","0"))
  perf.boot <- confusionMatrix(pred_.boot, as.factor(new.data$death_1y),positive="1")
  names(perf.boot$byClass) <- paste("thre_best",names(perf.boot$byClass))
  #sen, spe, ppv, npv by 0.05%
  pred_.boot0.05 <- as.factor(ifelse(predict(model.OR, new.data, type="response")>0.0005,"1","0"))
  perf.boot0.05 <- confusionMatrix(pred_.boot0.05, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.05$byClass) <- paste("thre_0.05%",names(perf.boot0.05$byClass))
  #sen, spe, ppv, npv by 0.1%
  pred_.boot0.1 <- as.factor(ifelse(predict(model.OR, new.data, type="response")>0.001,"1","0"))
  perf.boot0.1 <- confusionMatrix(pred_.boot0.1, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.1$byClass) <- paste("thre_0.1%",names(perf.boot0.1$byClass))
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(predict(model.OR, new.data, type="response")>0.002,"1","0"))
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

sink("pcr.age18-44.BW.txt")
print(or)
print(summary(model.OR))
check_collinearity(model.OR)
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

##age 45-64
train <- readRDS("train.pcr_45-64.RDS")
test <- readRDS("test.pcr_45-64.RDS")
summary(train)
train <- train[,-c("COVID.test","inpatient","major","ards","pcr","ageg","pd","bp","anxiety","htn","cbc.wbc","cbc.neuphil","cbc.plt","inflam.crp","inflam.ldh")]

var <- names(train)
var <- var[!var %in% c("id","death_1y")]
model.train <- stepwiseLogit(as.formula(paste0("death_1y~",paste(var,collapse = "+"))),data = train,
                             selection = "backward",select = "SBC")
save(model.train,file="pcr.age45-64.BW.Rdata")
# load("pcr.age45-64.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
or <- exp(cbind(coef(model.OR),confint.default(model.OR)))
predicted.train <- predict(model.OR,train, type="response")
roc.train <- roc(train$death_1y,predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(predict(model.OR, test, type="response")>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
predicted.test <-  predict(model.OR,test, type="response")
test$predicted.test <-  predict(model.OR,test, type="response")
roc.test <- roc(test$death_1y,test$predicted.test)

## check collinearity
library(performance)
check_collinearity(model.OR)

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

sink("pcr.age45-64.backward.txt")
print(or)
print(summary(model.OR))
check_collinearity(model.OR)
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()

##age>=65
train <- readRDS("train.pcr_65+.RDS")
test <- readRDS("test.pcr_65+.RDS")
summary(train)
train <- train[,-c("COVID.test","inpatient","major","ards","pcr","ageg","pd","bp","anxiety","htn","cbc.wbc","cbc.neuphil","cbc.plt","inflam.crp","inflam.ldh")]

var <- names(train)
var <- var[!var %in% c("id","death_1y")]
model.train <- stepwiseLogit(as.formula(paste0("death_1y~",paste(var,collapse = "+"))),data = train,
                             selection = "backward",select = "SBC")
save(model.train,file="pcr.age65.BW.Rdata")
# load("pcr.age65.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
or <- exp(cbind(coef(model.OR),confint.default(model.OR)))
predicted.train <- predict(model.OR,train, type="response")
roc.train <- roc(train$death_1y,predicted.train)
thre <- coords(roc.train, "best", ret = "threshold")
pred_ <- as.factor(ifelse(predict(model.OR, test, type="response")>thre[1,],"1","0"))
perf <- confusionMatrix(pred_, as.factor(test$death_1y),positive="1")
predicted.test <-  predict(model.OR,test, type="response")
test$predicted.test <-  predict(model.OR,test, type="response")
roc.test <- roc(test$death_1y,test$predicted.test)

## check collinearity
library(performance)
check_collinearity(model.OR)

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

sink("pcr.age65.backward.txt")
print(or)
print(summary(model.OR))
check_collinearity(model.OR)
print(paste("threshold of Train set",thre))
print("---Performance of Test set-----")
print(paste("AUC of Test set:",roc.test$auc))
print(ci(roc.test))
print("--package PRROC---")
print(pr.test)
print("--package ROCR---")
print(perf.ci)
sink()


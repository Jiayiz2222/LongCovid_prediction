setwd("/mnt/A/long_covid_jiayi/jiayi/operation/longcovid_prediction")
library(data.table)
library(tableone)
require(caret)
library(StepReg)
library(pROC)
library(ROCR)

##age<45
train <- readRDS("train.inpatient_18-44.RDS")
test <- readRDS("test.inpatient_18-44.RDS")
summary(train)
train <- train[,-c("COVID.test","ild","major","ards","inpatient","ageg","pd","bp","anxiety","htn","cbc.wbc","cbc.neuphil","cbc.plt","inflam.crp","inflam.ldh")]

var <- names(train)
var <- var[!var %in% c("id","death_1y")]
model.train <- stepwiseLogit(as.formula(paste0("death_1y~",paste(var,collapse = "+"))),data = train,
                             selection = "backward",select = "SBC")
save(model.train,file="inpatient.age18-44.BW.Rdata")
# load("inpatient.age18-44.BW.Rdata")
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
  #sen, spe, ppv, npv by 0.2%
  pred_.boot0.2 <- as.factor(ifelse(predict(model.OR, new.data, type="response")>0.002,"1","0"))
  perf.boot0.2 <- confusionMatrix(pred_.boot0.2, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.2$byClass) <- paste("thre_0.2%",names(perf.boot0.2$byClass))
  #sen, spe, ppv, npv by 0.5%
  pred_.boot0.5 <- as.factor(ifelse(predict(model.OR, new.data, type="response")>0.005,"1","0"))
  perf.boot0.5 <- confusionMatrix(pred_.boot0.5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot0.5$byClass) <- paste("thre_0.5%",names(perf.boot0.5$byClass))
  #sen, spe, ppv, npv by 1%
  pred_.boot1 <- as.factor(ifelse(predict(model.OR, new.data, type="response")>0.01,"1","0"))
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

sink("inpatient.age18-44.BW.txt")
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
train <- readRDS("train.inpatient_45-64.RDS")
test <- readRDS("test.inpatient_45-64.RDS")
summary(train)
train <- train[,-c("COVID.test","major","ards","inpatient","ageg","pd","bp","anxiety","htn","cbc.wbc","cbc.neuphil","cbc.plt","inflam.crp","inflam.ldh")]

var <- names(train)
var <- var[!var %in% c("id","death_1y")]
model.train <- stepwiseLogit(as.formula(paste0("death_1y~",paste(var,collapse = "+"))),data = train,
                             selection = "backward",select = "SBC")
save(model.train,file="inpatient.age45-64.BW.Rdata")
# load("inpatient.age45-64.BW.Rdata")
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
  #sen, spe, ppv, npv by 5%
  pred_.boot5 <- as.factor(ifelse(predict(model.OR, new.data, type="response")>0.05,"1","0"))
  perf.boot5 <- confusionMatrix(pred_.boot5, as.factor(new.data$death_1y),positive="1")
  names(perf.boot5$byClass) <- paste("thre_5%",names(perf.boot5$byClass))
  #sen, spe, ppv, npv by 8%
  pred_.boot8 <- as.factor(ifelse(predict(model.OR, new.data, type="response")>0.08,"1","0"))
  perf.boot8 <- confusionMatrix(pred_.boot8, as.factor(new.data$death_1y),positive="1")
  names(perf.boot8$byClass) <- paste("thre_8%",names(perf.boot8$byClass))
  #sen, spe, ppv, npv by 10%
  pred_.boot10 <- as.factor(ifelse(predict(model.OR, new.data, type="response")>0.1,"1","0"))
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

sink("inpatient.age45-64.backward.txt")
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
train <- readRDS("train.inpatient_65+.RDS")
test <- readRDS("test.inpatient_65+.RDS")
summary(train)
train <- train[,-c("COVID.test","major","ards","inpatient","ageg","pd","bp","anxiety","htn","cbc.wbc","cbc.neuphil","cbc.plt","inflam.crp","inflam.ldh")]

var <- names(train)
var <- var[!var %in% c("id","death_1y")]
model.train <- stepwiseLogit(as.formula(paste0("death_1y~",paste(var,collapse = "+"))),data = train,
                             selection = "backward",select = "SBC")
save(model.train,file="inpatient.age65.BW.Rdata")
# load("inpatient.age65.BW.Rdata"")
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
  #sen, spe, ppv, npv by 15%
  pred_.boot15 <- as.factor(ifelse(predict(model.OR, new.data, type="response")>0.15,"1","0"))
  perf.boot15 <- confusionMatrix(pred_.boot15, as.factor(new.data$death_1y),positive="1")
  names(perf.boot15$byClass) <- paste("thre_15%",names(perf.boot15$byClass))
  #sen, spe, ppv, npv by 20%
  pred_.boot20 <- as.factor(ifelse(predict(model.OR, new.data, type="response")>0.2,"1","0"))
  perf.boot20 <- confusionMatrix(pred_.boot20, as.factor(new.data$death_1y),positive="1")
  names(perf.boot20$byClass) <- paste("thre_20%",names(perf.boot20$byClass))
  #sen, spe, ppv, npv by 30%
  pred_.boot25 <- as.factor(ifelse(predict(model.OR, new.data, type="response")>0.25,"1","0"))
  perf.boot25 <- confusionMatrix(pred_.boot25, as.factor(new.data$death_1y),positive="1")
  names(perf.boot25$byClass) <- paste("thre_25%",names(perf.boot25$byClass))
  #auc.roc
  roc.test.boot <- roc(new.data$death_1y,new.data$predicted.test)
  #auc.pr
  pred.boot <- prediction(new.data$predicted.test, new.data$death_1y)
  aucpr <- ROCR::performance(pred.boot,"aucpr")
  return(c(perf.boot$byClass,perf.boot15$byClass,perf.boot20$byClass,perf.boot25$byClass,roc.test.boot$auc,aucpr@y.values[[1]]))
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

sink("inpatient.age65.backward.txt")
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


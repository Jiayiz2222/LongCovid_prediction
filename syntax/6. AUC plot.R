setwd("/mnt/A/long_covid_jiayi/jiayi/operation/longcovid_prediction")
library(data.table)
require(ggplot)
library(pROC)
library(ROCR)

### AUROC ----
## age 18-44 #####
#logistic
train <- readRDS("train.18-44.RDS")
test <- readRDS("test.18-44.RDS")
load("age18-44.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
roc.test <- roc(test$death_1y,test$predicted.test,ci=T)

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/predicted_proba_18-44/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
roc.test.xgboost <- roc(test.xgboost$death_1y,test.xgboost$predicted.test,ci=T)

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/predicted_proba_18-44/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
roc.test.lgbm <- roc(test.lgbm$death_1y,test.lgbm$predicted.test,ci=T)

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/predicted_proba_18-44/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
roc.test.rf <- roc(test.rf$death_1y,test.rf$predicted.test,ci=T)

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/predicted_proba_18-44/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
roc.test.lasso <- roc(test.lasso$death_1y,test.lasso$predicted.test,ci=T)

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(roc.test)
plot(roc.test.xgboost,col=cols[4],add=T)
plot(roc.test.lgbm,col=cols[3],add=T)
plot(roc.test.rf,col=cols[2],add=T)
plot(roc.test.lasso,col=cols[1],add=T)
text(0.33,0.35,"Logistic (AUROC 0.79, 95%CI 0.72-0.86)",xpd=TRUE,cex=0.9)
text(0.325,0.30,"XGBoost (AUROC 0.77, 95%CI 0.69-0.84)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.317,0.25,"LightGBM (AUROC 0.78, 95%CI 0.70-0.86)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.28,0.2,"Random Forest (AUROC 0.79, 95%CI 0.71-0.86)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.345,0.15,"Lasso (AUROC 0.81, 95%CI 0.74-0.88)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for Age 18-44",xpd=TRUE,cex=1.3)


## age 45-64 #####
#logistic
train <- readRDS("train.45-64.RDS")
test <- readRDS("test.45-64.RDS")
load("age45-64.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
roc.test <- roc(test$death_1y,test$predicted.test,ci=T)

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/predicted_proba_45-64/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
roc.test.xgboost <- roc(test.xgboost$death_1y,test.xgboost$predicted.test,ci=T)

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/predicted_proba_45-64/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
roc.test.lgbm <- roc(test.lgbm$death_1y,test.lgbm$predicted.test,ci=T)

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/predicted_proba_45-64/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
roc.test.rf <- roc(test.rf$death_1y,test.rf$predicted.test,ci=T)

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/predicted_proba_45-64/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
roc.test.lasso <- roc(test.lasso$death_1y,test.lasso$predicted.test,ci=T)

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(roc.test)
plot(roc.test.xgboost,col=cols[4],add=T)
plot(roc.test.lgbm,col=cols[3],add=T)
plot(roc.test.rf,col=cols[2],add=T)
plot(roc.test.lasso,col=cols[1],add=T)
text(0.33,0.35,"Logistic (AUROC 0.83, 95%CI 0.81-0.85)",xpd=TRUE,cex=0.9)
text(0.325,0.30,"XGBoost (AUROC 0.83, 95%CI 0.81-0.86)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.317,0.25,"LightGBM (AUROC 0.83, 95%CI 0.80-0.85)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.28,0.2,"Random Forest (AUROC 0.83, 95%CI 0.81-0.85)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.345,0.15,"Lasso (AUROC 0.83, 95%CI 0.81-0.85)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for Age 45-64",xpd=TRUE,cex=1.3)

## age 65+ #####
#logistic
train <- readRDS("train.65.RDS")
test <- readRDS("test.65.RDS")
load("age65.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
roc.test <- roc(test$death_1y,test$predicted.test,ci=T)

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/predicted_proba_65/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
roc.test.xgboost <- roc(test.xgboost$death_1y,test.xgboost$predicted.test,ci=T)

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/predicted_proba_65/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
roc.test.lgbm <- roc(test.lgbm$death_1y,test.lgbm$predicted.test,ci=T)

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/predicted_proba_65/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
roc.test.rf <- roc(test.rf$death_1y,test.rf$predicted.test,ci=T)

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/predicted_proba_65/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
roc.test.lasso <- roc(test.lasso$death_1y,test.lasso$predicted.test,ci=T)

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(roc.test)
plot(roc.test.xgboost,col=cols[4],add=T)
plot(roc.test.lgbm,col=cols[3],add=T)
plot(roc.test.rf,col=cols[2],add=T)
plot(roc.test.lasso,col=cols[1],add=T)
text(0.33,0.35,"Logistic (AUROC 0.87, 95%CI 0.87-0.88)",xpd=TRUE,cex=0.9)
text(0.325,0.30,"XGBoost (AUROC 0.87, 95%CI 0.87-0.88)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.317,0.25,"LightGBM (AUROC 0.87, 95%CI 0.87-0.88)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.28,0.2,"Random Forest (AUROC 0.86, 95%CI 0.86-0.87)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.345,0.15,"Lasso (AUROC 0.87, 95%CI 0.86-0.87)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for Age 65+",xpd=TRUE,cex=1.3)

## age all #####
#logistic
train <- readRDS("train.RDS")
test <- readRDS("test.RDS")
load("allage.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
roc.test <- roc(test$death_1y,test$predicted.test,ci=T)

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/predicted_proba/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
roc.test.xgboost <- roc(test.xgboost$death_1y,test.xgboost$predicted.test,ci=T)

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/predicted_proba/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
roc.test.lgbm <- roc(test.lgbm$death_1y,test.lgbm$predicted.test,ci=T)

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/predicted_proba/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
roc.test.rf <- roc(test.rf$death_1y,test.rf$predicted.test,ci=T)

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/predicted_proba/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
roc.test.lasso <- roc(test.lasso$death_1y,test.lasso$predicted.test,ci=T)

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(roc.test)
plot(roc.test.xgboost,col=cols[4],add=T)
plot(roc.test.lgbm,col=cols[3],add=T)
plot(roc.test.rf,col=cols[2],add=T)
plot(roc.test.lasso,col=cols[1],add=T)
text(0.33,0.35,"Logistic (AUROC 0.93, 95%CI 0.93-0.94)",xpd=TRUE,cex=0.9)
text(0.325,0.30,"XGBoost (AUROC 0.94, 95%CI 0.93-0.94)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.317,0.25,"LightGBM (AUROC 0.93, 95%CI 0.93-0.94)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.28,0.2,"Random Forest (AUROC 0.92, 95%CI 0.92-0.93)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.345,0.15,"Lasso (AUROC 0.93, 95%CI 0.93-0.94)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for all patients",xpd=TRUE,cex=1.3)

## age inpatient 18-44 #####
#logistic
train <- readRDS("train.inpatient_18-44.RDS")
test <- readRDS("test.inpatient_18-44.RDS")
load("inpatient.age18-44.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
roc.test <- roc(test$death_1y,test$predicted.test,ci=T)

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
roc.test.xgboost <- roc(test.xgboost$death_1y,test.xgboost$predicted.test,ci=T)

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
roc.test.lgbm <- roc(test.lgbm$death_1y,test.lgbm$predicted.test,ci=T)

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
roc.test.rf <- roc(test.rf$death_1y,test.rf$predicted.test,ci=T)

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
roc.test.lasso <- roc(test.lasso$death_1y,test.lasso$predicted.test,ci=T)

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(roc.test)
plot(roc.test.xgboost,col=cols[4],add=T)
plot(roc.test.lgbm,col=cols[3],add=T)
plot(roc.test.rf,col=cols[2],add=T)
plot(roc.test.lasso,col=cols[1],add=T)
text(0.33,0.35,"Logistic (AUROC 0.81, 95%CI 0.68-0.93)",xpd=TRUE,cex=0.9)
text(0.325,0.30,"XGBoost (AUROC 0.84, 95%CI 0.75-0.92)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.317,0.25,"LightGBM (AUROC 0.77, 95%CI 0.64-0.89)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.28,0.2,"Random Forest (AUROC 0.81, 95%CI 0.72-0.91)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.347,0.15,"Lasso (AUROC 0.82, 95%CI 0.70-0.93)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for hospitalized patients aged 18-44",xpd=TRUE,cex=1.3)

## age inpatient 45-64 #####
#logistic
train <- readRDS("train.inpatient_45-64.RDS")
test <- readRDS("test.inpatient_45-64.RDS")
load("inpatient.age45-64.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
roc.test <- roc(test$death_1y,test$predicted.test,ci=T)

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
roc.test.xgboost <- roc(test.xgboost$death_1y,test.xgboost$predicted.test,ci=T)

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
roc.test.lgbm <- roc(test.lgbm$death_1y,test.lgbm$predicted.test,ci=T)

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
roc.test.rf <- roc(test.rf$death_1y,test.rf$predicted.test,ci=T)

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
roc.test.lasso <- roc(test.lasso$death_1y,test.lasso$predicted.test,ci=T)

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(roc.test)
plot(roc.test.xgboost,col=cols[4],add=T)
plot(roc.test.lgbm,col=cols[3],add=T)
plot(roc.test.rf,col=cols[2],add=T)
plot(roc.test.lasso,col=cols[1],add=T)
text(0.33,0.35,"Logistic (AUROC 0.73, 95%CI 0.69-0.77)",xpd=TRUE,cex=0.9)
text(0.325,0.30,"XGBoost (AUROC 0.77, 95%CI 0.73-0.80)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.317,0.25,"LightGBM (AUROC 0.77, 95%CI 0.73-0.80)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.28,0.2,"Random Forest (AUROC 0.76, 95%CI 0.73-0.80)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.347,0.15,"Lasso (AUROC 0.74, 95%CI 0.70-0.77)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for hospitalized patients aged 45-64",xpd=TRUE,cex=1.3)

## age inpatient 65+ #####
#logistic
train <- readRDS("train.inpatient_65+.RDS")
test <- readRDS("test.inpatient_65+.RDS")
load("inpatient.age65.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
roc.test <- roc(test$death_1y,test$predicted.test,ci=T)

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
roc.test.xgboost <- roc(test.xgboost$death_1y,test.xgboost$predicted.test,ci=T)

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
roc.test.lgbm <- roc(test.lgbm$death_1y,test.lgbm$predicted.test,ci=T)

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
roc.test.rf <- roc(test.rf$death_1y,test.rf$predicted.test,ci=T)

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
roc.test.lasso <- roc(test.lasso$death_1y,test.lasso$predicted.test,ci=T)

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(roc.test)
plot(roc.test.xgboost,col=cols[4],add=T)
plot(roc.test.lgbm,col=cols[3],add=T)
plot(roc.test.rf,col=cols[2],add=T)
plot(roc.test.lasso,col=cols[1],add=T)
text(0.33,0.35,"Logistic (AUROC 0.72, 95%CI 0.70-0.73)",xpd=TRUE,cex=0.9)
text(0.325,0.30,"XGBoost (AUROC 0.72, 95%CI 0.71-0.73)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.317,0.25,"LightGBM (AUROC 0.72, 95%CI 0.71-0.73)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.28,0.2,"Random Forest (AUROC 0.71, 95%CI 0.70-0.73)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.347,0.15,"Lasso (AUROC 0.72, 95%CI 0.70-0.73)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for hospitalized patients aged 65+",xpd=TRUE,cex=1.3)

## PCR age 18-44 #####
#logistic
train <- readRDS("train.pcr_18-44.RDS")
test <- readRDS("test.pcr_18-44.RDS")
load("pcr.age18-44.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
roc.test <- roc(test$death_1y,test$predicted.test,ci=T)

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
roc.test.xgboost <- roc(test.xgboost$death_1y,test.xgboost$predicted.test,ci=T)

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
roc.test.lgbm <- roc(test.lgbm$death_1y,test.lgbm$predicted.test,ci=T)

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
roc.test.rf <- roc(test.rf$death_1y,test.rf$predicted.test,ci=T)

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
roc.test.lasso <- roc(test.lasso$death_1y,test.lasso$predicted.test,ci=T)

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(roc.test)
plot(roc.test.xgboost,col=cols[4],add=T)
plot(roc.test.lgbm,col=cols[3],add=T)
plot(roc.test.rf,col=cols[2],add=T)
plot(roc.test.lasso,col=cols[1],add=T)
text(0.33,0.35,"Logistic (AUROC 0.75, 95%CI 0.66-0.82)",xpd=TRUE,cex=0.9)
text(0.325,0.30,"XGBoost (AUROC 0.73, 95%CI 0.65-0.80)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.317,0.25,"LightGBM (AUROC 0.73, 95%CI 0.65-0.80)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.28,0.2,"Random Forest (AUROC 0.75, 95%CI 0.67-0.83)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.345,0.15,"Lasso (AUROC 0.74, 95%CI 0.66-0.81)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for patients with PCR test aged 18-44",xpd=TRUE,cex=1.3)

## PCR age 45-64 #####
#logistic
train <- readRDS("train.pcr_45-64.RDS")
test <- readRDS("test.pcr_45-64.RDS")
load("pcr.age45-64.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
roc.test <- roc(test$death_1y,test$predicted.test,ci=T)

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
roc.test.xgboost <- roc(test.xgboost$death_1y,test.xgboost$predicted.test,ci=T)

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
roc.test.lgbm <- roc(test.lgbm$death_1y,test.lgbm$predicted.test,ci=T)

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
roc.test.rf <- roc(test.rf$death_1y,test.rf$predicted.test,ci=T)

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
roc.test.lasso <- roc(test.lasso$death_1y,test.lasso$predicted.test,ci=T)

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(roc.test)
plot(roc.test.xgboost,col=cols[4],add=T)
plot(roc.test.lgbm,col=cols[3],add=T)
plot(roc.test.rf,col=cols[2],add=T)
plot(roc.test.lasso,col=cols[1],add=T)
text(0.33,0.35,"Logistic (AUROC 0.85, 95%CI 0.82-0.87)",xpd=TRUE,cex=0.9)
text(0.325,0.30,"XGBoost (AUROC 0.84, 95%CI 0.82-0.87)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.317,0.25,"LightGBM (AUROC 0.85, 95%CI 0.82-0.87)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.28,0.2,"Random Forest (AUROC 0.84, 95%CI 0.82-0.86)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.345,0.15,"Lasso (AUROC 0.84, 95%CI 0.82-0.86)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for patients with PCR test aged 45-64",xpd=TRUE,cex=1.3)

## PCR age 65+ #####
#logistic
train <- readRDS("train.pcr_65+.RDS")
test <- readRDS("test.pcr_65+.RDS")
load("pcr.age65.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
roc.test <- roc(test$death_1y,test$predicted.test,ci=T)

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
roc.test.xgboost <- roc(test.xgboost$death_1y,test.xgboost$predicted.test,ci=T)

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
roc.test.lgbm <- roc(test.lgbm$death_1y,test.lgbm$predicted.test,ci=T)

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
roc.test.rf <- roc(test.rf$death_1y,test.rf$predicted.test,ci=T)

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
roc.test.lasso <- roc(test.lasso$death_1y,test.lasso$predicted.test,ci=T)

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(roc.test)
plot(roc.test.xgboost,col=cols[4],add=T)
plot(roc.test.lgbm,col=cols[3],add=T)
plot(roc.test.rf,col=cols[2],add=T)
plot(roc.test.lasso,col=cols[1],add=T)
text(0.33,0.35,"Logistic (AUROC 0.87, 95%CI 0.87-0.88)",xpd=TRUE,cex=0.9)
text(0.325,0.30,"XGBoost (AUROC 0.87, 95%CI 0.87-0.88)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.317,0.25,"LightGBM (AUROC 0.88, 95%CI 0.87-0.89)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.28,0.2,"Random Forest (AUROC 0.87, 95%CI 0.86-0.88)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.345,0.15,"Lasso (AUROC 0.87, 95%CI 0.87-0.88)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for patients with PCR test aged 65+",xpd=TRUE,cex=1.3)

### AUPRC ----
## age 18-44 #####
#logistic
train <- readRDS("train.18-44.RDS")
test <- readRDS("test.18-44.RDS")
load("age18-44.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr, "prec", "rec")

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/predicted_proba_18-44/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
pred.pr.xgboost <- prediction(test.xgboost$predicted.test, test.xgboost$death_1y)
aucpr.xgboost <- ROCR::performance(pred.pr.xgboost, "prec", "rec")

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/predicted_proba_18-44/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
pred.pr.lgbm <- prediction(test.lgbm$predicted.test, test.lgbm$death_1y)
aucpr.lgbm <- ROCR::performance(pred.pr.lgbm, "prec", "rec")

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/predicted_proba_18-44/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
pred.pr.rf <- prediction(test.rf$predicted.test, test.rf$death_1y)
aucpr.rf <- ROCR::performance(pred.pr.rf, "prec", "rec")

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/predicted_proba_18-44/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
pred.pr.lasso <- prediction(test.lasso$predicted.test, test.lasso$death_1y)
aucpr.lasso <- ROCR::performance(pred.pr.lasso, "prec", "rec")

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(aucpr.lasso,col=cols[1])
plot(aucpr.lgbm,col=cols[3],add=T)
plot(aucpr,add=T)
plot(aucpr.xgboost,col=cols[4],add=T)
plot(aucpr.rf,col=cols[2],add=T)
text(0.65,0.15,"Logistic (AUPRC 0.02, 95%CI 0.01-0.04)",xpd=TRUE,cex=0.9)
text(0.658,0.13,"XGBoost (AUPRC 0.02, 95%CI 0.01-0.05)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.6645,0.11,"LightGBM (AUPRC 0.02, 95%CI 0.01-0.04)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.7,0.09,"Random Forest (AUPRC 0.02, 95%CI 0.01-0.04)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.64,0.07,"Lasso (AUPRC 0.03, 95%CI 0.01-0.07)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,0.36,"Models for Age 18-44",xpd=TRUE,cex=1.3)


## age 45-64 #####
#logistic
train <- readRDS("train.45-64.RDS")
test <- readRDS("test.45-64.RDS")
load("age45-64.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr, "prec", "rec")

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/predicted_proba_45-64/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
pred.pr.xgboost <- prediction(test.xgboost$predicted.test, test.xgboost$death_1y)
aucpr.xgboost <- ROCR::performance(pred.pr.xgboost, "prec", "rec")

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/predicted_proba_45-64/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
pred.pr.lgbm <- prediction(test.lgbm$predicted.test, test.lgbm$death_1y)
aucpr.lgbm <- ROCR::performance(pred.pr.lgbm, "prec", "rec")

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/predicted_proba_45-64/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
pred.pr.rf <- prediction(test.rf$predicted.test, test.rf$death_1y)
aucpr.rf <- ROCR::performance(pred.pr.rf, "prec", "rec")

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/predicted_proba_45-64/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
pred.pr.lasso <- prediction(test.lasso$predicted.test, test.lasso$death_1y)
aucpr.lasso <- ROCR::performance(pred.pr.lasso, "prec", "rec")

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(aucpr.lasso,col=cols[1])
plot(aucpr.lgbm,col=cols[3],add=T)
plot(aucpr,add=T)
plot(aucpr.xgboost,col=cols[4],add=T)
plot(aucpr.rf,col=cols[2],add=T)
text(0.6,0.85,"Logistic (AUPRC 0.10, 95%CI 0.07-0.13)",xpd=TRUE,cex=0.9)
text(0.608,0.78,"XGBoost (AUPRC 0.11, 95%CI 0.08-0.14)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.616,0.71,"LightGBM (AUPRC 0.09, 95%CI 0.07-0.12)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.651,0.64,"Random Forest (AUPRC 0.10, 95%CI 0.07-0.13)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.591,0.57,"Lasso (AUPRC 0.09, 95%CI 0.07-0.12)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for Age 45-64",xpd=TRUE,cex=1.3)

## age 65+ #####
#logistic
train <- readRDS("train_65.RDS")
test <- readRDS("test_65.RDS")
load("age65.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr, "prec", "rec")

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/predicted_proba_65/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
pred.pr.xgboost <- prediction(test.xgboost$predicted.test, test.xgboost$death_1y)
aucpr.xgboost <- ROCR::performance(pred.pr.xgboost, "prec", "rec")

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/predicted_proba_65/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
pred.pr.lgbm <- prediction(test.lgbm$predicted.test, test.lgbm$death_1y)
aucpr.lgbm <- ROCR::performance(pred.pr.lgbm, "prec", "rec")

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/predicted_proba_65/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
pred.pr.rf <- prediction(test.rf$predicted.test, test.rf$death_1y)
aucpr.rf <- ROCR::performance(pred.pr.rf, "prec", "rec")

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/predicted_proba_65/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
pred.pr.lasso <- prediction(test.lasso$predicted.test, test.lasso$death_1y)
aucpr.lasso <- ROCR::performance(pred.pr.lasso, "prec", "rec")

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(aucpr.lasso,col=cols[1])
plot(aucpr.lgbm,col=cols[3],add=T)
plot(aucpr,add=T)
plot(aucpr.xgboost,col=cols[4],add=T)
plot(aucpr.rf,col=cols[2],add=T)
text(0.6,0.85,"Logistic (AUPRC 0.29, 95%CI 0.27-0.30)",xpd=TRUE,cex=0.9)
text(0.608,0.79,"XGBoost (AUPRC 0.28, 95%CI 0.26-0.29)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.614,0.73,"LightGBM (AUPRC 0.28, 95%CI 0.26-0.29)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.65,0.67,"Random Forest (AUPRC 0.26, 95%CI 0.25-0.27)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.591,0.61,"Lasso (AUPRC 0.27, 95%CI 0.26-0.29)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for Age 65+",xpd=TRUE,cex=1.3)

## age all #####
#logistic
train <- readRDS("train.RDS")
test <- readRDS("test.RDS")
load("allage.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr, "prec", "rec")

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/predicted_proba/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
pred.pr.xgboost <- prediction(test.xgboost$predicted.test, test.xgboost$death_1y)
aucpr.xgboost <- ROCR::performance(pred.pr.xgboost, "prec", "rec")

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/predicted_proba/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
pred.pr.lgbm <- prediction(test.lgbm$predicted.test, test.lgbm$death_1y)
aucpr.lgbm <- ROCR::performance(pred.pr.lgbm, "prec", "rec")

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/predicted_proba/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
pred.pr.rf <- prediction(test.rf$predicted.test, test.rf$death_1y)
aucpr.rf <- ROCR::performance(pred.pr.rf, "prec", "rec")

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/predicted_proba/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
pred.pr.lasso <- prediction(test.lasso$predicted.test, test.lasso$death_1y)
aucpr.lasso <- ROCR::performance(pred.pr.lasso, "prec", "rec")

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(aucpr.lasso,col=cols[1])
plot(aucpr.lgbm,col=cols[3],add=T)
plot(aucpr,add=T)
plot(aucpr.xgboost,col=cols[4],add=T)
plot(aucpr.rf,col=cols[2],add=T)
text(0.6,0.85,"Logistic (AUPRC 0.25, 95%CI 0.23-0.26)",xpd=TRUE,cex=0.9)
text(0.608,0.79,"XGBoost (AUPRC 0.25, 95%CI 0.24-0.27)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.614,0.73,"LightGBM (AUPRC 0.25, 95%CI 0.24-0.26)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.65,0.67,"Random Forest (AUPRC 0.23, 95%CI 0.22-0.24)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.591,0.61,"Lasso (AUPRC 0.24, 95%CI 0.23-0.25)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for all patients",xpd=TRUE,cex=1.3)

## age inpatient 18-44 #####
#logistic
train <- readRDS("train.inpatient_18-44.RDS")
test <- readRDS("test.inpatient_18-44.RDS")
load("inpatient.age18-44.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr, "prec", "rec")

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
pred.pr.xgboost <- prediction(test.xgboost$predicted.test, test.xgboost$death_1y)
aucpr.xgboost <- ROCR::performance(pred.pr.xgboost, "prec", "rec")

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
pred.pr.lgbm <- prediction(test.lgbm$predicted.test, test.lgbm$death_1y)
aucpr.lgbm <- ROCR::performance(pred.pr.lgbm, "prec", "rec")

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
pred.pr.rf <- prediction(test.rf$predicted.test, test.rf$death_1y)
aucpr.rf <- ROCR::performance(pred.pr.rf, "prec", "rec")

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
pred.pr.lasso <- prediction(test.lasso$predicted.test, test.lasso$death_1y)
aucpr.lasso <- ROCR::performance(pred.pr.lasso, "prec", "rec")

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(aucpr)
plot(aucpr.rf,col=cols[2],add=T)
plot(aucpr.lasso,col=cols[1],add=T)
plot(aucpr.lgbm,col=cols[3],add=T)
plot(aucpr.xgboost,col=cols[4],add=T)
text(0.65,0.25,"Logistic (AUPRC 0.09, 95%CI 0.05-0.24)",xpd=TRUE,cex=0.9)
text(0.658,0.23,"XGBoost (AUPRC 0.06, 95%CI 0.05-0.15)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.664,0.21,"LightGBM (AUPRC 0.06, 95%CI 0.05-0.16)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.7,0.19,"Random Forest (AUPRC 0.08, 95%CI 0.05-0.21)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.64,0.17,"Lasso (AUPRC 0.09, 95%CI 0.06-0.22)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,0.31,"Models for hospitalized patients aged 18-44",xpd=TRUE,cex=1.3)

## age inpatient 45-64 #####
#logistic
train <- readRDS("train.inpatient_45-64.RDS")
test <- readRDS("test.inpatient_45-64.RDS")
load("inpatient.age45-64.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr, "prec", "rec")

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
pred.pr.xgboost <- prediction(test.xgboost$predicted.test, test.xgboost$death_1y)
aucpr.xgboost <- ROCR::performance(pred.pr.xgboost, "prec", "rec")

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
pred.pr.lgbm <- prediction(test.lgbm$predicted.test, test.lgbm$death_1y)
aucpr.lgbm <- ROCR::performance(pred.pr.lgbm, "prec", "rec")

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
pred.pr.rf <- prediction(test.rf$predicted.test, test.rf$death_1y)
aucpr.rf <- ROCR::performance(pred.pr.rf, "prec", "rec")

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
pred.pr.lasso <- prediction(test.lasso$predicted.test, test.lasso$death_1y)
aucpr.lasso <- ROCR::performance(pred.pr.lasso, "prec", "rec")

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(aucpr.rf,col=cols[2])
plot(aucpr.lasso,col=cols[1],add=T)
plot(aucpr.lgbm,col=cols[3],add=T)
plot(aucpr,add=T)
plot(aucpr.xgboost,col=cols[4],add=T)
text(0.65,0.85,"Logistic (AUPRC 0.17, 95%CI 0.13-0.22)",xpd=TRUE,cex=0.9)
text(0.658,0.79,"XGBoost (AUPRC 0.19, 95%CI 0.15-0.25)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.664,0.73,"LightGBM (AUPRC 0, 95%CI 0.15-0.26)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.7,0.67,"Random Forest (AUPRC 0.19, 95%CI 0.14-0.24)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.64,0.61,"Lasso (AUPRC 0.18, 95%CI 0.14-0.24)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for hospitalized patients aged 45-64",xpd=TRUE,cex=1.3)

## age inpatient 65+ #####
#logistic
train <- readRDS("train.inpatient_65+.RDS")
test <- readRDS("test.inpatient_65+.RDS")
load("inpatient.age65.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr, "prec", "rec")

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
pred.pr.xgboost <- prediction(test.xgboost$predicted.test, test.xgboost$death_1y)
aucpr.xgboost <- ROCR::performance(pred.pr.xgboost, "prec", "rec")

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
pred.pr.lgbm <- prediction(test.lgbm$predicted.test, test.lgbm$death_1y)
aucpr.lgbm <- ROCR::performance(pred.pr.lgbm, "prec", "rec")

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
pred.pr.rf <- prediction(test.rf$predicted.test, test.rf$death_1y)
aucpr.rf <- ROCR::performance(pred.pr.rf, "prec", "rec")

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
pred.pr.lasso <- prediction(test.lasso$predicted.test, test.lasso$death_1y)
aucpr.lasso <- ROCR::performance(pred.pr.lasso, "prec", "rec")

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(aucpr.rf,col=cols[2])
plot(aucpr.lasso,col=cols[1],add=T)
plot(aucpr.lgbm,col=cols[3],add=T)
plot(aucpr,add=T)
plot(aucpr.xgboost,col=cols[4],add=T)
text(0.65,0.85,"Logistic (AUPRC 0.38, 95%CI 0.36-0.41)",xpd=TRUE,cex=0.9)
text(0.658,0.79,"XGBoost (AUPRC 0.38, 95%CI 0.35-0.40)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.664,0.73,"LightGBM (AUPRC 0.39, 95%CI 0.36-0.40)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.7,0.67,"Random Forest (AUPRC 0.38, 95%CI 0.35-0.40)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.64,0.61,"Lasso (AUPRC 0.39, 95%CI 0.36-0.40)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for hospitalized patients aged 65+",xpd=TRUE,cex=1.3)

## PCR age 18-44 #####
#logistic
train <- readRDS("train.pcr_18-44.RDS")
test <- readRDS("test.pcr_18-44.RDS")
load("pcr.age18-44.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr, "prec", "rec")

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
pred.pr.xgboost <- prediction(test.xgboost$predicted.test, test.xgboost$death_1y)
aucpr.xgboost <- ROCR::performance(pred.pr.xgboost, "prec", "rec")

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
pred.pr.lgbm <- prediction(test.lgbm$predicted.test, test.lgbm$death_1y)
aucpr.lgbm <- ROCR::performance(pred.pr.lgbm, "prec", "rec")

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
pred.pr.rf <- prediction(test.rf$predicted.test, test.rf$death_1y)
aucpr.rf <- ROCR::performance(pred.pr.rf, "prec", "rec")

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
pred.pr.lasso <- prediction(test.lasso$predicted.test, test.lasso$death_1y)
aucpr.lasso <- ROCR::performance(pred.pr.lasso, "prec", "rec")

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(aucpr.lgbm,col=cols[3])
plot(aucpr.lasso,col=cols[1],add=T)
plot(aucpr,add=T)
plot(aucpr.xgboost,col=cols[4],add=T)
plot(aucpr.rf,col=cols[2],add=T)
text(0.65,0.07,"Logistic (AUPRC 0.02, 95%CI 0.01-0.03)",xpd=TRUE,cex=0.9)
text(0.658,0.065,"XGBoost (AUPRC 0.02, 95%CI 0.01-0.03)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.6645,0.06,"LightGBM (AUPRC 0.02, 95%CI 0.01-0.04)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.7,0.055,"Random Forest (AUPRC 0.02, 95%CI 0.02-0.04)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.64,0.05,"Lasso (AUPRC 0.02, 95%CI 0.01-0.04)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,0.36,"Models for patients with PCR test aged 18-44",xpd=TRUE,cex=1.3)

## PCR age 45-64 #####
#logistic
train <- readRDS("train.pcr_45-64.RDS")
test <- readRDS("test.pcr_45-64.RDS")
load("pcr.age45-64.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr, "prec", "rec")

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
pred.pr.xgboost <- prediction(test.xgboost$predicted.test, test.xgboost$death_1y)
aucpr.xgboost <- ROCR::performance(pred.pr.xgboost, "prec", "rec")

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
pred.pr.lgbm <- prediction(test.lgbm$predicted.test, test.lgbm$death_1y)
aucpr.lgbm <- ROCR::performance(pred.pr.lgbm, "prec", "rec")

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
pred.pr.rf <- prediction(test.rf$predicted.test, test.rf$death_1y)
aucpr.rf <- ROCR::performance(pred.pr.rf, "prec", "rec")

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
pred.pr.lasso <- prediction(test.lasso$predicted.test, test.lasso$death_1y)
aucpr.lasso <- ROCR::performance(pred.pr.lasso, "prec", "rec")

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(aucpr.lasso,col=cols[1])
plot(aucpr.lgbm,col=cols[3],add=T)
plot(aucpr,add=T)
plot(aucpr.xgboost,col=cols[4],add=T)
plot(aucpr.rf,col=cols[2],add=T)
text(0.6,0.85,"Logistic (AUPRC 0.09, 95%CI 0.07-0.11)",xpd=TRUE,cex=0.9)
text(0.608,0.78,"XGBoost (AUPRC 0.10, 95%CI 0.07-0.13)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.616,0.71,"LightGBM (AUPRC 0.09, 95%CI 0.07-0.12)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.651,0.64,"Random Forest (AUPRC 0.10, 95%CI 0.08-0.13)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.591,0.57,"Lasso (AUPRC 0.09, 95%CI 0.07-0.12)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for patients with PCR test aged 45-64",xpd=TRUE,cex=1.3)

## PCR age 65+ #####
#logistic
train <- readRDS("train.pcr_65+.RDS")
test <- readRDS("test.pcr_65+.RDS")
load("pcr.age65.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
test$predicted.test <-  predict(model.OR,test, type="response")
pred.pr <- prediction(test$predicted.test, test$death_1y)
aucpr <- ROCR::performance(pred.pr, "prec", "rec")

#XGBoost
test.xgboost <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/XGBoost.csv"))
colnames(test.xgboost) <- c("death_1y","predicted.test")
pred.pr.xgboost <- prediction(test.xgboost$predicted.test, test.xgboost$death_1y)
aucpr.xgboost <- ROCR::performance(pred.pr.xgboost, "prec", "rec")

#LightGBM
test.lgbm <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/LightGBM.csv"))
colnames(test.lgbm) <- c("death_1y","predicted.test")
pred.pr.lgbm <- prediction(test.lgbm$predicted.test, test.lgbm$death_1y)
aucpr.lgbm <- ROCR::performance(pred.pr.lgbm, "prec", "rec")

#Random Forest
test.rf <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/Random Forest.csv"))
colnames(test.rf) <- c("death_1y","predicted.test")
pred.pr.rf <- prediction(test.rf$predicted.test, test.rf$death_1y)
aucpr.rf <- ROCR::performance(pred.pr.rf, "prec", "rec")

#LASSO
test.lasso <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/Lasso.csv"))
colnames(test.lasso) <- c("death_1y","predicted.test")
pred.pr.lasso <- prediction(test.lasso$predicted.test, test.lasso$death_1y)
aucpr.lasso <- ROCR::performance(pred.pr.lasso, "prec", "rec")

par(oma=c(0, 0, 0.8, 0))
cols <- hcl.colors(4, palette = "Dark2", alpha = 1)
# cols <- hcl.colors(4, palette = "viridis", alpha = 1)
plot(aucpr.lasso,col=cols[1])
plot(aucpr.lgbm,col=cols[3],add=T)
plot(aucpr,add=T)
plot(aucpr.xgboost,col=cols[4],add=T)
plot(aucpr.rf,col=cols[2],add=T)
text(0.6,0.85,"Logistic (AUPRC 0.30, 95%CI 0.28-0.31)",xpd=TRUE,cex=0.9)
text(0.608,0.79,"XGBoost (AUPRC 0.30, 95%CI 0.28-0.31)",xpd=TRUE,cex=0.9,col=cols[4])
text(0.614,0.73,"LightGBM (AUPRC 0.33, 95%CI 0.31-0.34)",xpd=TRUE,cex=0.9,col=cols[3])
text(0.65,0.67,"Random Forest (AUPRC 0.31, 95%CI 0.29-0.32)",xpd=TRUE,cex=0.9,col=cols[2])
text(0.591,0.61,"Lasso (AUPRC 0.31, 95%CI 0.29-0.33)",xpd=TRUE,cex=0.9,col=cols[1])
text(0.5,1.08,"Models for patients with PCR test aged 65+",xpd=TRUE,cex=1.3)

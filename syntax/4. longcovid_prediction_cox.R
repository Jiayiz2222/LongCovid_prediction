setwd("/mnt/A/long_covid_jiayi/jiayi/operation/longcovid_prediction")
library(data.table)
library(tableone)
require(caret)
library(StepReg)
library(pROC)
library(ROCR)
library(survival)
library(survey)

##age<45
train <- readRDS("train.18-44.RDS")
test <- readRDS("test.18-44.RDS")
summary(train)
train <- train[,-c("major","ards","ageg","pd","bp","anxiety","htn","cbc.wbc","cbc.neuphil","cbc.plt","inflam.crp","inflam.ldh")]
var <- names(train)
var <- var[!var %in% c("id","death_1y")]

cohort <- readRDS("final_cohort3.RDS")
train <- merge(train,cohort[,.(id,death_date_ymd,index.date)],all.x=T,by="id")
train$death_date_ymd <- as.Date(train$death_date_ymd)
train[!is.na(death_1y),date_death_1y := as.Date(death_date_ymd)]
OUTCOMES <- c("death_1y")
# Post-acute phase (within 30 days)
for(oc in OUTCOMES) {
  #cohort[, c(paste0("postacute.hx.",oc)) := as.numeric(get(paste0("dx.",oc)) )]
  train[, c(paste0("postacute.censor.date.", oc)) := pmin(get(paste0("date_",oc)), (index.date+365), as.Date("2023-08-31"), na.rm=T)]
  train[, c(paste0("postacute.time.to.censor.", oc)) := as.numeric((get(paste0("postacute.censor.date.", oc)) - index.date) + 1)/365]
  train[, c(paste0("postacute.outcome.", oc)) := as.numeric(!is.na(get(paste0("date_",oc))) & get(paste0("postacute.censor.date.", oc))==get(paste0("date_",oc)) & get(paste0("date_",oc))>=index.date)]
}

test <- merge(test,cohort[,.(id,death_date_ymd,index.date)],all.x=T,by="id")
test$death_date_ymd <- as.Date(test$death_date_ymd)
test[!is.na(death_1y),date_death_1y := as.Date(death_date_ymd)]
OUTCOMES <- c("death_1y")
# Post-acute phase (within 30 days)
for(oc in OUTCOMES) {
  #cohort[, c(paste0("postacute.hx.",oc)) := as.numeric(get(paste0("dx.",oc)) )]
  test[, c(paste0("postacute.censor.date.", oc)) := pmin(get(paste0("date_",oc)), (index.date+365), as.Date("2023-08-31"), na.rm=T)]
  test[, c(paste0("postacute.time.to.censor.", oc)) := as.numeric((get(paste0("postacute.censor.date.", oc)) - index.date) + 1)/365]
  test[, c(paste0("postacute.outcome.", oc)) := as.numeric(!is.na(get(paste0("date_",oc))) & get(paste0("postacute.censor.date.", oc))==get(paste0("date_",oc)) & get(paste0("date_",oc))>=index.date)]
}


train$death_1y <- as.numeric(train$death_1y)-1
model.train <- stepwiseCox(formula = as.formula(paste0("Surv(postacute.time.to.censor.death_1y,death_1y)~",paste(var,collapse = "+"))),
                           data = train,selection = "backward",select = "SBC")
save(model.train,file="age18-44.Cox.Rdata")
load("age18-44.Cox.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!="",]
var <- paste(v1, collapse = "+")
model.HR <- coxph(as.formula(paste0("Surv(postacute.time.to.censor.death_1y,death_1y)~",var)), data = train)
hr <- exp(cbind(coef(model.HR),confint.default(model.HR)))
summary(model.HR)
sink("age18-44.Cox.PH.assumption.txt")
cox.zph(model.HR)
sink()

##age 45-64
train <- readRDS("train.45-64.RDS")
test <- readRDS("test.45-64.RDS")
summary(train)
train <- train[,-c("major","ards","ageg","pd","bp","anxiety","htn","cbc.wbc","cbc.neuphil","cbc.plt","inflam.crp","inflam.ldh")]
var <- names(train)
var <- var[!var %in% c("id","death_1y")]

cohort <- readRDS("final_cohort3.RDS")
train <- merge(train,cohort[,.(id,death_date_ymd,index.date)],all.x=T,by="id")
train$death_date_ymd <- as.Date(train$death_date_ymd)
train[!is.na(death_1y),date_death_1y := as.Date(death_date_ymd)]
OUTCOMES <- c("death_1y")
# Post-acute phase (within 30 days)
for(oc in OUTCOMES) {
  #cohort[, c(paste0("postacute.hx.",oc)) := as.numeric(get(paste0("dx.",oc)) )]
  train[, c(paste0("postacute.censor.date.", oc)) := pmin(get(paste0("date_",oc)), (index.date+365), as.Date("2023-08-31"), na.rm=T)]
  train[, c(paste0("postacute.time.to.censor.", oc)) := as.numeric((get(paste0("postacute.censor.date.", oc)) - index.date) + 1)/365]
  train[, c(paste0("postacute.outcome.", oc)) := as.numeric(!is.na(get(paste0("date_",oc))) & get(paste0("postacute.censor.date.", oc))==get(paste0("date_",oc)) & get(paste0("date_",oc))>=index.date)]
}

test <- merge(test,cohort[,.(id,death_date_ymd,index.date)],all.x=T,by="id")
test$death_date_ymd <- as.Date(test$death_date_ymd)
test[!is.na(death_1y),date_death_1y := as.Date(death_date_ymd)]
OUTCOMES <- c("death_1y")
# Post-acute phase (within 30 days)
for(oc in OUTCOMES) {
  #cohort[, c(paste0("postacute.hx.",oc)) := as.numeric(get(paste0("dx.",oc)) )]
  test[, c(paste0("postacute.censor.date.", oc)) := pmin(get(paste0("date_",oc)), (index.date+365), as.Date("2023-08-31"), na.rm=T)]
  test[, c(paste0("postacute.time.to.censor.", oc)) := as.numeric((get(paste0("postacute.censor.date.", oc)) - index.date) + 1)/365]
  test[, c(paste0("postacute.outcome.", oc)) := as.numeric(!is.na(get(paste0("date_",oc))) & get(paste0("postacute.censor.date.", oc))==get(paste0("date_",oc)) & get(paste0("date_",oc))>=index.date)]
}

train$death_1y <- as.numeric(train$death_1y)
model.train <- stepwiseCox(formula = as.formula(paste0("Surv(postacute.time.to.censor.death_1y,death_1y)~",paste(var,collapse = "+"))),
                           data = train,selection = "backward",select = "SBC")
save(model.train,file="age45-64.Cox.Rdata")
load("age45-64.Cox.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.HR <- coxph(as.formula(paste0("Surv(postacute.time.to.censor.death_1y,death_1y)~",var)), data = train)
hr <- exp(cbind(coef(model.HR),confint.default(model.HR)))
summary(model.HR)
sink("age45-64.Cox.PH.assumption.txt")
cox.zph(model.HR)
sink()


##age 65+
train <- readRDS("train.65.RDS")
test <- readRDS("test.65.RDS")
summary(train)
train <- train[,-c("major","ards","ageg","pd","bp","anxiety","htn","cbc.wbc","cbc.neuphil","cbc.plt","inflam.crp","inflam.ldh")]
var <- names(train)
var <- var[!var %in% c("id","death_1y")]

cohort <- readRDS("final_cohort3.RDS")
train <- merge(train,cohort[,.(id,death_date_ymd,index.date)],all.x=T,by="id")
train$death_date_ymd <- as.Date(train$death_date_ymd)
train[!is.na(death_1y),date_death_1y := as.Date(death_date_ymd)]
OUTCOMES <- c("death_1y")
# Post-acute phase (within 30 days)
for(oc in OUTCOMES) {
  #cohort[, c(paste0("postacute.hx.",oc)) := as.numeric(get(paste0("dx.",oc)) )]
  train[, c(paste0("postacute.censor.date.", oc)) := pmin(get(paste0("date_",oc)), (index.date+365), as.Date("2023-08-31"), na.rm=T)]
  train[, c(paste0("postacute.time.to.censor.", oc)) := as.numeric((get(paste0("postacute.censor.date.", oc)) - index.date) + 1)/365]
  train[, c(paste0("postacute.outcome.", oc)) := as.numeric(!is.na(get(paste0("date_",oc))) & get(paste0("postacute.censor.date.", oc))==get(paste0("date_",oc)) & get(paste0("date_",oc))>=index.date)]
}

test <- merge(test,cohort[,.(id,death_date_ymd,index.date)],all.x=T,by="id")
test$death_date_ymd <- as.Date(test$death_date_ymd)
test[!is.na(death_1y),date_death_1y := as.Date(death_date_ymd)]
OUTCOMES <- c("death_1y")
# Post-acute phase (within 30 days)
for(oc in OUTCOMES) {
  #cohort[, c(paste0("postacute.hx.",oc)) := as.numeric(get(paste0("dx.",oc)) )]
  test[, c(paste0("postacute.censor.date.", oc)) := pmin(get(paste0("date_",oc)), (index.date+365), as.Date("2023-08-31"), na.rm=T)]
  test[, c(paste0("postacute.time.to.censor.", oc)) := as.numeric((get(paste0("postacute.censor.date.", oc)) - index.date) + 1)/365]
  test[, c(paste0("postacute.outcome.", oc)) := as.numeric(!is.na(get(paste0("date_",oc))) & get(paste0("postacute.censor.date.", oc))==get(paste0("date_",oc)) & get(paste0("date_",oc))>=index.date)]
}

train$death_1y <- as.numeric(train$death_1y)
model.train <- stepwiseCox(formula = as.formula(paste0("Surv(postacute.time.to.censor.death_1y,death_1y)~",paste(var,collapse = "+"))),
                           data = train,selection = "backward",select = "SBC")
save(model.train,file="age65.Cox.Rdata")
load("age65.Cox.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.HR <- coxph(as.formula(paste0("Surv(postacute.time.to.censor.death_1y,death_1y)~",var)), data = train)
hr <- exp(cbind(coef(model.HR),confint.default(model.HR)))
summary(model.HR)
sink("age65.Cox.PH.assumption.txt")
cox.zph(model.HR)
sink()
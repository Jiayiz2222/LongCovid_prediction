setwd("/mnt/A/long_covid_jiayi/jiayi/operation/longcovid_prediction")
library(data.table)
require(ggplot2)
library(rms)
library(pROC)
library(ResourceSelection)
library(boot)

####  calibration plot  ######
## logistic #####
## age 18-44 ####
train <- readRDS("train.18-44.RDS")
test <- readRDS("test.18-44.RDS")
load("age18-44.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
summary(predict(model.OR, newdata=test,type="response"))

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ predict(model.OR, newdata=new.data,type="link") ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

pred.ext <- predict(model.OR, newdata=test,type="response")
#predicted probabilities
group.cut <- quantile(pred.ext, c(seq(0, 1, 0.1)))
group <- cut(pred.ext, group.cut) 
pred.prob <- tapply(pred.ext, group, mean)
# Observed probabilities
test$death_1y <- as.numeric(test$death_1y)-1
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for logistic model in patients aged 18-44") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11.7))+
  annotate("text", x=0.00074, y=0.0031, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.000668, y=0.00285, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",round(df$value_lower[2],3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.00102, y=0.0026, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value=",round(hoslem$p.value,3)))

## age 45-64 ####
train <- readRDS("train.45-64.RDS")
test <- readRDS("test.45-64.RDS")
load("age45-64.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ predict(model.OR, newdata=new.data,type="link") ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

pred.ext <- predict(model.OR, newdata=test,type="response")
#predicted probabilities
group.cut <- quantile(pred.ext, c(seq(0, 1, 0.1)))
group <- cut(pred.ext, group.cut) 
pred.prob <- tapply(pred.ext, group, mean)
# Observed probabilities
test$death_1y <- as.numeric(test$death_1y)-1
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for logistic model in patients aged 45-64") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11.7))+
  annotate("text", x=0.0083, y=0.0275, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.007, y=0.0255, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",round(df$value_lower[2],3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.011, y=0.0235, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value=",round(hoslem$p.value,3)))

## age 65+ ####
train <- readRDS("train.65.RDS")
test <- readRDS("test.65.RDS")
load("age65.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ predict(model.OR, newdata=new.data,type="link") ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

pred.ext <- predict(model.OR, newdata=test,type="response")
#predicted probabilities
group.cut <- quantile(pred.ext, c(seq(0, 1, 0.1)))
group <- cut(pred.ext, group.cut) 
pred.prob <- tapply(pred.ext, group, mean)
# Observed probabilities
test$death_1y <- as.numeric(test$death_1y)-1
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for logistic model in patients aged 65+") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=12))+
  annotate("text", x=0.075, y=0.255, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.0654, y=0.235, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",round(df$value_lower[2],3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.103, y=0.215, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value<0.001"))

## all age #####
train <- readRDS("train.RDS")
test <- readRDS("test.RDS")
load("allage.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ predict(model.OR, newdata=new.data,type="link") ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

pred.ext <- predict(model.OR, newdata=test,type="response")
hoslem <- hoslem.test(as.numeric(test$death_1y)-1, pred.ext)

pred.ext <- predict(model.OR, newdata=test,type="response")
#predicted probabilities
group.cut <- quantile(pred.ext, c(seq(0, 1, 0.1)))
group <- cut(pred.ext, group.cut) 
pred.prob <- tapply(pred.ext, group, mean)
# Observed probabilities
test$death_1y <- as.numeric(test$death_1y)-1
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for logistic model in all patients") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=12))+
  annotate("text", x=0.032, y=0.12, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.028, y=0.11, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",round(df$value_lower[2],3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.045, y=0.1, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value<0.001"))


## inpatient age 18-44 ####
train <- readRDS("inpatient/train.inpatient_18-44.RDS")
test <- readRDS("inpatient/test.inpatient_18-44.RDS")
load("inpatient.age18-44.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
summary(predict(model.OR, newdata=test,type="response"))

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ predict(model.OR, newdata=new.data,type="link") ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

pred.ext <- predict(model.OR, newdata=test,type="response")
hoslem <- hoslem.test(as.numeric(test$death_1y)-1, pred.ext)

## inpatient age 45-64 ####
train <- readRDS("inpatient/train.inpatient_45-64.RDS")
test <- readRDS("inpatient/test.inpatient_45-64.RDS")
load("inpatient.age45-64.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ predict(model.OR, newdata=new.data,type="link") ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

pred.ext <- predict(model.OR, newdata=test,type="response")
hoslem <- hoslem.test(as.numeric(test$death_1y)-1, pred.ext)

## inpatient age 65+ ####
train <- readRDS("inpatient/train.inpatient_65+.RDS")
test <- readRDS("inpatient/test.inpatient_65+.RDS")
load("inpatient.age65.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ predict(model.OR, newdata=new.data,type="link") ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

pred.ext <- predict(model.OR, newdata=test,type="response")
hoslem <- hoslem.test(as.numeric(test$death_1y)-1, pred.ext)


## pcr age 18-44 ####
train <- readRDS("pcr/train.pcr_18-44.RDS")
test <- readRDS("pcr/test.pcr_18-44.RDS")
load("pcr.age18-44.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
summary(predict(model.OR, newdata=test,type="response"))

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ predict(model.OR, newdata=new.data,type="link") ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

pred.ext <- predict(model.OR, newdata=test,type="response")
hoslem <- hoslem.test(as.numeric(test$death_1y)-1, pred.ext)

## pcr age 45-64 ####
train <- readRDS("pcr/train.pcr_45-64.RDS")
test <- readRDS("pcr/test.pcr_45-64.RDS")
load("pcr.age45-64.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ predict(model.OR, newdata=new.data,type="link") ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

pred.ext <- predict(model.OR, newdata=test,type="response")
hoslem <- hoslem.test(as.numeric(test$death_1y)-1, pred.ext)

## pcr age 65+ ####
train <- readRDS("pcr/train.pcr_65+.RDS")
test <- readRDS("pcr/test.pcr_65+.RDS")
load("pcr.age65.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
model.OR <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ predict(model.OR, newdata=new.data,type="link") ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

pred.ext <- predict(model.OR, newdata=test,type="response")
hoslem <- hoslem.test(as.numeric(test$death_1y)-1, pred.ext)


## XGBoost #####
## age 18-44 ####
test <- setDT(read.csv("predicted_proba_18-44/XGBoost.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.1)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for XGBoost model in patients aged 18-44") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(title = element_text(size=12),
        axis.text.x=element_text(size=11),axis.title.x=element_text(size=13),
        axis.text.y=element_text(size=11),axis.title.y=element_text(size=13))+
  annotate("text", x=0.00089, y=0.0031, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.000815, y=0.0029, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",round(df$value_lower[2],3),",",format(round(df$value_upper[2],3),nsmall=3),")"))+
  annotate("text", x=0.001175, y=0.0027, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value ",round(hoslem$p.value,3)))

## age 45-64 ####
test <- setDT(read.csv("predicted_proba_45-64/XGBoost.csv"))
colnames(test) <- c("death_1y","pred.ext")
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.1)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for XGBoost model in patients aged 45-64") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11))+
  annotate("text", x=0.0077, y=0.027, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",format(round(df$value_upper[1],3),nsmall = 3),")"))+
  annotate("text", x=0.00677, y=0.025, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",round(df$value_lower[2],3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.0106, y=0.023, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value ",round(hoslem$p.value,3)))

## age 65+ ####
test <- setDT(read.csv("predicted_proba_65/XGBoost.csv"))
colnames(test) <- c("death_1y","pred.ext")
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.1)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for XGBoost model in patients aged 65+") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11))+
  annotate("text", x=0.071, y=0.24, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.0595, y=0.22, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",format(round(df$value_lower[2],3),nsmall=3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.096, y=0.2, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value ",round(hoslem$p.value,3)))

## all age ####
test <- setDT(read.csv("predicted_proba/XGBoost.csv"))
colnames(test) <- c("death_1y","pred.ext")
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.1)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for XGBoost model in all patients") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11))+
  annotate("text", x=0.0345, y=0.12, label=paste0("Intercept ",format(round(df$value[1],3),nsmall=3)," (95%CI ",round(df$value_lower[1],3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.031, y=0.11, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",format(round(df$value_lower[2],3),nsmall=3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.0485, y=0.1, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value<0.001"))

## inpatient age 18-44 ####
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/XGBoost.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## inpatient age 45-64 ####
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/XGBoost.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## inpatient age 65+ ####
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/XGBoost.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## pcr age 18-44 ####
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/XGBoost.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## pcr age 45-64 ####
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/XGBoost.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## pcr age 65+ ####
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/XGBoost.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## LightGBM #####
## age 18-44 #####
test <- setDT(read.csv("predicted_proba_18-44/LightGBM.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the LightGBM (with objective: binary) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.1)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for LightGBM model in patients aged 18-44") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11))+
  annotate("text", x=0.000935, y=0.0033, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.00083, y=0.0031, label=paste0("Slope ",format(round(df$value[2],3),nsmall=3)," (95%CI ",round(df$value_lower[2],3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.00122, y=0.0029, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value ",round(hoslem$p.value,3)))

## age 45-64 ####
test <- setDT(read.csv("predicted_proba_45-64/LightGBM.csv"))
colnames(test) <- c("death_1y","pred.ext")
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.1)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for LightGBM model in patients aged 45-64") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11))+
  annotate("text", x=0.0083, y=0.027, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",format(round(df$value_upper[1],3),nsmall = 3),")"))+
  annotate("text", x=0.00729, y=0.025, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",round(df$value_lower[2],3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.0111, y=0.023, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value ",round(hoslem$p.value,3)))

## age 65+ ####
test <- setDT(read.csv("predicted_proba_65/LightGBM.csv"))
colnames(test) <- c("death_1y","pred.ext")
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.1)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for LightGBM model in patients aged 65+") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11))+
  annotate("text", x=0.07, y=0.24, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.061, y=0.22, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",format(round(df$value_lower[2],3),nsmall=3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.097, y=0.2, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value ",format(round(hoslem$p.value,3),nsmall=3)))

## all age ####
test <- setDT(read.csv("predicted_proba/LightGBM.csv"))
colnames(test) <- c("death_1y","pred.ext")
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.1)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for LightGBM model in all patients") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11))+
  annotate("text", x=0.0323, y=0.12, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.028, y=0.11, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",format(round(df$value_lower[2],3),nsmall=3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.0468, y=0.1, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value<0.001"))

## inpatient age 18-44 ####
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/LightGBM.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## inpatient age 45-64 ####
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/LightGBM.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## inpatient age 65+ ####
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/LightGBM.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## pcr age 18-44 ####
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/LightGBM.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## pcr age 45-64 ####
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/LightGBM.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## pcr age 65+ ####
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/LightGBM.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## Random Forest #####
## age 18-44 #####
test <- setDT(read.csv("predicted_proba_18-44/Random Forest.csv"))
colnames(test) <- c("death_1y","pred.ext")
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.1)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for Random Forest in patients aged 18-44") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11))+
  annotate("text", x=0.00079, y=0.0032, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.000746, y=0.003, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",round(df$value_lower[2],3),",",format(round(df$value_upper[2],3),nsmall=3),")"))+
  annotate("text", x=0.001015, y=0.0028, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value ",round(hoslem$p.value,3)))

## age 45-64 ####
test <- setDT(read.csv("predicted_proba_45-64/Random Forest.csv"))
colnames(test) <- c("death_1y","pred.ext")
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.1)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for Random Forest in patients aged 45-64") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11))+
  annotate("text", x=0.0075, y=0.027, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",format(round(df$value_upper[1],3),nsmall = 3),")"))+
  annotate("text", x=0.00705, y=0.025, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",round(df$value_lower[2],3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.0102, y=0.023, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value<0.001"))

## age 65+ ####
test <- setDT(read.csv("predicted_proba_65/Random Forest.csv"))
colnames(test) <- c("death_1y","pred.ext")
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.15)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for Random Forest in patients aged 65+") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11))+
  annotate("text", x=0.0315, y=0.085, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.03, y=0.08, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",format(round(df$value_lower[2],3),nsmall=3),",",format(round(df$value_upper[2],3),nsmall=3),")"))+
  annotate("text", x=0.04, y=0.075, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value<0.001"))

## all age ####
test <- setDT(read.csv("predicted_proba/Random Forest.csv"))
colnames(test) <- c("death_1y","pred.ext")
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.15)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for Random Forest in all patients") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11))+
  annotate("text", x=0.00655, y=0.015, label=paste0("Intercept ",format(round(df$value[1],3),nsmall=3)," (95%CI ",format(round(df$value_lower[1],3),nsmall=3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.00628, y=0.014, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",format(round(df$value_lower[2],3),nsmall=3),",",format(round(df$value_upper[2],3),nsmall=3),")"))+
  annotate("text", x=0.008, y=0.013, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value<0.001"))

## inpatient age 18-44 ####
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/Random Forest.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## inpatient age 45-64 ####
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/Random Forest.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## inpatient age 65+ ####
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/Random Forest.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## pcr age 18-44 ####
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/Random Forest.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## pcr age 45-64 ####
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/Random Forest.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## pcr age 65+ ####
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/Random Forest.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)


## LASSO #####
## age 18-44 #####
test <- setDT(read.csv("predicted_proba_18-44/Lasso.csv"))
colnames(test) <- c("death_1y","pred.ext")
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 
summary(test$X0.00048391117375042763)
#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.1)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for LASSO model in patients aged 18-44") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11))+
  annotate("text", x=0.00091, y=0.0032, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.00081, y=0.003, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",round(df$value_lower[2],3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.00118, y=0.0028, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value ",round(hoslem$p.value,3)))

## age 45-64 ####
test <- setDT(read.csv("predicted_proba_45-64/Lasso.csv"))
colnames(test) <- c("death_1y","pred.ext")
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.1)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for LASSO model in patients aged 45-64") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11))+
  annotate("text", x=0.0085, y=0.0265, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",format(round(df$value_upper[1],3),nsmall = 3),")"))+
  annotate("text", x=0.0073, y=0.025, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",round(df$value_lower[2],3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.011, y=0.0235, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value ",round(hoslem$p.value,3)))

## age 65+ ####
test <- setDT(read.csv("predicted_proba_65/Lasso.csv"))
colnames(test) <- c("death_1y","pred.ext")
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.1)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for LASSO model in patients aged 65+") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11))+
  annotate("text", x=0.07, y=0.245, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.061, y=0.23, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",format(round(df$value_lower[2],3),nsmall=3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.0972, y=0.215, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value<0.001"))

## all age ####
test <- setDT(read.csv("predicted_proba/Lasso.csv"))
colnames(test) <- c("death_1y","pred.ext")
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

#predicted probabilities
group.cut <- quantile(test$pred.ext, c(seq(0, 1, 0.1)))
group <- cut(test$pred.ext, group.cut) 
pred.prob <- tapply(test$pred.ext, group, mean)
# Observed probabilities
obs.prob <- tapply(test$death_1y, group, mean)
cal.m.ext <- data.frame(cbind(pred.prob, obs.prob))

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

plot(ggplot(cal.m.ext,aes(x=pred.prob,y=obs.prob)) + 
       scale_x_continuous(name = "Predicted Probabilities") +
       scale_y_continuous(name = "Observed Probabilities") + 
       geom_smooth(method="lm", formula = y~x, se = FALSE) + 
       geom_point(size=2, shape = 19) + 
       geom_abline(intercept = 0, slope = 1, linetype=2) + 
       theme_light() + 
       ggtitle("Calibration Curve for LASSO model in all patients") )+ theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=11))+
  annotate("text", x=0.031, y=0.12, label=paste0("Intercept ",round(df$value[1],3)," (95%CI ",round(df$value_lower[1],3),",",round(df$value_upper[1],3),")"))+
  annotate("text", x=0.028, y=0.11, label=paste0("Slope ",round(df$value[2],3)," (95%CI ",format(round(df$value_lower[2],3),nsmall=3),",",round(df$value_upper[2],3),")"))+
  annotate("text", x=0.045, y=0.1, label=paste0("Hosmer and Lemeshow Goodness of fit: p-value<0.001"))

## inpatient age 18-44 ####
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_18-44/Lasso.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## inpatient age 45-64 ####
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_45-64/Lasso.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## inpatient age 65+ ####
test <- setDT(read.csv("predicted_proba_test/inpatient/predicted_proba_65/Lasso.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## pcr age 18-44 ####
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_18-44/Lasso.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## pcr age 45-64 ####
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_45-64/Lasso.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

## pcr age 65+ ####
test <- setDT(read.csv("predicted_proba_test/pcr/predicted_proba_65/Lasso.csv"))
colnames(test) <- c("death_1y","pred.ext")
#the XGBoost (with objective: binary:logistic) used sigmoid function to output predicted probability
test[,lp.ext:=log(pred.ext/(1 - pred.ext))] 

#intercept & slope (95% CI by bootstrapping)
cali.f <- function(data, indices){
  new.data <- data[indices,]
  fit.ext.lp <- lrm(new.data$death_1y ~ new.data$lp.ext ,x = T, y = T)
  return(c(fit.ext.lp$coefficients))
}
set.seed(888)
boot.perf <- boot(data=test, statistic=cali.f, R = 1000)

df <- data.frame(variable  = c("Intercept","Slope"),
                 value        = boot.perf$t0,
                 value_lower  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,4],boot.ci(boot.perf, type="perc",index=2)$percent[,4]),
                 value_upper  = c(boot.ci(boot.perf, type="perc",index=1)$percent[,5],boot.ci(boot.perf, type="perc",index=2)$percent[,5]),
                 row.names = NULL)

hoslem <- hoslem.test(test$death_1y, test$pred.ext)

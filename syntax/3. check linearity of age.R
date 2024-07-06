##### check linearity
setwd("/mnt/A/long_covid_jiayi/jiayi/operation/longcovid_prediction")
library(data.table)
library(rms)
library(ggplot2)
library(Hmisc)
library(car)
library(tidyverse)


## age<45 ####
train <- readRDS("train.18-44.RDS")
test <- readRDS("test.18-44.RDS")

load("age18-44.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))

v1 <- v[v$xModel!=1&v$xModel!="Age",]
var <- paste(v1, collapse = "+")
train <- as.data.frame(train)
train[v1] <- lapply(train[v1],as.numeric)
test <- boxTidwell(formula = as.numeric(death_1y)-1 ~ Age, 
           other.x = as.formula(paste0("~", var)), 
           data = train)
#0.08181

v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
lreg <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
logit.use <- log(lreg$fitted.values/(1-lreg$fitted.values))
# make a small data frame with the logit variable and the yearsSmoke predictor
linearity.data <- data.frame(logit.use, age = lreg$model$Age)
# create a plot with linear and actual relationships shown
linearPlot <- linearity.data %>%
  ggplot(aes(x = age, y = logit.use))+
  geom_point(aes(size = "Observation"), color = "gray80", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) + 
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) + 
  theme_minimal(base_size = 10, base_family = "serif") +
  labs(x = "Age", y = "Log-odds of predicted probability") +
  ggtitle("linearity assumption for logistic model in patients aged 18-44")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name="Type of fit line", values=c("gray50", "black")) +
  scale_size_manual(values = 1, name = "")+ 
  annotate("text", x=28, y=-1.5, label=paste0("The Box-Tidwell test: p-value=",round(test$result[3],3)))
linearPlot

### 45-64 ####
train <- readRDS("train.45-64.RDS")
test <- readRDS("test.45-64.RDS")

load("age45-64.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1&v$xModel!="Age",]
var <- paste(v1, collapse = "+")
test <- boxTidwell(formula = as.numeric(death_1y)-1 ~ Age, 
           other.x = as.formula(paste0("~", var)), 
           data = train)
#0.053

v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
lreg <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
logit.use <- log(lreg$fitted.values/(1-lreg$fitted.values))
# make a small data frame with the logit variable and the yearsSmoke predictor
linearity.data <- data.frame(logit.use, age = lreg$model$Age)
# create a plot with linear and actual relationships shown
linearPlot <- linearity.data %>%
  ggplot(aes(x = age, y = logit.use))+
  geom_point(aes(size = "Observation"), color = "gray80", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) + 
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) + 
  theme_minimal(base_size = 10, base_family = "serif") +
  labs(x = "Age", y = "Log-odds of predicted probability") +
  ggtitle("linearity assumption for logistic model in patients aged 45-64")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name="Type of fit line", values=c("gray50", "black")) +
  scale_size_manual(values = 1, name = "")+ 
  annotate("text", x=52, y=1, label=paste0("The Box-Tidwell test: p-value=",round(test$result[3],3)))
linearPlot

## 65+ ####
train <- readRDS("train.65.RDS")
test <- readRDS("test.65.RDS")

load("age65.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1&v$xModel!="Age",]
var <- paste(v1, collapse = "+")
train$age2 <- train$Age^2
test <- boxTidwell(formula = as.numeric(death_1y)-1 ~ Age, 
           other.x = as.formula(paste0("~", var)), 
           data = train)
#<0.001 

v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
lreg <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
logit.use <- log(lreg$fitted.values/(1-lreg$fitted.values))
# make a small data frame with the logit variable and the yearsSmoke predictor
linearity.data <- data.frame(logit.use, age = lreg$model$Age)
# create a plot with linear and actual relationships shown
linearPlot <- linearity.data %>%
  ggplot(aes(x = age, y = logit.use))+
  geom_point(aes(size = "Observation"), color = "gray80", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) + 
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) + 
  theme_minimal(base_size = 10, base_family = "serif") +
  labs(x = "Age", y = "Log-odds of predicted probability") +
  ggtitle("linearity assumption for logistic model in patients aged 65+")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name="Type of fit line", values=c("gray50", "black")) +
  scale_size_manual(values = 1, name = "")+ 
  annotate("text", x=82, y=2.5, label=paste0("The Box-Tidwell test: p-value<0.001"))
linearPlot


## all ####
train <- readRDS("train.RDS")
test <- readRDS("test.RDS")
load("allage.BW.Rdata")
v <- as.data.frame(t(model.train$`Selected Varaibles`))
v1 <- v[v$xModel!=1&v$xModel!="Age",]
var <- paste(v1, collapse = "+")
test <- boxTidwell(formula = as.numeric(death_1y)-1 ~ Age, 
           other.x = as.formula(paste0("~", var)), 
           data = train)
#<0.001 

v1 <- v[v$xModel!=1,]
var <- paste(v1, collapse = "+")
lreg <- glm(as.formula(paste0("death_1y~", var)),family = binomial,data=train)
logit.use <- log(lreg$fitted.values/(1-lreg$fitted.values))
# make a small data frame with the logit variable and the yearsSmoke predictor
linearity.data <- data.frame(logit.use, age = lreg$model$Age)
# create a plot with linear and actual relationships shown
linearPlot <- linearity.data %>%
  ggplot(aes(x = age, y = logit.use))+
  geom_point(aes(size = "Observation"), color = "gray80", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) + 
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) + 
  theme_minimal(base_size = 10, base_family = "serif") +
  labs(x = "Age", y = "Log-odds of predicted probability") +
  ggtitle("linearity assumption for logistic model in all patients")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name="Type of fit line", values=c("gray50", "black")) +
  scale_size_manual(values = 1, name = "")+ 
  annotate("text", x=50, y=2.5, label=paste0("The Box-Tidwell test: p-value<0.001"))
linearPlot


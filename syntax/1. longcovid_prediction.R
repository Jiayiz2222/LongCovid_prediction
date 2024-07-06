setwd("/mnt/A/long_covid_jiayi/jiayi/operation/longcovid_prediction")
library(data.table)
require(caret)
library(StepReg)
library(pROC)

# 1. split data ------
cohort <- readRDS("final_cohort3.RDS")
cohort <- cohort[,.(id,inpatient,COVID.test,sex,Age,fully_vacciated,major,mi,hf,stroke,af,dvt,pd,ee,bp,ild,cpd,dx.hx.ards,dx.bw.ards,ards,pancreatitis,li,esr,
                    akd,t1dm,t2dm,htn,anxiety,ptsd,seizure,covid.ct0.low20, inflam.crp, cbc.wbc, cbc.neuphil, cbc.plt, 
                    inflam.esr, inflam.procalcitonin, 
                    inflam.ferritin.mol, inflam.ldh, cbc.lympho, cbc.neuphil.pct,death_1y)]
var <- names(cohort)
var.factor <- var[!var %in% c("Age")]
cohort <- as.data.frame(cohort)
cohort[var.factor] <- lapply(cohort[var.factor],factor)
cohort <- setDT(cohort)

cohort$covid.ct0.low20 <- relevel(cohort$covid.ct0.low20,ref="2")
cohort$inflam.crp <- relevel(cohort$inflam.crp,ref = "1")
cohort$cbc.wbc <- relevel(cohort$cbc.wbc,ref = "1")
cohort$cbc.neuphil <- relevel(cohort$cbc.neuphil,ref = "1")
cohort$cbc.plt <- relevel(cohort$cbc.plt,ref = "2")
cohort$inflam.esr <- relevel(cohort$inflam.esr,ref = "1")
cohort$inflam.procalcitonin <- relevel(cohort$inflam.procalcitonin,ref = "1")
cohort$inflam.ferritin.mol <- relevel(cohort$inflam.ferritin.mol,ref = "1")
cohort$inflam.ldh <- relevel(cohort$inflam.ldh,ref = "1")
cohort$cbc.lympho <- relevel(cohort$cbc.lympho,ref = "2")
cohort$cbc.neuphil.pct <- relevel(cohort$cbc.neuphil.pct,ref = "1")

cohort[Age<45,ageg:=1]
cohort[Age>=45 & Age <=64,ageg:=2]
cohort[Age>=65,ageg:=3]

## split data
set.seed(123)
sample_size = floor(0.7*nrow(cohort))
picked = sample(seq_len(nrow(cohort)),size = sample_size)
train =cohort[picked,]
test =cohort[-picked,]
saveRDS(train,"train.RDS")
saveRDS(test,"test.RDS")

cohort1 <- cohort[ageg==1]
set.seed(123)
sample_size = floor(0.7*nrow(cohort1))
picked = sample(seq_len(nrow(cohort1)),size = sample_size)
train =cohort1[picked,]
test =cohort1[-picked,]
saveRDS(train,"train.18-44.RDS")
saveRDS(test,"test.18-44.RDS")

cohort2 <- cohort[ageg==2]
set.seed(123)
sample_size = floor(0.7*nrow(cohort2))
picked = sample(seq_len(nrow(cohort2)),size = sample_size)
train =cohort2[picked,]
test =cohort2[-picked,]
saveRDS(train,"train.45-64.RDS")
saveRDS(test,"test.45-64.RDS")

cohort3 <- cohort[ageg==3]
set.seed(123)
sample_size = floor(0.7*nrow(cohort3))
picked = sample(seq_len(nrow(cohort3)),size = sample_size)
train =cohort3[picked,]
test =cohort3[-picked,]
saveRDS(train,"train.65.RDS")
saveRDS(test,"test.65.RDS")

## hospital
cohort.in <- cohort[inpatient==1]

cohort.in1 <- cohort.in[ageg==1]
set.seed(123)
sample_size = floor(0.7*nrow(cohort.in1))
picked = sample(seq_len(nrow(cohort.in1)),size = sample_size)
train =cohort.in1[picked,]
test =cohort.in1[-picked,]
saveRDS(train,"train.inpatient_18-44.RDS")
saveRDS(test,"test.inpatient_18-44.RDS")

cohort.in2 <- cohort.in[ageg==2]
set.seed(123)
sample_size = floor(0.7*nrow(cohort.in2))
picked = sample(seq_len(nrow(cohort.in2)),size = sample_size)
train =cohort.in2[picked,]
test =cohort.in2[-picked,]
saveRDS(train,"train.inpatient_45-64.RDS")
saveRDS(test,"test.inpatient_45-64.RDS")

cohort.in3 <- cohort.in[ageg==3]
set.seed(123)
sample_size = floor(0.7*nrow(cohort.in3))
picked = sample(seq_len(nrow(cohort.in3)),size = sample_size)
train =cohort.in3[picked,]
test =cohort.in3[-picked,]
saveRDS(train,"train.inpatient_65+.RDS")
saveRDS(test,"test.inpatient_65+.RDS")

#PCR
cohort.pcr <- cohort[COVID.test!="RAT"]

cohort.pcr1 <- cohort.pcr[ageg==1]
set.seed(123)
sample_size = floor(0.7*nrow(cohort.pcr1))
picked = sample(seq_len(nrow(cohort.pcr1)),size = sample_size)
train =cohort.pcr1[picked,]
test =cohort.pcr1[-picked,]
saveRDS(train,"train.pcr_18-44.RDS")
saveRDS(test,"test.pcr_18-44.RDS")

cohort.pcr2 <- cohort.pcr[ageg==2]
set.seed(123)
sample_size = floor(0.7*nrow(cohort.pcr2))
picked = sample(seq_len(nrow(cohort.pcr2)),size = sample_size)
train =cohort.pcr2[picked,]
test =cohort.pcr2[-picked,]
saveRDS(train,"train.pcr_45-64.RDS")
saveRDS(test,"test.pcr_45-64.RDS")

cohort.pcr3 <- cohort.pcr[ageg==3]
set.seed(123)
sample_size = floor(0.7*nrow(cohort.pcr3))
picked = sample(seq_len(nrow(cohort.pcr3)),size = sample_size)
train =cohort.pcr3[picked,]
test =cohort.pcr3[-picked,]
saveRDS(train,"train.pcr_65+.RDS")
saveRDS(test,"test.pcr_65+.RDS")

#### table 1 ########
cohort <- readRDS("final_cohort3.RDS")
cohort <- cohort[,.(id,sex,Age,fully_vacciated,major,mi,hf,stroke,af,dvt,pd,ee,bp,ild,cpd,dx.hx.ards,dx.bw.ards,ards,pancreatitis,li,esr,
                    akd,t1dm,t2dm,htn,anxiety,ptsd,seizure,covid.ct0.low20, inflam.crp, cbc.wbc, cbc.neuphil, cbc.plt, 
                    inflam.esr, inflam.procalcitonin, 
                    inflam.ferritin.mol, inflam.ldh, cbc.lympho, cbc.neuphil.pct,death_1y,inpatient,COVID.test)]
var <- names(cohort)
var.factor <- var[!var %in% c("Age")]
cohort <- as.data.frame(cohort)
cohort[var.factor] <- lapply(cohort[var.factor],factor)
cohort <- setDT(cohort)
library(tableone)
var <- names(cohort) 
var <- var[!var %in% c("id","death_1y")]

table <- CreateTableOne(vars = var,data = setDT(cohort),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.csv")

table <- CreateTableOne(vars = var,strata = c("death_1y"),data = setDT(cohort),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.death_1y.csv")

cohort[Age<45,ageg:=1]
cohort[Age>=45 & Age <=64,ageg:=2]
cohort[Age>=65,ageg:=3]

cohort1 <- cohort[ageg==1]
table <- CreateTableOne(vars = var,data = setDT(cohort1),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.age18-44.csv")
table <- CreateTableOne(vars = var,strata = c("death_1y"),data = setDT(cohort1),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.death_1y.age18-44.csv")

cohort2 <- cohort[ageg==2]
table <- CreateTableOne(vars = var,data = setDT(cohort2),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.age45-64.csv")
table <- CreateTableOne(vars = var,strata = c("death_1y"),data = setDT(cohort2),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.death_1y.age45-64.csv")

cohort3 <- cohort[ageg==3]
table <- CreateTableOne(vars = var,data = setDT(cohort3),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.age65.csv")
table <- CreateTableOne(vars = var,strata = c("death_1y"),data = setDT(cohort3),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.death_1y.age65.csv")

# inpatient
cohort.in <- cohort[inpatient==1]
table <- CreateTableOne(vars = var,data = setDT(cohort.in),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.inpatient.csv")
table <- CreateTableOne(vars = var,strata = c("death_1y"),data = setDT(cohort.in),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.death_1y.inpatient.csv")

cohort1 <- cohort.in[ageg==1]
table <- CreateTableOne(vars = var,data = setDT(cohort1),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.inpatient.age18-44.csv")
table <- CreateTableOne(vars = var,strata = c("death_1y"),data = setDT(cohort1),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.inpatient.death_1y.age18-44.csv")

cohort2 <- cohort.in[ageg==2]
table <- CreateTableOne(vars = var,data = setDT(cohort2),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.inpatient.age45-64.csv")
table <- CreateTableOne(vars = var,strata = c("death_1y"),data = setDT(cohort2),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.inpatient.death_1y.age45-64.csv")

cohort3 <- cohort.in[ageg==3]
table <- CreateTableOne(vars = var,data = setDT(cohort3),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.inpatient.age65.csv")
table <- CreateTableOne(vars = var,strata = c("death_1y"),data = setDT(cohort3),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.inpatient.death_1y.age65.csv")

#### check patients with PCR 
table(cohort$COVID.test)
cohort.pcr <- cohort[COVID.test!="RAT"]
table <- CreateTableOne(vars = var,data = setDT(cohort.pcr),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.pcr.csv")
table <- CreateTableOne(vars = var,strata = c("death_1y"),data = setDT(cohort.pcr),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.death_1y.pcr.csv")

cohort1 <- cohort.pcr[ageg==1]
table <- CreateTableOne(vars = var,data = setDT(cohort1),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.pcr.age18-44.csv")
table <- CreateTableOne(vars = var,strata = c("death_1y"),data = setDT(cohort1),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.pcr.death_1y.age18-44.csv")

cohort2 <- cohort.pcr[ageg==2]
table <- CreateTableOne(vars = var,data = setDT(cohort2),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.pcr.age45-64.csv")
table <- CreateTableOne(vars = var,strata = c("death_1y"),data = setDT(cohort2),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.pcr.death_1y.age45-64.csv")

cohort3 <- cohort.pcr[ageg==3]
table <- CreateTableOne(vars = var,data = setDT(cohort3),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.pcr.age65.csv")
table <- CreateTableOne(vars = var,strata = c("death_1y"),data = setDT(cohort3),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.pcr.death_1y.age65.csv")

#### check patients with RAT
table(cohort$COVID.test)
cohort.pcr <- cohort[COVID.test=="RAT"]
table <- CreateTableOne(vars = var,data = setDT(cohort.pcr),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.rat.csv")
table <- CreateTableOne(vars = var,strata = c("death_1y"),data = setDT(cohort.pcr),test=T)
table1 <- print(table)
write.csv(table1, file = "table1.death_1y.rat.csv")

### 2. exclude correlated predictors ######
cohort <- readRDS("final_cohort3.RDS")
cohort <- cohort[,.(id,sex,Age,fully_vacciated,major,mi,hf,stroke,af,dvt,pd,ee,bp,ild,cpd,dx.hx.ards,dx.bw.ards,ards,pancreatitis,li,esr,
                    akd,t1dm,t2dm,htn,anxiety,ptsd,seizure,
                    covid.ct0.low20, inflam.crp, cbc.wbc, cbc.neuphil, cbc.plt, 
                    inflam.esr, inflam.procalcitonin, 
                    inflam.ferritin.mol, inflam.ldh, cbc.lympho, cbc.neuphil.pct,death_1y)]
var <- names(cohort)
var.factor <- var[!var %in% c("Age")]
cohort <- as.data.frame(cohort)
cohort[var.factor] <- lapply(cohort[var.factor],factor)
cohort$covid.ct0.low20 <- relevel(cohort$covid.ct0.low20,ref="2")
cohort$inflam.crp <- relevel(cohort$inflam.crp,ref = "1")
cohort$cbc.wbc <- relevel(cohort$cbc.wbc,ref = "1")
cohort$cbc.neuphil <- relevel(cohort$cbc.neuphil,ref = "1")
cohort$cbc.plt <- relevel(cohort$cbc.plt,ref = "2")
cohort$inflam.esr <- relevel(cohort$inflam.esr,ref = "1")
cohort$inflam.procalcitonin <- relevel(cohort$inflam.procalcitonin,ref = "1")
cohort$inflam.ferritin.mol <- relevel(cohort$inflam.ferritin.mol,ref = "1")
cohort$inflam.ldh <- relevel(cohort$inflam.ldh,ref = "1")
cohort$cbc.lympho <- relevel(cohort$cbc.lympho,ref = "2")
cohort$cbc.neuphil.pct <- relevel(cohort$cbc.neuphil.pct,ref = "1")

### age and sex-adjusted univariate model
age.sex <- glm(as.formula(paste0("death_1y"," ~ Age+sex")), data = cohort,family=binomial)
cbind(exp(cbind(coef(age.sex),confint(age.sex))),melt(c(round(summary(age.sex)[['coefficients']][,'Pr(>|z|)'],3))))[-1,]

v <- var[!var %in% c("id","death_1y","ards","major","Age","sex")]
or2 <- c()
or2_function <- function(outcome){
  for(p in v) {
    cat(p,"\n")
    m <- glm(as.formula(paste0(outcome," ~ Age+sex+",p)), data = cohort,family=binomial)
    or2 <- rbind(or2,cbind(exp(cbind(coef(m),confint(m))),melt(c(round(summary(m)[['coefficients']][,'Pr(>|z|)'],3))))[-(1:3),])
  }
  colnames(or2) <- c("OR","lower","upper","p")
  return(or2)
}
or2_death <- or2_function("death_1y")
sink("univariate analysis.lab.value.txt")
print(or2_death)
sink()

v <- var[!var %in% c("id","death_1y","ards","major","pd","bp","anxiety","htn")] ## exclude insignificant predictors
v2 <- paste(v, collapse = "+")
setDT(cohort)[Age<45,ageg:=1]
cohort[Age>=45 & Age <=64,ageg:=2]
cohort[Age>=65,ageg:=3]
model <- glm(as.formula(paste0("death_1y~", v2)),family = binomial,data=cohort)
## check collinearity
library(performance)
sink("collinearity_multivariable.txt")
check_collinearity(model)
sink()


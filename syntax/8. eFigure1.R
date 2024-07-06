### check death within 30 days after infection ------
cohort <- readRDS("4.cohort.RDS") #n=4308252
cohort <- cohort[Age>=18][(as.Date(death_date_ymd)>=COVID.date & !is.na(death_date_ymd)) | is.na(death_date_ymd)]
## outcome: death within 1 year after index.date
cohort[as.Date(death_date_ymd)<=(index.date+364) & !is.na(death_date_ymd), death_1y := 1]
cohort[is.na(death_1y),death_1y:=0]
cohort[as.Date(death_date_ymd)<=(index.date+30) & !is.na(death_date_ymd), death_acute := 1]
cohort[is.na(death_acute),death_acute:=0]

final <- readRDS("final_cohort3.RDS")
final <- merge(final,cohort[,.(id,death_acute)],by="id",all.x=T)

final.death <- final[,.(id,Age,COVID.date,index.date,death_1y,death_date_ymd)]
add.death <- cohort[!id %in% final$id][,.(id,Age,COVID.date,index.date,death_1y,death_date_ymd)]

death <- rbind(final.death,add.death)
death2 <- death[death_1y==1]
death2$death_date_ymd <- as.Date(death2$death_date_ymd)
death2[,covid.af:=as.numeric(death_date_ymd-COVID.date)]
hist(death2$covid.af)

death2[Age<45,ageg:=1]
death2[Age>=45 & Age <=64,ageg:=2]
death2[Age>=65,ageg:=3]
death2$ageg <- as.factor(death2$ageg)
setorder(death2,covid.af)
death2[,cumulative:=cumsum(death_1y),by=c("ageg")]
death3 <- death2[,.SD[which.max(cumulative)],by=c("covid.af","ageg")]
ggplot(data=death3, aes(x = covid.af,y=cumulative,group= forcats::fct_rev(ageg),color = forcats::fct_rev(ageg)))+ 
  geom_line(stat="identity") + 
  ylab("cumulative death counts") +xlab("days since infection")+
  scale_x_continuous(breaks =seq(0, 400, 30))+
  ggtitle("Cumulative number of deaths after COVID infection") + 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=16),
        axis.text.x = element_text(size=12),
        axis.title = element_text(size = 15))+
  guides(color = guide_legend(title = "Age group"))+
  scale_color_discrete(labels = c("age 65+","age 45-64","age 18-44"))+ 
  geom_vline(xintercept=30)

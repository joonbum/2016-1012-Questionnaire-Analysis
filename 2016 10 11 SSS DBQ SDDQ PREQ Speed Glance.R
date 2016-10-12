# Purpose: to score SDDQ and test correlation with SSS and DBQ
# Author: Joonbum Lee (joonbum@mit.edu)
# last update: 09/30/2016

# note: this is linked in GitHub

# note for Birsen: you can directly jump in [3. Analyze] section after importing preprocessed data ("2016 09 30 SSS DBQ SDDQ PREQ.csv") from the Dropbox. 
# You just need to import and name it as "data.sddq.sss.preq".

# 1. Load ----


# load library (some of them may not be used here)
library(plyr)
library(ggplot2)
library(plyr)
library(DBI)
library(RPostgreSQL);
library(gdata)
library(dplyr)
library(reshape2)
library(ez)
library(lme4)
library(agelabr)
library(corrplot)
library(MASS)
library(magrittr)
library(coefplot)

# load custom functions (some of them may not be used here)
source("~/Dropbox/Works/R codes/MIT/Functions/sss.score.R")
source("~/Dropbox/Works/R codes/MIT/Functions/dbq.score.R")
source("~/Dropbox/Works/R codes/MIT/Jon code/new glance functions.R")
source("~/Dropbox/Works/R codes/MIT/Functions/add.task time.r")
source("~/Dropbox/Works/R codes/MIT/Functions/cor.mtest.R")
source("~/Dropbox/Works/R codes/MIT/Functions/multiplot.R")
source("~/Dropbox/Works/R/Fuction/sddq.score.v1.R")
source("~/Dropbox/Works/R/Fuction/preq.score.R")

# import two sddq data
sddq.2014t <- read.csv("~/Documents/Local data archive/AgeLab2014t/2014t SRSDD Coding.csv", header = TRUE)
sddq.2015b <- read.csv("~/Documents/Local data archive/AgeLab2015b/2015b SRSDD Coding.csv", header = TRUE)

# import technology questions
preq.2014t <- read.csv("~/Documents/Local data archive/AgeLab2014t/2014t Pre Q Coding.csv", header = TRUE) %>% preq.score() 
preq.2015b <- read.csv("~/Documents/Local data archive/AgeLab2015b/2015b Pre Q Coding.csv", header = TRUE) %>% preq.score()

# add unique subject id
preq.2014t$new_subject <- preq.2014t$subject+1000
preq.2015b$new_subject <- preq.2015b$subject+3000


# import previous data. n = 70 (from the complete subject pool)
data.sss <- read.csv("~/Dropbox (Personal)/Works/Massachusetts Institute of Technology (AgeLab)/JB and Birsen/Data/2016 09 14 sss_dbq_glance_speed_renamed variable.csv")
data.all <- read.csv("~/Dropbox (Personal)/Works/Massachusetts Institute of Technology (AgeLab)/JB and Birsen/Data/2016 10 11 SSS DBQ SDDQ PREQ speed for repeated measure design.csv")

# import preprocessed data sets
data.sddq.sss.preq <- read.csv("~/Dropbox (Personal)/Works/Massachusetts Institute of Technology (AgeLab)/JB and Birsen/Data/2016 09 30 SSS DBQ SDDQ PREQ.csv", header = TRUE)
data.baseline.speed <- read.csv("~/Dropbox (Personal)/Works/Massachusetts Institute of Technology (AgeLab)/JB and Birsen/Data/2016 09 29 baseline driving speed.csv", header = TRUE)
data.repeated <- read.csv("~/Dropbox (Personal)/Works/Massachusetts Institute of Technology (AgeLab)/JB and Birsen/Data/2016 09 29 glance data for repeated measures design.csv", header = TRUE)
data.task.speed <- read.csv("~/Dropbox (Personal)/Works/Massachusetts Institute of Technology (AgeLab)/JB and Birsen/Data/2016 10 07 driving speed for repeated measures design.csv")

# this include two trials for all variables
data.all <- read.csv("~/Dropbox (Personal)/Works/Massachusetts Institute of Technology (AgeLab)/JB and Birsen/Data/2016 10 11 SSS DBQ SDDQ PREQ speed for repeated measure design.csv", header= TRUE)





# 2. Clean ----

#::::2.1 For the combined data

# get sddq scores (this requires a "sddq.score" function)
sddq.2014t.output <- sddq.score(sddq.2014t, 1000) # this function takes two input: (a) data name, (b) random number to generate new subject ids (1000 and 3000 used previously for 2014t and 2015b respectively) 
sddq.2015b.output <- sddq.score(sddq.2015b, 3000)

# combine two tables together
sddq.all <- rbind(sddq.2014t.output, sddq.2015b.output)
preq.all <- rbind(preq.2014t, preq.2015b) %>% subset(select = c("new_subject", "Q21", "Q22", "Q23"))

# join sss and sddq
data.sddq.sss <- join(data.sss, sddq.all, by="new_subject")
data.sddq.sss.preq <- join(data.sddq.sss, preq.all, by="new_subject")

#::::2.2 For the seperate trials (final output--data.all--can be loaded without processing codes below)
data.baseline.speed <- data.baseline.speed[,-2]
names(data.baseline.speed)[2]<-"mean_baseline_speed"


# get 70 subs from the original data
list.70 <- unique(data.sddq.sss.preq$new_subject)

# combine two data sets and include only the 70 subs
data.glance.speed <- join(data.repeated, data.task.speed, by=c("new_subject","task"))%>%subset(new_subject%in%list.70)
data.glance.speed.baseline <- join(data.glance.speed, data.baseline.speed, by="new_subject")

var<-c("new_subject", "Gender","Age","study","SSS_Total","SSS_BS","SSS_Dis","SSS_ES","SSS_TAS","DBQ_Error","DBQ_Lapse","DBQ_Violation",
       "Pleasant_mean","Pleasant_sum","Pleasant_na","Safe_mean","Safe_sum","Safe_na","Wise_mean","Wise_sum","Wise_na","Q21",
       "Q22","Q23")

data.sddq.short <-data.sddq.sss.preq[var]

data.all <- join(data.glance.speed.baseline, data.sddq.short, by="new_subject")





# 3. Analyze----

# ::::3.1 Correlation----
#data.sddq.sss.preq <- read.csv("~/Dropbox (Personal)/Works/Massachusetts Institute of Technology (AgeLab)/JB and Birsen/Data/2016 09 30 SSS DBQ SDDQ PREQ.csv", header = TRUE)

set1 <- subset(data.sddq.sss.preq, select=c("SSS_Total","SSS_TAS","SSS_Dis","SSS_ES","SSS_BS","Pleasant_mean","Safe_mean","Wise_mean","DBQ_Error","DBQ_Violation","DBQ_Lapse",
                                       "Mean_duration","SD_duration","Percentage_long_glance","Frequency_glance_pm","task_time","Q21","Q22","Q23"))

ct1<-cor(set1, use="complete")
ct2<-cor.mtest(set1, 0.95)
corrplot(ct1, method="number", type="lower")
corrplot(ct1, p.mat=ct2[[1]], insig="blank", sig.level=0.05, type="lower", method="number", mar=c(0,0,0,0)) # 10 x 10


# subset by age groups
set1.1 <- subset(data.sddq.sss.preq, Age < 40) %>%  subset(select=c("SSS_Total","SSS_TAS","SSS_Dis","SSS_ES","SSS_BS","Pleasant_mean","Safe_mean","Wise_mean","DBQ_Error","DBQ_Violation","DBQ_Lapse",
                                                               "Mean_duration","SD_duration","Percentage_long_glance","Frequency_glance_pm","task_time","Q21","Q22","Q23"))
set1.2 <- subset(data.sddq.sss.preq, Age >= 40) %>%  subset(select=c("SSS_Total","SSS_TAS","SSS_Dis","SSS_ES","SSS_BS","Pleasant_mean","Safe_mean","Wise_mean","DBQ_Error","DBQ_Violation","DBQ_Lapse",
                                                                "Mean_duration","SD_duration","Percentage_long_glance","Frequency_glance_pm","task_time","Q21","Q22","Q23"))

ct1.1<-cor(set1.1, use="complete")
ct2.1<-cor.mtest(set1.1, 0.95)
corrplot(ct1.1, method="number", type="lower")
corrplot(ct1.1, p.mat=ct2.1[[1]], insig="blank", sig.level=0.05, type="lower", method="number", mar=c(0,0,0,0)) # 10 x 10

ct1.2<-cor(set1.2, use="complete")
ct2.2<-cor.mtest(set1.2, 0.95)
corrplot(ct1.2, method="number", type="lower")
corrplot(ct1.2, p.mat=ct2.2[[1]], insig="blank", sig.level=0.05, type="lower", method="number", mar=c(0,0,0,0)) # 10 x 10

cor.test(set1.2$Wise_mean, set1.2$DBQ_Violation)




# quick plots
ggplot(data.sddq.sss.preq, aes(x=Mean_duration, y=Frequency_glance_pm))+geom_point(aes(colour=Age), size=4)+stat_smooth(method="lm", se=FALSE, linetype=3)+theme_bw(16)
ggplot(data.sddq.sss.preq, aes(x=SSS_TAS, y=Q21))+geom_point()+stat_smooth(method="lm")
data.sddq.sss.preq$age_group <- "Older"
data.sddq.sss.preq$age_group[data.sddq.sss.preq$Age >40]<-"Younger"

ggplot(data.sddq.sss.preq, aes(x=SSS_TAS, y=Q21))+geom_point()+stat_smooth(method="lm", se=FALSE, linetype=3)+facet_wrap(~age_group)+ylab("How often do you make\na phone call while driving?\n[1:More than once a day - 6:Never]")+xlab("Thrill and Advanture Seeking")+theme_bw(16)
ggplot(data.sddq.sss.preq, aes(x=Safe_mean, y=Q21))+geom_point()+stat_smooth(method="lm", se=FALSE, linetype=3)+facet_wrap(~age_group)+
  ylab("How often do you make\na phone call while driving?\n[1:More than once a day - 6:Never]")+xlab("EX: Driving in such a scenario and at the same time holding phone conversations on hand-held device is \n[1:Safe - 5:Dangerous]")+theme_bw(16)
ggplot(data.sddq.sss.preq, aes(x=Pleasant_mean, y=Q23))+geom_point()+stat_smooth(method="lm", se=FALSE, linetype=3)+facet_wrap(~age_group)+
  ylab("How often do you answer\na phone call while driving?\n[1:More than once a day - 6:Never]")+xlab("EX: Driving in such a scenario and at the same time holding phone conversations on hand-held device is \n[1:Pleasant - 5:Unpleasant]")+theme_bw(16)

# ::::3.2 Linear model

model.meanduration <- lm(Mean_duration ~ Age + SSS_BS + SSS_Dis + SSS_ES + SSS_TAS + study, data=data.sddq.sss.preq)
model.sdduration <- lm(SD_duration ~ Age + SSS_BS + SSS_Dis + SSS_ES + SSS_TAS + study, data=data.sddq.sss.preq)
model.tasktime <- lm(task_time ~ Age + SSS_BS + SSS_Dis + SSS_ES + SSS_TAS + study, data=data.sddq.sss.preq)
model.longglance <- lm(Percentage_long_glance ~ Age + SSS_BS + SSS_Dis + SSS_ES + SSS_TAS + study, data=data.sddq.sss.preq)
model.frequency <- lm(Frequency_glance_pm ~ Age + SSS_BS + SSS_Dis + SSS_ES + SSS_TAS + study, data=data.sddq.sss.preq)

model.Q21 <- lm(Q21 ~ Age + study + Pleasant_mean + Safe_mean + Wise_mean + SSS_TAS + SSS_BS + task_time, data=data.sddq.sss.preq)
model.Q22 <- lm(Q22 ~ Age + study + Pleasant_mean + Safe_mean + Wise_mean + SSS_TAS + SSS_BS + task_time, data=data.sddq.sss.preq)
model.Q23 <- lm(Q23 ~ Age + study + Pleasant_mean + Safe_mean + Wise_mean + SSS_TAS + SSS_BS + task_time, data=data.sddq.sss.preq)

model.Q21.short <- lm(Q21 ~ Age + study + SSS_TAS + SSS_BS + task_time, data=data.sddq.sss.preq)
model.Q22.short <- lm(Q22 ~ Age + study + SSS_TAS + SSS_BS + task_time, data=data.sddq.sss.preq)
model.Q23.short <- lm(Q23 ~ Age + study + SSS_TAS + SSS_BS + task_time, data=data.sddq.sss.preq)


summary(model.meanduration)
summary(model.sdduration)
summary(model.tasktime)
summary(model.longglance)
summary(model.frequency)

summary(model.Q21)
summary(model.Q22)
summary(model.Q23)

summary(model.Q21.short)
summary(model.Q22.short)
summary(model.Q23.short)

coefplot(model.Q21)+ggtitle("How often do you make a phone call while driving a car?")
coefplot(model.Q23)+ggtitle("How often do you answer a phone call while driving a car?")

# variable selection
step1<-stepAIC(model.meanduration)
step2<-stepAIC(model.sdduration)
step3<-stepAIC(model.tasktime)
step4<-stepAIC(model.longglance)
step5<-stepAIC(model.frequency)

step1$anova
step2$anova
step3$anova
step4$anova
step5$anova

model.meanduration.final <- lm(Mean_duration ~ Age + SSS_TAS + study, data=data.sddq.sss)
model.sdduration.final <- lm(SD_duration ~ Age + SSS_TAS, data=data.sddq.sss)
model.tasktime <- lm(task_time ~ Age + SSS_BS + SSS_Dis + SSS_ES + SSS_TAS + study, data=data.sddq.sss)
model.longglance <- lm(Percentage_long_glance ~ Age + SSS_BS + SSS_Dis + SSS_ES + SSS_TAS + study, data=data.sddq.sss)
model.frequency <- lm(Frequency_glance_pm ~ Age + SSS_BS + SSS_Dis + SSS_ES + SSS_TAS + study, data=data.sddq.sss)


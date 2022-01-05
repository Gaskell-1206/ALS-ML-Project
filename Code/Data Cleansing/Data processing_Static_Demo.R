library(plyr)
library(tidyverse)
library(magrittr)

data.dir <- "./ALS trial/"
zip.name <- "2016_01_04_ALL_FORMS_CSV.zip"
home.dir <- "./ML project"
#if ends with /, not gonna work

# unzip all files
ldply(.data=paste0(data.dir,zip.name), .fun = unzip, exdir = home.dir)
# get the csv files
csv_files <- list.files(path = home.dir, pattern = "*.csv")
setwd(home.dir)

dfs<-list()
for (i in 1:length(csv_files)){
  dfs[[i]]<-read.csv(csv_files[i])
}
lapply(dfs,dim)
lapply(dfs,head)
lapply(dfs,summary)

#10,12,13 what are variables with _Delta? 
#(time in days from trial onset to the date when the assessment was made)

#ignore dfs[[1]] -AEs
head(dfs[[2]]) #alsfrs multi records per id
head(dfs[[3]]) #Alshistory, multi per id
head(dfs[[4]]) #ConMeds,multi records per id
head(dfs[[5]]) #Death,one per id
head(dfs[[6]]) #demo,one per id
head(dfs[[7]]) #famhist,one per id
head(dfs[[8]]) #fvc,multi per id
head(dfs[[9]]) #labs,multi per id
head(dfs[[10]]) #Riluzole,one per id
head(dfs[[11]]) #Svc,multi per id
head(dfs[[12]]) #trt,one per id
head(dfs[[13]]) #vital,multi per id

#start from treatment data
alldata <- dfs[[12]]
length(unique(alldata$subject_id)) #9640
length(unique(dfs[[3]]$subject_id)) #9393
dim(dfs[[3]]) #12058

#death
length(unique(dfs[[5]]$subject_id)) #4633
dim(dfs[[5]]) #4634
death <- dfs[[5]]
death[duplicated(death$subject_id),]$subject_id
death[death$subject_id%in%442984,]

#       subject_id Subject_Died Death_Days
#2055     442984          Yes         84
#2056     442984          Yes         95
#keep the earlier one
death <- death %>% group_by(subject_id) %>% filter(row_number()==1)

#demo
demo<-dfs[[6]]
length(unique(demo$subject_id))
table(demo$subject_id%in%alldata$subject_id) #9640 TRUE

#famhist
famhist<-dfs[[7]]
length(unique(famhist$subject_id)) #1071, 1007 unique
dup<-famhist[duplicated(famhist$subject_id),]$subject_id #64
recs<-famhist[famhist$subject_id%in%dup,] #124 obs

#Riluzole
Rilu<-dfs[[10]] #8817
length(unique(Rilu$subject_id)) #8817


alldata <- dfs[[12]]
alldata <- left_join(alldata,death)
alldata <- left_join(alldata,demo)
alldata <- left_join(alldata,Rilu)
#save.image("workspace 10-1.RData")
summary(alldata)
table(alldata$Study_Arm,useNA = "ifany") #6728 Active, 2912 Placebo
table(alldata[alldata$Treatment_Group_Delta<=93,]$Study_Arm)

#check for multiple records data-use min/max approach
alsfrs<-dfs[[2]] #outcome data
alshist<-dfs[[3]]

#supplement ~87: the score is collected 4 times, visit 0, month 1,2,3 in first 3 months
#total 12 records per person
tabl_n<-alsfrs %>% group_by(subject_id) %>%
  summarize(n=n())
summary(tabl_n$n) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00    6.00    9.00    8.88   12.00   38.00 

#if multiple visits>12 months, assign the earliest visit>12 month as last visit

#months 0-3 as training, 3-12 as testing

#clean main data
main <- alldata[,!names(alldata)%in%c("Subject_Died","Death_Days","Date_of_Birth")]
summary(main)
dim(main)
table(main$Ethnicity, useNA = "ifany")
#         Hispanic or Latino Non-Hispanic or Latino                Unknown 
#7903                     91                   1607                     39 

main$race_white <-ifelse(main$Race_Caucasian%in%1,1,0)
main$race_black <-ifelse(main$Race_Black_African_American%in%1,1,0)
main$race_asiangrp <-ifelse(main$Race_Asian%in%1,1,0)
main$race_othergrps <-ifelse(main$Race_Americ_Indian_Alaska_Native%in%1 |
                               main$Race_Hawaiian_Pacific_Islander %in%1 |
                               main$Race_Unknown %in%1 | main$Race_Other%in%1, 1, 0)

#check overlap
table(main$race_white,main$race_black,useNA = "ifany")
#3548 non-black or white, 5996 white, 96 black

table(main$race_white,main$race_asiangrp,useNA = "ifany")
#5996 white, 51 asian

table(main$race_white,main$race_othergrps,useNA = "ifany")
# 137 other, 5996 white

table(main$Sex,useNA = "ifany") #no missing
table(main$Subject_used_Riluzole,useNA = "ifany")
table(main$Study_Arm)
summary(main$Demographics_Delta) #-35 to 32 days

main.demo<-main[,c(1,2,5,15,18:21)]
#checked that study_arm and sex no missing
main.demo$study_arm_active <- ifelse(main.demo$Study_Arm%in%"Active",1,0)
main.demo$sex_male <- ifelse(main.demo$Sex%in%"Male",1,0)
summary(main.demo)
pacman::p_load("dplyr","magrittr","janitor")

main.demo_out <- main.demo %>% select(1,3,10,5:9)

#write.csv(main.demo_out,file="demo_clean.csv",row.names = F)

vital<-dfs[[13]]
names(vital)
vital[,c(3,5,7,9,11,13,15)]<-sapply(vital[,c(3,5,7,9,11,13,15)], FUN=tolower)
#save.image("workspace 10-1.RData")

pacman::p_load("dplyr","magrittr","janitor","readxl")

#get baseline-note dups here
vital1_baseline <- vital1 %>% filter(Vital_Signs_Delta==0)
summary(vital1_baseline)
#lots of missing
length(unique(vital1[!is.na(vital1$Baseline_Standing_BP_Diastolic),]$subject_id))
#536
sapply(vital1,FUN=function(x)length(unique(vital1[!is.na(x),]$subject_id)))

write.csv(vital1_baseline,"vital_baseline.csv",row.names = F)
#save.image("workspace 10-1.RData")

#only need height and weight(for table 1) from baseline
#add age^2, age^3-could use poly function
#see if height changes
vital_sampleid<-sample(unique(vital1$subject_id),size=100)
vital_temp<-vital1[vital1$subject_id%in%vital_sampleid,]
plot(x=vital_temp$Vital_Signs_Delta,y=vital_temp$Height_cm,col=factor(vital_temp$subject_id),
     ylab="Height, cm",xlab="Delta (days)",main="n=100 subjects randomly selected")
fit<-lm(Height_cm~Vital_Signs_Delta,data=vital1)
summary(fit) #very minor increase although significant

vital1_ht <- vital1 %>% arrange(Vital_Signs_Delta) %>% filter(Vital_Signs_Delta>=0) %>% 
  select(subject_id,Height_cm, Weight_kg) %>% 
  distinct() %>% #9338
  group_by(subject_id) %>% filter(row_number()==1) %>%
  mutate(BMI_baseline=Weight_kg/((Height_cm/100)^2)) %>%
  select(-Weight_kg)
summary(vital1_ht) #7583

main.demo_out <- left_join(main.demo_out, vital1_ht) #9640
#main.demo_out %<>% mutate(Age_square=Age^2, Age_cubic=Age^3)
summary(main.demo_out)
#add race missing indicator
main.demo_out %<>% mutate(race_missing=ifelse(race_white==0 & race_black==0 & race_asiangrp==0 & race_othergrps,
                                              1,0))
table(main.demo_out$race_missing,useNA = "ifany")
#write.csv(main.demo_out,file="cleaned/demo_clean.csv",row.names = F)

#merge with training ids and outcome in 3 months
train_id=read.csv("cleaned/training_set.csv",header=T)
train_ids <- unique(train_id$subject_id) #2485

outcome=read.csv("cleaned/ALSFRS_following_9_months.csv",header=T) #2026

y_train=outcome[outcome$subject_id%in%train_ids,] #1222

#subset demo to training
demo_train<-main.demo_out%>%filter(subject_id%in%y_train$subject_id) #1104
#write.csv(demo_train,file="cleaned/demo_clean_train.csv",row.names = F)

#subset demo to testing
test_id<-read.csv("cleaned/test_set.csv")
test_ids <- unique(test_id$subject_id) #829
y_test<-outcome[outcome$subject_id%in%test_ids,] #423
demo_test<-main.demo_out%>%filter(subject_id%in%y_test$subject_id) #388
#write.csv(demo_test,file="cleaned/demo_clean_test.csv",row.names = F)

#subset demo to validate
valid_id<-read.csv("cleaned/validation_set.csv")
valid_ids<-unique(valid_id$subject_id) #828
y_valid<-outcome[outcome$subject_id%in%valid_ids,] #381
demo_valid<-main.demo_out%>%filter(subject_id%in%y_valid$subject_id) #348
#write.csv(demo_valid,file="cleaned/demo_clean_valid.csv",row.names = F)

#save(main,main.demo,main.demo_out,vital1_baseline,vital1_ht,file="main-demo.RData")
#save(alldata,death,demo,dfs,dup,famhist,Rilu,vital,file="workspace 10-1.RData")
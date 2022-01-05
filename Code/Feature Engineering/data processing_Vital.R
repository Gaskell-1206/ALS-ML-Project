load("workspace 10-1.RData")
pacman::p_load("dplyr","magrittr","janitor")

#check if units same
for (i in c(3,5,7,9,11,13,15)){
  cat(names(vital[i]))
  print(table(vital[,i],useNA = "ifany"))
}

#height - only need baseline
#Height_Units
#      centimeters      inches 
#65016        6947         459 
#Weight_Units
#      kilograms    pounds 
#36427     35709       286 

vital$Height_cm<-ifelse(vital$Height_Units%in%"inches",vital$Height*2.54,vital$Height)
vital$Weight_kg<-ifelse(vital$Weight_Units%in%"pounds",vital$Weight/2.205,vital$Weight)
summary(vital) 

#remove units and weight, height raw variables
vital1<-vital[,-c(3,5:7,9,11,13:15)] %>% select(subject_id,Vital_Signs_Delta,everything())

#save(vital1,file="vital.RData")

#Baseline_Standing_BP_Diastolic_mean Baseline_Standing_BP_Systolic_mean
#Baseline_Supine_BP_Diastolic_mean Baseline_Supine_BP_Systolic_mean Baseline_Weight_mean
#Endpoint_Supine_Pulse_mean Endpoint_Standing_Pulse_mean Baseline_Supine_Pulse_mean 
#Baseline_Standing_Pulse_mean
#Endpoint_Weight_mean Endpoint_Standing_BP_Diastolic_mean Endpoint_Standing_BP_Systolic_mean 
#Endpoint_Supine_BP_Systolic_mean

vital1_bsvar<-vital1 %>% select(subject_id,Vital_Signs_Delta,contains(c("Baseline","Endpoint")) )
names(vital1_bsvar)
#save(vital1_bsvar,file="vital baseline endpoint.RData")
load("vital baseline endpoint.RData")

#remove NAs for each var, memory issue, need distinct
out<-NULL
for (i in 3:16){ #14 features
  temp<-vital1_bsvar[,c(1,i)]
  temp<-temp[!is.na(temp[,2]),] %>% distinct()#3858
  out[[i-2]]<-temp
}
lapply(out,dim) #480~550

out_dt<-NULL
for (i in 1:length(out)){
  if (i==1) out_dt<-out[[1]]
  else out_dt<-full_join(out_dt,out[[i]])
}
summary(out_dt)
length(unique(out_dt$subject_id)) #550
vital1_static<-out_dt
#write.csv(vital1_static,file="cleaned/vital static.csv",row.names = F)
#save(vital1_bsvar,vital1_static,file="vital baseline endpoint.RData")

load("vital.RData")
#by id generate min, max, average-among non-baseline-for first 3 months!
#time series data features
#3 month min, mean, max, sd,(including baseline) 
#last measurement, number of measurements, delta for first and last measurement
#slope of time series-(last-first)/(last delta-first delta)
names(vital1)
#first non missing height
vital1_first_ht<-vital1 %>% arrange(Vital_Signs_Delta) %>% group_by(subject_id) %>%
  summarise(Height_cm_firstavail=first(Height_cm[!is.na(Height_cm)])) %>%
  rename(Height_cm=Height_cm_firstavail)
#9973
summary(vital1_first_ht)
vital1<-left_join(vital1 %>% select(-Height_cm), vital1_first_ht) %>% 
  mutate(BMI=Weight_kg/((Height_cm/100)^2))
summary(vital1) #missing due to missing height-use first non missing height!

#remove static: Baseline ** or Endpoint **
vital1 %<>% select(-contains(c("Baseline","Endpoint")))
#now 14 features
#save(vital1,file="vital_imp_ht.RData")

load(file="vital_imp_ht.RData")
#mean
vital1_mean_3m <- vital1 %>% filter(Vital_Signs_Delta>=0 & Vital_Signs_Delta<=92) %>% 
  select(-Height_cm) %>% group_by(subject_id) %>%
  summarise( across(Blood_Pressure_Diastolic:BMI,function(x)mean(x,na.rm=T)) ) #
names(vital1_mean_3m)[2:ncol(vital1_mean_3m)]<-sapply(names(vital1_mean_3m)[2:ncol(vital1_mean_3m)],FUN=function(x)paste(x,"mean",sep="_"))
summary(vital1_mean_3m)
length(unique(vital1_mean_3m$subject_id)) #7624
#Temperature (NA=4015), Supine Pulse, Standing Pulse, Supine BP Dias and Sys
#Standing BP Sys and Dias (NA~6300/7624)

#min
vital1_min_3m <- vital1 %>% filter(Vital_Signs_Delta>=0 & Vital_Signs_Delta<=92) %>% 
  select(-Height_cm) %>% group_by(subject_id) %>%
  summarise( across(Blood_Pressure_Diastolic:BMI, ~min(.x,na.rm=T)) ) #
#50 warnings,no non-missing arguments to min; returning Inf-all missing for an individual
names(vital1_min_3m)[2:ncol(vital1_min_3m)]<-sapply(names(vital1_min_3m)[2:ncol(vital1_min_3m)],FUN=function(x)paste(x,"min",sep="_"))
#replace Inf with NA
for (i in 1:nrow(vital1_min_3m)){
  for (j in 1:ncol(vital1_min_3m)){
    if (vital1_min_3m[i,j]=="Inf") vital1_min_3m[i,j]<-NA
  }
}
summary(vital1_min_3m) #7624

#max
vital1_max_3m <- vital1 %>% filter(Vital_Signs_Delta>=0 & Vital_Signs_Delta<=92) %>% 
  select(-Height_cm) %>% group_by(subject_id) %>%
  summarise( across(Blood_Pressure_Diastolic:BMI,~max(.x,na.rm=T)) ) #
names(vital1_max_3m)[2:ncol(vital1_max_3m)]<-sapply(names(vital1_max_3m)[2:ncol(vital1_max_3m)],FUN=function(x)paste(x,"max",sep="_"))
#replace Inf with NA
for (i in 1:nrow(vital1_max_3m)){
  for (j in 1:ncol(vital1_max_3m)){
    if (vital1_max_3m[i,j]=="-Inf") vital1_max_3m[i,j]<-NA
  }
}
summary(vital1_max_3m)

#sd
vital1_sd_3m <- vital1 %>% filter(Vital_Signs_Delta>=0 & Vital_Signs_Delta<=92) %>% 
  select(-Height_cm) %>% group_by(subject_id) %>%
  summarise( across(Blood_Pressure_Diastolic:BMI,~sd(.x,na.rm=T)) ) #
names(vital1_sd_3m)[2:ncol(vital1_sd_3m)]<-sapply(names(vital1_sd_3m)[2:ncol(vital1_sd_3m)],FUN=function(x)paste(x,"sd",sep="_"))
#7624
summary(vital1_sd_3m)

#last non-missing & delta
vital1_last_3m <- vital1 %>% filter(Vital_Signs_Delta>=0 & Vital_Signs_Delta<=92) %>% 
  select(-Height_cm) %>% group_by(subject_id) %>% arrange(Vital_Signs_Delta) %>%
  summarize( across(Vital_Signs_Delta:BMI,~last(.x[!is.na(.x)])))
#7624
names(vital1_last_3m)[2:ncol(vital1_last_3m)]<-sapply(names(vital1_last_3m)[2:ncol(vital1_last_3m)],FUN=function(x)paste(x,"last",sep="_"))
summary(vital1_last_3m)

#first delta
vital1_first_3m <- vital1 %>% group_by(subject_id) %>% arrange(Vital_Signs_Delta) %>% filter(Vital_Signs_Delta>=0) %>%
  filter(row_number()==1) %>% select(subject_id,Vital_Signs_Delta) %>% rename(Vital_Signs_Delta_first=Vital_Signs_Delta)
summary(vital1_first_3m) #-243 to 155, 3rd Q is 0
#not use
table(vital1_first_3m$Vital_Signs_Delta_first>0) #59 non zero/7624

#number of measurements-nonmissing
vital1_nmeas_3m <- vital1 %>% filter(Vital_Signs_Delta>=0 & Vital_Signs_Delta<=92) %>% 
  select(-Height_cm) %>% group_by(subject_id) %>% arrange(Vital_Signs_Delta) %>%
  summarize( across(Blood_Pressure_Diastolic:BMI,~sum(!is.na(.x))))
names(vital1_nmeas_3m)[2:ncol(vital1_nmeas_3m)]<-sapply(names(vital1_nmeas_3m)[2:ncol(vital1_nmeas_3m)],FUN=function(x)paste(x,"nmeas",sep="_"))
summary(vital1_nmeas_3m)

vital1_clean<-full_join(vital1_mean_3m,vital1_sd_3m)
vital1_clean<-full_join(vital1_clean,vital1_min_3m)
vital1_clean<-full_join(vital1_clean,vital1_max_3m)
vital1_clean<-full_join(vital1_clean,vital1_first_3m)
vital1_clean<-full_join(vital1_clean,vital1_last_3m)
vital1_clean<-full_join(vital1_clean,vital1_nmeas_3m)
summary(vital1_clean)

#write.csv(vital1_clean,file="cleaned/vital_clean.csv",row.names = F)
#save(vital1_clean,file="vital1_clean.RData")
#save(vital1_first_3m,vital1_last_3m,vital1_max_3m,vital1_mean_3m,vital1_min_3m,
#     vital1_nmeas_3m,vital1_sd_3m,file="vital1 workspace.RData")

#check if nmeas<2 then slope=0 for 1 measurement
names(vital1_clean)
for (i in c(73:79)){
  print(table(vital1_clean[,i]<2))
  print(table(vital1_clean[,i]==1))
}
#4459 and 6896/6895/6894 true<2
#444 558/587/585/566 true=1
#time series data features
#linear regression (intercept, slope, correlation)? nope...
#slope last-first/t2-t1
#note this vital1 has not dropped baseline and endpoint-"vital.RData"
set.seed(567)
vital1_sampleid<-sample(unique(vital1$subject_id),size=100)
vital1_temp<-vital1[vital1$subject_id%in%vital1_sampleid,]
dim(vital1_temp) #30 vars
names(vital1_temp)

library(ggplot2)

pdf(file="spaghetti plot.pdf")
for (i in 3:28){ #29-check index
  ploti<-ggplot(data=vital1_temp,aes(x=Vital_Signs_Delta,y=vital1_temp[,i],col=factor(subject_id)))+
    geom_line()+theme_classic()+theme(legend.position="none")+ylab(names(vital1_temp)[i])+xlab('Delta (days)')+
    ggtitle(label="n=100 subjects randomly selected")
  print(ploti)
}
dev.off()

#so drop certain features-Baseline **, Supine **, Standing **, Endpoint **, later first do a correlation plot
#vital1_clean2 <- vital1_clean %>% select(-contains(c("Baseline","Supine","Standing","Endpoint")))
#summary(vital1_clean2)

#drop>80% missing
vital1_clean2 <- vital1_clean %>% select(-contains(c("Supine","Standing")))
summary(vital1_clean2)

#write.csv(vital1_clean2,file="cleaned/vital_clean_drop_missing.csv",row.names = F)
#save(vital1_clean,vital1_clean2,file="vital1_clean.RData")

#get slope
vital1_first_slope<- vital1 %>% filter(Vital_Signs_Delta>=0 & Vital_Signs_Delta<=92) %>%
  select(-contains(c("Supine","Standing","Height"))) %>%
  group_by(subject_id) %>%
  arrange(Vital_Signs_Delta) %>%
  filter(row_number()==1)
names(vital1_first_slope)[2:ncol(vital1_first_slope)]<-sapply(names(vital1_first_slope)[2:ncol(vital1_first_slope)],function(x)paste(x,"first",sep="_"))


vital1_last_slope<- vital1 %>% filter(Vital_Signs_Delta>=0 & Vital_Signs_Delta<=92) %>%
  select(-contains(c("Supine","Standing","Height"))) %>%
  group_by(subject_id) %>%
  arrange(desc(Vital_Signs_Delta)) %>%
  filter(row_number()==1)
names(vital1_last_slope)[2:ncol(vital1_last_slope)]<-sapply(names(vital1_last_slope)[2:ncol(vital1_last_slope)],function(x)paste(x,"last",sep="_"))

vital1_slope<-inner_join(vital1_first_slope,vital1_last_slope)
names(vital1_slope)

for (i in 3:9){
  vital1_slope[,(ncol(vital1_slope)+1)]<-(vital1_slope[,i+8]-vital1_slope[,i])/(vital1_slope$Vital_Signs_Delta_last-vital1_slope$Vital_Signs_Delta_first)
  names(vital1_slope)[ncol(vital1_slope)]<-sapply(names(vital1_slope)[i],
                                                  function(x)paste(substr(x,1,nchar(x)-6),"slope",sep="_"))
}
summary(vital1_slope)

vital1_clean2<-left_join(vital1_clean2,vital1_slope[,c(1,18:24)])
#write.csv(vital1_clean2,file="cleaned/vital_clean2.csv",row.names = F)
#save(vital1_first_slope,vital1_last_slope,vital1_slope,vital1_clean2,
#     file="vital slope workspace.RData")

#merge with outcome and trainid
train_id=read.csv("cleaned/training_set.csv",header=T)
train_ids <- unique(train_id$subject_id) #2485

outcome=read.csv("cleaned/ALSFRS_following_9_months.csv",header=T) #2026

y_train=outcome[outcome$subject_id%in%train_ids,] #1222
vital1_clean_train<-vital1_clean2[vital1_clean2$subject_id%in%y_train$subject_id,]
names(vital1_clean_train)
dim(vital1_clean_train) #1122*52
summary(vital1_clean_train) 
#write.csv(vital1_clean_train,file="cleaned/vital_clean_train.csv",row.names = F)


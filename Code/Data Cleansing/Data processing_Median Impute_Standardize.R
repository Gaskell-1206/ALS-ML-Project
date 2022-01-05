library(ggplot2)
library(magrittr)
library(dplyr)

dat<-read.csv("Train-Test/training.csv",header=T) 
dat<-dat %>% select(-X)
#annotate binary and categorical variables
#1710 * 325 (1 subject_id, 1 outcome)

#process testing, validation using same codes one by one
test<-read.csv("Train-Test/testing.csv",header=T)
dat<-test %>% select(-X)
valid<-read.csv("Train-Test/validation.csv",header=T)
dat<-valid %>% select(-X)

#calculate missing % for continuous
out<-NULL
for (j in 1:ncol(dat)){
  missj<-sum(is.na(dat[,j]))/nrow(dat)*100
  missj<-c(names(dat)[j],missj)
  out<-rbind(out,missj)
}
out

#neurodisease and family history only subset had form, impute NA with 0
#also 0 for NA in riluzole use
for (i in c(18:27,302)){
  dat[,i]<-ifelse(is.na(dat[,i]),0,dat[,i])
  print(table(is.na(dat[,i])))
}

#drop those with all missing svc variables
dat %<>% select(!starts_with("svc_"))

#impute missing using median/mean for continuous 
cont_ind<-c(10,11,29,40:301,303:325)
for (i in 1:length(cont_ind)){
  #i=1
  coli<-cont_ind[i]
  dat[,coli]<-as.numeric(dat[,coli])
  
  imputei<-median(dat[,coli],na.rm=T)
  dat[,coli]<-ifelse(is.na(dat[,coli]),imputei,dat[,coli])
  
}

#impute mode for categorical and binary-no more with missing

#code siteofonset as dummy
table(dat$SiteOfOnset,useNA = "ifany") #0,1,2,5, only 1=0, probably use 1 as reference best
#merge 0 to group 1
#no 0 in testing or validation
dat$site_onset1=ifelse(dat$SiteOfOnset%in%c(1,0),1,0)
dat$site_onset2=ifelse(dat$SiteOfOnset==2,1,0)
dat$site_onset5=ifelse(dat$SiteOfOnset==5,1,0)
dat<-dat %>% select(-c(SiteOfOnset)) #race_missing(part of othergrp)

write.csv(dat,"Train-Test/training_impute_median.csv",row.names = F)
write.csv(dat,"Train-Test/testing_impute_median.csv",row.names = F)
write.csv(dat,"Train-Test/validation_impute_median.csv",row.names = F)

#normalize continuous features
bin_ind<-c(1,2,4:9,12,18:27,29:38,301,325:327) #subject_id and outcome also don't need std
dat_std<-dat
dat_std[,-bin_ind]<-apply(dat_std[,-bin_ind],2,scale)
apply(dat_std[,c(3,10,11,13:17)],2,mean)
apply(dat_std[,c(3,10,11,13:17)],2,sd)

write.csv(dat_std,file="Train-Test/training_impute_median_norm.csv",row.names = F)
write.csv(dat_std,"Train-Test/testing_impute_median_norm.csv",row.names = F)
write.csv(dat_std,"Train-Test/validation_impute_median_norm.csv",row.names = F)

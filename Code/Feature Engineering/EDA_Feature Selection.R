library(ggplot2)
library(magrittr)
library(dplyr)

dat<-read.csv("Train-Test/training.csv",header=T) #temp_training
dat<-dat %>% select(-X)
#1710 * 302 (1 subject_id)

#loess plot-for continuous variables
cont_ind<-c(2:7,14,15,28,39:300,302:322)

pdf(file="loess plots-median imp1.pdf")
for (i in 1:57){ #length(cont_ind),%/% integer division
  #i=1

    coli<-cont_ind[i]
    dat[,coli]<-as.numeric(dat[,coli])

    pi<-ggplot(data=dat,aes(x=dat[,coli],y=slope_following_9_months))+geom_point()+
      geom_smooth(method="loess",formula='y~x')+theme_classic()+
      theme(panel.grid = element_blank())+xlab(names(dat)[coli])+ylab("Slope: 3 to 12 month")

  print(pi)
}
dev.off()
pdf(file="loess plots-median imp2.pdf")
for (i in 58:104){ #length(cont_ind),%/% integer division
  #i=1
  
  coli<-cont_ind[i]
  dat[,coli]<-as.numeric(dat[,coli])
  
  pi<-ggplot(data=dat,aes(x=dat[,coli],y=slope_following_9_months))+geom_point()+
    geom_smooth(method="loess",formula='y~x')+theme_classic()+
    theme(panel.grid = element_blank())+xlab(names(dat)[coli])+ylab("Slope: 3 to 12 month")
  
  print(pi)
}
dev.off()
pdf(file="loess plots-median imp3.pdf")
for (i in 105:161){ #length(cont_ind),%/% integer division
  #i=1
  
  coli<-cont_ind[i]
  dat[,coli]<-as.numeric(dat[,coli])
  
  pi<-ggplot(data=dat,aes(x=dat[,coli],y=slope_following_9_months))+geom_point()+
    geom_smooth(method="loess",formula='y~x')+theme_classic()+
    theme(panel.grid = element_blank())+xlab(names(dat)[coli])+ylab("Slope: 3 to 12 month")
  
  print(pi)
}
dev.off()
pdf(file="loess plots-median imp4.pdf")
for (i in 162:218){ #length(cont_ind),%/% integer division
  #i=1
  
  coli<-cont_ind[i]
  dat[,coli]<-as.numeric(dat[,coli])
  
  pi<-ggplot(data=dat,aes(x=dat[,coli],y=slope_following_9_months))+geom_point()+
    geom_smooth(method="loess",formula='y~x')+theme_classic()+
    theme(panel.grid = element_blank())+xlab(names(dat)[coli])+ylab("Slope: 3 to 12 month")
  
  print(pi)
}
dev.off()
pdf(file="loess plots-median imp5.pdf")
for (i in 219:286){ #length(cont_ind),%/% integer division
  #i=1
  
  coli<-cont_ind[i]
  dat[,coli]<-as.numeric(dat[,coli])
  
  pi<-ggplot(data=dat,aes(x=dat[,coli],y=slope_following_9_months))+geom_point()+
    geom_smooth(method="loess",formula='y~x')+theme_classic()+
    theme(panel.grid = element_blank())+xlab(names(dat)[coli])+ylab("Slope: 3 to 12 month")
  
  print(pi)
}
dev.off()
#simple linear regression
#continuous or binary
cont_ind<-c(2:5,7,14,15,28,39:300,302:322)
var_ind<-c(8,13,17:26,29:38,301,cont_ind) #314
out<-NULL
for (i in 1:length(var_ind)){
  #i=1
  coli<-var_ind[i]
  dat[,coli]<-as.numeric(dat[,coli])
  fit<-lm(slope_following_9_months~dat[,coli],data=dat)
  beta<-coef(fit)[2]
  se<-summary(fit)$coef[2,2]
  pval<-summary(fit)$coef[2,4]
  r_square<-summary(fit)$r.squared
  rowi<-data.frame(Beta=beta,SE=se,P_Value=pval,RSQ=r_square)
  row.names(rowi)<-names(dat)[coli]
  out<-rbind(out,rowi)
}
head(out)
#write.csv(out,file="output/SLR.csv")

#for categorical
#race, omit white-reference group
#SiteOfOnset
fit<-lm(slope_following_9_months~race_black+race_asiangrp+race_othergrps,data=dat)
summary(fit)
sum_fun<-function(fit){
  beta<-coef(fit)[2:length(coef(fit))]
  summ<-summary(fit)
  se<-summ$coef[2:nrow(summ$coef),2]
  pval<-summ$coef[2:nrow(summ$coef),4]
  r_square<-summ$r.squared
  rowi<-data.frame(Beta=beta,SE=se,
                   P_Value=pval,
                   RSQ=c(r_square,rep(NA,length(beta)-1)) )
  return(rowi)  
}

raceout<-sum_fun(fit) #missing grouped with other

table(dat$SiteOfOnset,useNA = "ifany")
#fit<-lm(slope_following_9_months~factor(SiteOfOnset),data=dat)
fit<-lm(slope_following_9_months~site_onset2+site_onset5,data=dat)
siteout<-sum_fun(fit)

#write.csv(rbind(raceout,siteout),file="output/SLR-category.csv")

#lasso
library(glmnet)

X=as.matrix(dat[,-c(1:2)]) #subject_id,outcome, ALSFRS related features
y_train=dat$slope_following_9_months

lambdas <- 10^seq(2, -3, by = -.1)
set.seed(789)
#specified standardize=T
lasso_reg <- cv.glmnet(X, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 10)
summary(lasso_reg)
lasso_reg$glmnet.fit
lasso_reg$lambda.min #0.0316
str(lasso_reg)

lambda=0.03162278
set.seed(789)
#with known lambda use glmnet without CV
lasso_reg <- glmnet(X, y_train, alpha = 1, lambda =lambda, standardize = TRUE)
lasso_reg
betas <- lasso_reg$beta[,1]
head(betas)

table(betas>0) #18 true
out1<- as.matrix(betas[betas>0])

#perhaps shouldn't normalize binary vars

#normalize all features-excluding binary!
bin_ind<-c(2:7,10:19,21:31,293,315:316)
X_bin<-X[,bin_ind]
X_std<-apply(X[,-bin_ind],2,scale)
apply(X_std[,1:10],2,mean)
apply(X_std[,1:10],2,sd)
X_std<-cbind(X_bin,X_std)

#run lasso again
lambdas <- 10^seq(2, -3, by = -.1)
set.seed(789)
lasso_reg <- cv.glmnet(X_std, y_train, alpha = 1, lambda = lambdas, standardize = F, nfolds = 10)
lasso_reg
lasso_reg$lambda.min #0.025 if turn standardize off

lambda=0.02511886
set.seed(789)
#with known lambda use glmnet without CV
lasso_reg <- glmnet(X_std, y_train, alpha = 1, lambda =lambda, standardize = F)
lasso_reg
betas <- lasso_reg$beta[,1]
head(betas)

table(betas>0) #18 true
out<- as.matrix(betas[betas>0])
write.csv(out,file="output/lasso selection2.csv")

#Lasso barplot
beta_res<-rbind(out,as.matrix(betas[betas==0][1:2])) #20
beta_res %<>% as.data.frame() %>% arrange(desc(V1))
beta_res$domain <- c("FVC","Vital",rep("Lab",4),rep("Vital",4),rep("Lab",2),
                     rep(c("Vital","Lab"),2),rep("Lab",2),rep("Demographics",2))
beta_res$X<-row.names(beta_res)

levs<-beta_res$X
beta_res$X = factor(beta_res$X,levels=levs)

ggplot(data=beta_res,aes(x=V1,y=X,fill=domain)) + geom_col() + theme_classic() +
  xlab("Beta Coefficient") + ylab("") + theme(legend.title = element_blank())


#barplots for SLR
res<-read.csv("output/SLR.csv",header=T)
#remove ALSFRS related
#select P<0.05, top 20 features
res_sig <- res %>% 
  filter(P_Value<0.05) %>% #N=79
  filter(!X%in%c("ALSFRS_last_visit","Delta_last_visit","ALSFRS_first_visit_after_3_months")) %>% 
  arrange(P_Value) %>%
  filter(row_number()%in%1:20)
res_sig$domain <- c("Symptom",rep("FVC",5),"Symptom",rep("Vital",2),"Symptom",
                    "FVC",rep("Lab",5),"Vital",rep("Lab",2),"Vital")

#sort by beta desc
res_sig2 <- res_sig %>% arrange(desc(Beta))
levs<-res_sig2$X
res_sig2$X = factor(res_sig2$X,levels=levs)
#scale beta
res_sig2[res_sig2$X%in%"SymOnsetDelta",]$Beta <- res_sig2[res_sig2$X%in%"SymOnsetDelta",]$Beta*100
res_sig2[res_sig2$X%in%"OnsetDelta",]$Beta <- res_sig2[res_sig2$X%in%"OnsetDelta",]$Beta*100
res_sig2[res_sig2$X%in%"Weight_kg_last",]$Beta <- res_sig2[res_sig2$X%in%"Weight_kg_last",]$Beta*5

ggplot(data=res_sig2,aes(x=Beta,y=X,fill=domain)) + geom_col() + theme_classic() +
  xlab("Beta Coefficient") + ylab("") + theme(legend.title = element_blank())





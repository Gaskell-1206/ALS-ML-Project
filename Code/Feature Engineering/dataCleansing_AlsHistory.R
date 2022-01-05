setwd('/Users/ruoxunzi/Desktop/Machine_Learning/Project/RawData/')
AlsHistory <- read.csv('AlsHistory.csv')

subject_id <- unique(AlsHistory$subject_id)
n_id <- length(subject_id)

# 'Symptom'
AlsHistory$Symptom <- replace(AlsHistory$Symptom, AlsHistory$Symptom == 'Weakness','WEAKNESS')
x <- as.data.frame(table(AlsHistory$Symptom))
x <- x[order(x$Freq,decreasing = TRUE),]
AlsHistory$Symptom <- replace(AlsHistory$Symptom, AlsHistory$Symptom == x$Var1[1],NA)

# 'Location'
x1 <- as.data.frame(table(AlsHistory$Location))
x1 <- x1[order(x1$Freq,decreasing = TRUE),]

# 'Site_of_Onset'
onsetSum <- as.data.frame(table(AlsHistory$Site_of_Onset))
onsetSum <- onsetSum[order(onsetSum$Freq,decreasing = TRUE),]

# make the cleaning table
AlsHistory1 <-data.frame(subject_id)
AlsHistory1$SiteBulbar <- rep(0, n_id)
AlsHistory1$DeltaBulbar <- rep(NA, n_id)
AlsHistory1$SiteLimb <- rep(0, n_id)
AlsHistory1$DeltaLimb <- rep(NA, n_id)
AlsHistory1$SiteLimbAndBulbar <- rep(0, n_id)
AlsHistory1$DeltaLimbAndBulbar <- rep(NA, n_id)
AlsHistory1$SiteOther <- rep(0, n_id)
AlsHistory1$DeltaOther <- rep(NA, n_id)
AlsHistory1$SiteSpine <- rep(0, n_id)
AlsHistory1$DeltaSpine <- rep(NA, n_id)
for (i in 1:n_id)
{
  row <- which(subject_id[i] == AlsHistory$subject_id)
  for (j in 1:length(row))
  {
    bulbar <- AlsHistory$Site_of_Onset___Bulbar[row[j]]
    if(!is.na(bulbar))
    {
      AlsHistory1$SiteBulbar[i] <- bulbar
      AlsHistory1$DeltaBulbar[i] <- AlsHistory$Onset_Delta[row[j]]
    }
    limb <- AlsHistory$Site_of_Onset___Limb[row[j]]
    if(!is.na(limb))
    {
      AlsHistory1$SiteLimb[i] <- limb
      AlsHistory1$DeltaLimb[i] <- AlsHistory$Onset_Delta[row[j]]
    }
    limbandbulbar <- AlsHistory$Site_of_Onset___Limb_and_Bulbar[row[j]]
    if(!is.na(limbandbulbar))
    {
      AlsHistory1$SiteLimbAndBulbar[i] <- limbandbulbar
      AlsHistory1$DeltaLimbAndBulbar[i] <- AlsHistory$Onset_Delta[row[j]]
    }
    other <- AlsHistory$Site_of_Onset___Other[row[j]]
    if(!is.na(other))
    {
      AlsHistory1$SiteOther[i] <- other
      AlsHistory1$DeltaOther[i] <- AlsHistory$Onset_Delta[row[j]]
    }
    spine <- AlsHistory$Site_of_Onset___Spine[row[j]]
    if(!is.na(spine))
    {
      AlsHistory1$SiteSpine[i] <- spine
      AlsHistory1$DeltaSpine[i] <- AlsHistory$Onset_Delta[row[j]]
    }
    onset <- AlsHistory$Site_of_Onset[row[j]]
    idx <- which(onset == onsetSum$Var1)
    if(idx == 2)
    {
      AlsHistory1$SiteLimb[i] <- AlsHistory1$SiteLimb[i]+1
      AlsHistory1$DeltaLimb[i] <- AlsHistory$Onset_Delta[row[j]]
    }else if (idx == 3)
    {
      AlsHistory1$SiteBulbar[i] <- AlsHistory1$SiteBulbar[i]+1
      AlsHistory1$DeltaBulbar[i] <- AlsHistory$Onset_Delta[row[j]]
    }else if (idx == 4)
    {
      AlsHistory1$SiteOther[i] <- AlsHistory1$SiteOther[i]+1
      AlsHistory1$DeltaOther[i] <- AlsHistory$Onset_Delta[row[j]]
    }else if (idx == 5)
    {
      AlsHistory1$SiteSpine[i] <- AlsHistory1$SiteSpine[i]+1
      AlsHistory1$DeltaSpine[i] <- AlsHistory$Onset_Delta[row[j]]
    }else if (idx == 6)
    {
      AlsHistory1$SiteLimbAndBulbar[i] <- AlsHistory1$SiteLimbAndBulbar[i]+1
      AlsHistory1$DeltaLimbAndBulbar[i] <- AlsHistory$Onset_Delta[row[j]]
    }
  }
}
AlsHistory1$SiteSum <- apply(AlsHistory1[,c(2,4,6,8,10)],1,sum)
# To test if 'Bulbar' and 'Limb' onset on one day and change to 'BulbarAndLimb'
temp <- AlsHistory1[which(AlsHistory1$SiteSum==2),]
test_row <- integer(dim(temp)[1])
for (i in 1:dim(temp)[1])
{
  test_row[i] <- length(which(temp$subject_id[i] == AlsHistory$subject_id))
  row <- which(temp$subject_id[i] == AlsHistory1$subject_id)
  AlsHistory1$SiteLimb[row] <- 0
  AlsHistory1$SiteBulbar[row] <- 0
  AlsHistory1$SiteLimbAndBulbar[row] <- 1
  AlsHistory1$DeltaLimbAndBulbar[row] <- AlsHistory1$DeltaLimb[row]
  AlsHistory1$DeltaLimb[row] <- 0
  AlsHistory1$DeltaBulbar[row] <- 0
}
AlsHistory1$SiteSum <- apply(AlsHistory1[,c(2,4,6,8,10)],1,sum)
onsetSum$Freq2<-c(length(which(AlsHistory1$SiteSum==0)),sum(AlsHistory1$SiteLimb),
            sum(AlsHistory1$SiteBulbar),sum(AlsHistory1$SiteOther),
            sum(AlsHistory1$SiteSpine),sum(AlsHistory1$SiteLimbAndBulbar))

# Add a new column as numeric: 0-NA, 1-Limb,2-Bulbar,3-Other,4-Spine,5-Limb and Bulbar
AlsHistory1$SiteOfOnset <- rep(0, n_id)
AlsHistory1$OnsetDelta <- rep(NA, n_id)
for (i in 1:n_id)
{
  if (AlsHistory1$SiteLimb[i] == 1)
  {
    AlsHistory1$SiteOfOnset[i] <- 1
    AlsHistory1$OnsetDelta[i] <- AlsHistory1$DeltaLimb[i]
  }else if (AlsHistory1$SiteBulbar[i] == 1)
  {
    AlsHistory1$SiteOfOnset[i] <- 2
    AlsHistory1$OnsetDelta[i] <- AlsHistory1$DeltaBulbar[i]
  }else if (AlsHistory1$SiteOther[i] == 1)
  {
    AlsHistory1$SiteOfOnset[i] <- 3
    AlsHistory1$OnsetDelta[i] <- AlsHistory1$DeltaOther[i]
  }else if (AlsHistory1$SiteSpine[i] == 1)
  {
    AlsHistory1$SiteOfOnset[i] <- 4
    AlsHistory1$OnsetDelta[i] <- AlsHistory1$DeltaSpine[i]
  }else if (AlsHistory1$SiteLimbAndBulbar[i] == 1)
  {
    AlsHistory1$SiteOfOnset[i] <- 5
    AlsHistory1$OnsetDelta[i] <- AlsHistory1$DeltaLimbAndBulbar[i]
  }
}
print(paste('The number with OnsetDelta:',length(which(AlsHistory1$OnsetDelta <0))))

##############################################################
for (i in (2:dim(x)[1]))
{
  variablename <- paste('Sym',x$Var1[i])
  variablename <- gsub("[: :]","",variablename)
  variablename2 <- paste('Delta',x$Var1[i])
  variablename2 <- gsub("[: :]","",variablename2)
  temp <- rep(0,n_id)
  temp2 <- rep(NA,n_id)
  row <- which(AlsHistory$Symptom == x$Var1[i])
  for (j in 1:length(row))
  {
    sub_id <- AlsHistory$subject_id[row[j]]
    sub_id <- which(AlsHistory1$subject_id == sub_id)
    temp[sub_id] <- 1
    temp2[sub_id] <- AlsHistory$Onset_Delta[row[j]]
  }
  AlsHistory1[variablename] <- temp
  AlsHistory1[variablename2] <- temp2
}
AlsHistory1$SymSum <- apply(AlsHistory1[c(15,17,19,21,23,25,27,29,31,33)],1,sum)

# to check the subjects with more than one sympton -> same onset delta
temp <- AlsHistory1[which(AlsHistory1$SymSum == 2),c(15,17,19,21,23,25,27,29,31,33)+1]
checknoNA <- function(x) {
  x[which(x<0)]}
temp <- apply(temp, 1, checknoNA)     

# make a column 'SymOnsetDelta'
checknoNA <- function(x) {
  idx <- which(x<0)
  x[idx[1]]}
AlsHistory1$SymOnsetDelta <- apply(AlsHistory1[,c(15,17,19,21,23,25,27,29,31,33)+1], 1, checknoNA)

AlsHistory2 <- data.frame(AlsHistory1[,c(1,2,4,14,15,17,19,21,23,25,27,29,31,33,36)])

write.csv(AlsHistory1,'/Users/ruoxunzi/Desktop/Machine_Learning/Project/CleanData/AlsHistory1_detailed.csv', row.names = FALSE)
write.csv(AlsHistory2,'/Users/ruoxunzi/Desktop/Machine_Learning/Project/CleanData/AlsHistory1.csv', row.names = FALSE)

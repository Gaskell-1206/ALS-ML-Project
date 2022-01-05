setwd('/Users/ruoxunzi/Desktop/Machine_Learning/Project/RawData/')
familyHistory <- read.csv('FamilyHistory.csv')

subject_id <- familyHistory$subject_id

x <- as.data.frame(table(familyHistory$Family_Hx_of_Neuro_Disease))
x <- x[order(x$Freq,decreasing = TRUE),]
Family_Hx_of_Neuro_Disease_ALS <- factor(familyHistory$Family_Hx_of_Neuro_Disease, labels = c(0,1))

x1 <- as.data.frame(table(familyHistory$Neurological_Disease))
x1 <- x1[order(x1$Freq,decreasing = TRUE),]
Neurological_Disease <- familyHistory$Neurological_Disease
Neurological_Disease_num <- factor(Neurological_Disease, labels = seq(0,9))

x2 <- as.data.frame(table(familyHistory$Neurological_Disease_Other))
x2 <- x2[order(x2$Freq,decreasing = TRUE),]

family <- familyHistory[,c(7:10,13:21,24,30,31,32,36:39)]
family[is.na(family)] <- 0
family_sum <- apply(family,1,sum)
family_member <- ifelse(family_sum>0, family_member <- 1, family_member <- 0)

familyHistory1 <- data.frame(subject_id,Family_Hx_of_Neuro_Disease_ALS,Neurological_Disease,
                             Neurological_Disease_num,family_member,family)

write.csv(familyHistory1,'/Users/ruoxunzi/Desktop/Machine_Learning/Project/CleanData/familyHistory1.csv', row.names = FALSE)


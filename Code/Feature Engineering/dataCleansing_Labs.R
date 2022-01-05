setwd('/Users/ruoxunzi/Desktop/Machine_Learning/Project/RawData/')
labs <- read.csv('Labs.csv')

labs <- labs[labs$Laboratory_Delta<92,]

subject_id <- unique(labs$subject_id)
n_id <- length(subject_id)
print(paste('unique subjects',n_id))
labs1 <- data.frame(subject_id)

# find the most used test
labs$Test_Name <- replace(labs$Test_Name, labs$Test_Name == 'Bilirubin (total)', 'Bilirubin (Total)')
x <- as.data.frame(table(labs$Test_Name))
x <- x[order(x$Freq,decreasing = TRUE),]
x$Var1 <- gsub("[:() :]","",x$Var1)
labs$Test_Name <- gsub("[:() :]","",labs$Test_Name)
test_names <- x$Var1[x$Freq>10000] #35 tests
n <- length(test_names)

# create a data frame for each test and a whole data frame containing all tests
num_tol <- integer(n)
num_uni <- integer(n)
for (i in 1:n)
{
  temp <- labs[labs$Test_Name==test_names[i],]
  #assign(test_names[i],temp)
  num_tol[i] = dim(temp)[1]
  sub_id <- unique(temp$subject_id)
  num_uni[i] = length(sub_id)
  
  # initialize the columns in each test data frame
  value_ave <- integer(num_uni[i])
  value_min <- integer(num_uni[i])
  value_max <- integer(num_uni[i])
  value_sd <- integer(num_uni[i])
  value_na <- integer(num_uni[i])
  test_num <- integer(num_uni[i])
  duration <- integer(num_uni[i])
  # initialize the columns in 'labs1' (the whole table)
  c1 <- rep(NA, n_id)
  c2 <- rep(NA, n_id)
  c3 <- rep(NA, n_id)
  c4 <- rep(NA, n_id)
  c6 <- rep(NA, n_id)
  c7 <- rep(NA, n_id)
  
  for (j in 1:num_uni[i])
  {
    values <- temp$Test_Result[temp$subject_id==sub_id[j]]
    values <- values[!is.na(values)]
    time <- temp$Laboratory_Delta[temp$subject_id==sub_id[j]]
    time <- values[!is.na(time)]
    values <- as.numeric(values)
    time <- as.numeric(time)
    value_ave[j] <- mean(values,na.rm=TRUE)
    value_min[j] <- min(values,na.rm=TRUE)
    value_max[j] <- max(values,na.rm=TRUE)
    value_sd[j] <- sd(values,na.rm=TRUE)
    test_num[j] <- length(values)
    duration[j] <- max(time)-min(time)
    
    # assign the values in the 'labs1' (the whole table)
    row <- which(subject_id == sub_id[j])
    c1[row] <- value_ave[j]
    c2[row] <- value_min[j]
    c3[row] <- value_max[j]
    c4[row] <- value_sd[j]
    c6[row] <- test_num[j]
    c7[row] <- duration[j]
  }
  temp <- data.frame(sub_id,value_ave,value_min,value_max,value_sd,test_num,duration)
  assign(test_names[i],temp)
  savepath <- paste('/Users/ruoxunzi/Desktop/Machine_Learning/Project/CleanData/labs/',test_names[i],'.csv')
  savepath <- gsub("[: :]","",savepath)
  write.csv(temp,savepath, row.names = TRUE)
  
  # rename the columns
  colname <- paste(test_names[i],'_ValueAve')
  colname <- gsub("[: :]","",colname)
  labs1[colname] <- c1
  colname <- paste(test_names[i],'_ValueMin')
  colname <- gsub("[: :]","",colname)
  labs1[colname] <- c2
  colname <- paste(test_names[i],'_ValueMax')
  colname <- gsub("[: :]","",colname)
  labs1[colname] <- c3
  colname <- paste(test_names[i],'_ValueSd')
  colname <- gsub("[: :]","",colname)
  labs1[colname] <- c4
  colname <- paste(test_names[i],'_TestNum')
  colname <- gsub("[: :]","",colname)
  labs1[colname] <- c6
  colname <- paste(test_names[i],'_Duration')
  colname <- gsub("[: :]","",colname)
  labs1[colname] <- c7
  
}
AveTestsPerSub <- num_tol/num_uni
test_sum <- data.frame(test_names,num_uni,num_tol,AveTestsPerSub)

write.csv(test_sum,'/Users/ruoxunzi/Desktop/Machine_Learning/Project/CleanData/labs_test_sum.csv', row.names = FALSE)
write.csv(labs1,'/Users/ruoxunzi/Desktop/Machine_Learning/Project/CleanData/labs1.csv', row.names = FALSE)

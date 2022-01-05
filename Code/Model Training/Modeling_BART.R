setwd('/Users/ruoxunzi/Desktop/Machine_Learning/Project/DataForModel/')
train <- read.csv('./median_imputer/training_impute_median_norm.csv')
test <- read.csv('./median_imputer/testing_impute_median_norm.csv')
val <- read.csv('./median_imputer/validation_impute_median_norm.csv')
#train <- read.csv('./knn_imputer/training_impute_knn_norm.csv')
#test <- read.csv('./knn_imputer/testing_impute_knn_norm.csv')
#val <- read.csv('./knn_imputer/validation_impute_knn_norm.csv')

library(BayesTree)

train_id <- train[,1]
X_train1 <- train[,3:327]
y_train1 <- train[,2]

test_id <- test[,1]
X_test1 <- test[,3:327]
y_test1 <- test[,2]

val_id <- val[,1]
X_val1 <- val[,3:327]
y_val1 <- val[,2]

X_pred <- rbind(X_test1,X_val1)

start_time = Sys.time()
y_pred <- bart(X_train1, y_train1, X_pred,
               ntree = 200, ndpost = 1000, nskip = 100)
end_time = Sys.time()
print(end_time - start_time)

y_pred1 <- data.frame(train_id,y_pred$yhat.train.mean,y_pred$y)
colnames(y_pred1) <- c('subject_id','y_pred_bart','y_true')
write.csv(y_pred1,'/Users/ruoxunzi/Desktop/Machine_Learning/Project/BART/train_pred.csv', row.names = FALSE)

y_predall <- y_pred$yhat.test.mean
y_pred_test <- y_predall[1:length(test_id)]
y_pred_test1 <- data.frame(test_id,y_pred_test,y_test1)
colnames(y_pred_test1) <- c('subject_id','y_pred_bart','y_true')
write.csv(y_pred_test1,'/Users/ruoxunzi/Desktop/Machine_Learning/Project/BART/test_pred.csv', row.names = FALSE)

y_pred_val <- tail(y_predall,length(val_id))
y_pred_val1 <- data.frame(val_id,y_pred_val,y_val1)
colnames(y_pred_val1) <- c('subject_id','y_pred_bart','y_true')
write.csv(y_pred_val1,'/Users/ruoxunzi/Desktop/Machine_Learning/Project/BART/val_pred.csv', row.names = FALSE)

#################
setwd('/Users/ruoxunzi/Desktop/Machine_Learning/Project/BART/')
train <- read.csv('./train_impute_knn_norm_pred.csv')
test <- read.csv('./test_impute_knn_norm_pred.csv')
val <- read.csv('./val_impute_knn_norm_pred.csv')

data <- rbind(train,test,val)
write.csv(data,'/Users/ruoxunzi/Desktop/Machine_Learning/Project/BART/bart_output.csv', row.names = FALSE)


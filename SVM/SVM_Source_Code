# CSV file I/O, e.g. the read_csv function
library(readr) 
library(e1071)
library(adabag)
#File Reading
voice <- read.csv("C:/Users/gfogl/Desktop/Study/Machinelearning/Project/voicegender/voice.csv")

#Scaling the data
voice[,21] <- as.numeric(voice[,21])
maxs <- apply(voice, 2, max)
mins <- apply(voice, 2, min)
voice <- as.data.frame(scale(voice, center = mins, scale = maxs - mins))
voice$label <- factor(voice$label)

set.seed(1892)

trainIndex <- sample(1:nrow(voice), 0.80 * nrow(voice))
train <- subset(voice[trainIndex,])
test <- subset(voice[-trainIndex, 1:20])
test_label <- voice[-trainIndex, 21]

#creating a basic model
svm_model <- svm(label ~ ., data=train)
summary(svm_model)

#calculating the accuracy
#Train accuracy
pred <- predict(svm_model,train[,-21])
table(pred,train[,21])
mean(pred == train[,21])*100

#Test accuracy
pred <- predict(svm_model,test)
table(pred,test_label)
mean(pred == test_label)*100

#now tuning the model with 10 cross-validation folds.
svm_tune <- tune(svm, train.x=train[,-21], train.y=train$label, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

#finding best parameters
print(svm_tune)
svm_tune$best.parameters$cost
svm_tune$best.parameters$gamma

#remaking the model with the best parameters
svm_model_after_tune <- svm(label ~ ., data=train, kernel="radial", cost=svm_tune$best.parameters$cost, gamma=svm_tune$best.parameters$gamma)
summary(svm_model_after_tune)

#re-calculating the accuracy
#Train accuracy
pred <- predict(svm_model_after_tune,train[,-21])
table(pred,train[,21])
mean(pred == train[,21])*100

#Test accuracy
pred <- predict(svm_model_after_tune,test)
table(pred,test_label)
mean(pred == test_label)*100
mat <- confusionMatrix(pred, test_label)

#ROC AND AUC
library(e1071)
svm_model_after_tune <- svm(label ~ ., data=train, kernel="radial", cost=svm_tune$best.parameters$cost, gamma=svm_tune$best.parameters$gamma, probability=TRUE)
pred <- predict(svm_model_after_tune, test, probability = TRUE)

svmPred <- prediction(attr(pred, "probabilities")[,2], test_label)
svmROC <- performance(svmPred,"tpr","fpr")
plot(svmROC)
svmAUC <- performance(svmPred,"auc")
svmAUC@y.values

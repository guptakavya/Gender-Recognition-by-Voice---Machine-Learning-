library(neuralnet)
voice <- read.csv("/Users/aditighamandi/Desktop/MLproject/voice.csv")
voice[,21] <- as.numeric(voice[,21])
maxs <- apply(voice, 2, max)
mins <- apply(voice, 2, min)
voice <- as.data.frame(scale(voice, center = mins, scale = maxs - mins))
voice$label <- factor(voice$label)
library(caret)
indextrain <- createDataPartition(y = voice$label,times = 1,p = 0.8,list=FALSE)
training <- voice[indextrain,]
testing <- voice[-indextrain,]
?train
train <- createFolds(training$label, k=10, returnTrain = FALSE )
nnetFit <- train(label ~ ., method = "nnet", data = voice,
                 tuneLength = 5,
                 trControl = trainControl(
                   method = "cv", indexOut = train))
nnetFit
nnetFit$finalModel
plot(nnetFit)
pred<-predict(nnetFit,newdata=testing)
confusionMatrix(pred, testing$label)

recall(pred, testing$label)
precision(pred, testing$label)


library(pROC)
nnetPredict <- predict(nnetFit,newdata = testing,type="raw")
nnetROC <- multiclass.roc(testing$label,as.numeric(nnetPredict))
nnetROC

voice <- read.csv("C:/Users/pawar/Downloads/voice.csv/voice.csv")

> voice[,21] <- as.numeric(voice[,21])
> maxs <- apply(voice, 2, max)
> mins <- apply(voice, 2, min)
> voice <- as.data.frame(scale(voice, center = mins, scale = maxs - mins))
> voice$label <- factor(voice$label)
> library(ISLR)
> library(caret)

> set.seed(300)
> indxTrain <- createDataPartition(y = voice$label,p = 0.75,list = FALSE)
> training <- voice[indxTrain,]
> testing <- voice[-indxTrain,]
> prop.table(table(training$label)) * 100
> prop.table(table(testing$label)) * 100
> prop.table(table(voice$label)) * 100
> set.seed(400)
> ctrl <- trainControl(method="repeatedcv",repeats = 3)
> knnFit <- train(label ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

> plot(knnFit)
> knnPredict <- predict(knnFit,newdata = testing )
> confusionMatrix(knnPredict, testing$label )

> library(pROC)
> knnPredict <- predict(knnFit,newdata = testing , type="prob")
> knnROC <- roc(testing$label,knnPredict[,"1"], levels = rev(testing$label))
> plot(knnROC)

#Code

RandomForestData=read.csv(file="C:/Users/sri21arun/Desktop/UTD/Machine Learning/Project/voice.csv")

library(adabag)

str(RandomForestData)
RandomForestDat=RandomForestData
RandomForestDat[,21]=as.factor(RandomForestDat[,21])



boost=boosting.cv(label~.,data=RandomForestDat,v=10,mfinal=100,control=rpart.control(maxdepth = 10),boos=FALSE)

sum(boost$confusion)
precision=sum(diag(boost$confusion))/(sum(diag(boost$confusion))+boost$confusion[3])
accuracy=(1-boost$error)*100


print(paste("Average Accuracy",accuracy))
print(paste("Average precision",precision*100))

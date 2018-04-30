##SVM Classifier R commands - by Nour and Clement

#install and load the caret package
install.packages("caret")
library(caret)

#import the data and creating training and testing sets
heart = read.table("heart.dat")

set.seed(3033)
intrain = createDataPartition(y = heart$V14, p= 0.8, list = FALSE)
traindata = heart[intrain,]
testdata = heart[-intrain,]

#making the label categorical in the training set
traindata[["V14"]] = factor(traindata[["V14"]])

#building the SVM classifier
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svmLinear = train(V14 ~., data = traindata, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

#try the classifier on the testing set and show results
testresult = predict(svmLinear,newdata=testdata)
table(testresult==testdata$V14)
confusionMatrix(testresult,testdata$V14)

#Trying to find the best cost parameter to improve accuracy
grid = expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svmLinearGrid = train(V14 ~., data = traindata, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
plot(svmLinearGrid)
confusionMatrix(predict(svmLinearGrid, newdata = testdata),testdata$V14)


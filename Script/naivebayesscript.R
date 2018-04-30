##Naive Bayes Classifier R commands - by Nour and Clement

#install and load the e1071 package
install.packages("e1071")
library(e1071)

#import the data and creating training and testing sets
heart = read.table("heart.dat")

set.seed(3033)
intrain = createDataPartition(y = heart$V14, p= 0.8, list = FALSE)
traindata = heart[intrain,]
testdata = heart[-intrain,]

#build the model and predict values in the testdata file

nbclass=naiveBayes(traindata,as.factor(traindata$V14))
thetest=predict(nbclass,testdata[1:13])

#compare the prediction results with the actual labels of testdata
table(thetest==testdata$V14)

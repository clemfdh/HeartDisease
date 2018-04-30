##KNN Classifier R commands - by Nour and Clement

#install and load the class package
install.packages("class")
install.packages("BBmisc")
library(class)
library(BBmisc)


#import the data and change the column headers' names
heart = read.table("heart.dat", header=TRUE)
colnames(heart) = c('age','sex','chestpain','bloodpressure','serumcholest','bloodsugar','ecg','maxheartrate','angina','oldpeak','exerciceslope','vessels','thal','label')


#Preprocessing: normalization.
normheart=normalize(heart, method='range')

#Making train and test data variables, and the label vector
traindata = normheart[1:216,]
testdata = tail(normheart,54)
labelvec = traindata$label

##Building the classifiers
knnmodel1=knn(train = traindata[1:13],test = testdata[1:13],labelvec,1)
knnmodel3=knn(train = traindata[1:13],test = testdata[1:13],labelvec,3)
knnmodel5=knn(train = traindata[1:13],test = testdata[1:13],labelvec,5)
knnmodel7=knn(train = traindata[1:13],test = testdata[1:13],labelvec,7)

##Comparing results with test data
table(testdata$label==knnmodel1)
table(testdata$label==knnmodel3)
table(testdata$label==knnmodel5)
table(testdata$label==knnmodel7)


##Doing the same thing with cross validation evaluation
knnmodel1=knn.cv(normheart[1:13],normheart$label,1)
knnmodel3=knn.cv(normheart[1:13],normheart$label,3)
knnmodel5=knn.cv(normheart[1:13],normheart$label,5)
knnmodel7=knn.cv(normheart[1:13],normheart$label,7)
table(normheart$label==knnmodel1)
table(normheart$label==knnmodel3)
table(normheart$label==knnmodel5)
table(normheart$label==knnmodel7)

##And now, with feature selection and cross validation evaluation:

knnmodel1=knn.cv(normheart[,c(3,5,7,8,9,10,12,13)],normheart$label,1)
knnmodel3=knn.cv(normheart[,c(3,5,7,8,9,10,12,13)],normheart$label,3)
knnmodel5=knn.cv(normheart[,c(3,5,7,8,9,10,12,13)],normheart$label,5)
knnmodel7=knn.cv(normheart[,c(3,5,7,8,9,10,12,13)],normheart$label,7)
table(normheart$label==knnmodel1)
table(normheart$label==knnmodel3)
table(normheart$label==knnmodel5)
table(normheart$label==knnmodel7)


##Classifying again after feature selection, features kept are 3,5,7,8,9,10,12,13
knnmodel1=knn(train = traindata[,c(3,5,7,8,9,10,12,13)],test = testdata[,c(3,5,7,8,9,10,12,13)],labelvec,1)
knnmodel3=knn(train = traindata[,c(3,5,7,8,9,10,12,13)],test = testdata[,c(3,5,7,8,9,10,12,13)],labelvec,3)
knnmodel5=knn(train = traindata[,c(3,5,7,8,9,10,12,13)],test = testdata[,c(3,5,7,8,9,10,12,13)],labelvec,5)
knnmodel7=knn(train = traindata[,c(3,5,7,8,9,10,12,13)],test = testdata[,c(3,5,7,8,9,10,12,13)],labelvec,7)
table(testdata$label==knnmodel1)
table(testdata$label==knnmodel3)
table(testdata$label==knnmodel5)
table(testdata$label==knnmodel7)

##And now, with feature selection and cross validation evaluation:

knnmodel1=knn.cv(normheart[,c(3,5,7,8,9,10,12,13)],normheart$label,1)
knnmodel3=knn.cv(normheart[,c(3,5,7,8,9,10,12,13)],normheart$label,3)
knnmodel5=knn.cv(normheart[,c(3,5,7,8,9,10,12,13)],normheart$label,5)
knnmodel7=knn.cv(normheart[,c(3,5,7,8,9,10,12,13)],normheart$label,7)
table(normheart$label==knnmodel1)
table(normheart$label==knnmodel3)
table(normheart$label==knnmodel5)
table(normheart$label==knnmodel7)


library(rpart)
library(caret)
library(rpart.plot)
library(ROCR)
library(psych)
library(plyr)

#decision treee
library(tree)

data1 = read.csv("heart.csv", header = FALSE, sep=" ")
data1 = data.frame(data1)
names(data1) <- c("age", "sex","chest_pain","resting_blood_pressure","serum_cholesterol","fasting_blood_sugar",
                  "resting_electrocardiographic","maximum_heart_rate","exercise_induced_angina","oldpeak","ST_segment_slope"
                  ,"number_of_major_vessels","thal","disease")


#data slicing

data1$disease <- as.factor(ifelse(data1$disease == 2, "presence", "absence"))

set.seed(120)
intrain <- createDataPartition(y = data1$disease, p= 0.7, list = FALSE)
training <- data1[intrain,]
testing <- data1[-intrain,]
dim(training); dim(testing)

#so we can compute their occurence
training = data.frame(training)
testing = data.frame(testing)
count(training,'disease')


#missing values ? 
anyNA(data1)

#summary
summary(data1)

#building the model1


model <- rpart(disease ~  age + sex +chest_pain+resting_blood_pressure+serum_cholesterol+fasting_blood_sugar+resting_electrocardiographic+maximum_heart_rate+exercise_induced_angina+oldpeak+ST_segment_slope+number_of_major_vessels+thal, method="class", data=training)
rpart.plot(model, extra=104, fallen.leaves = T, type=4)


##prediction
prediction <- predict(model, testing, type = "class")
prediction <- ifelse(prediction == testing$disease ,"Yes","No")
count <- count(prediction)
misClasificError <- (count$freq[2]/length(testing$disease))
print(paste('Accuracy',misClasificError*100,'%'))




#Confusion matrix - prediction
pred <- predict(fit1, testing, type="class")

expected <- as.factor(c(testing$disease))
predicted <- as.factor(c(pred))

cm <- confusionMatrix(data=predicted, reference=expected)
fourfoldplot(cm$table,main = "Confusion Matrix for Model")
length(testing$disease)



##new model

fit1 <- rpart(disease ~  age + sex +chest_pain+resting_blood_pressure+serum_cholesterol+fasting_blood_sugar+resting_electrocardiographic+maximum_heart_rate+exercise_induced_angina+oldpeak+ST_segment_slope+number_of_major_vessels+thal,
               data=training,
               method="class", 
               control=rpart.control(minsplit=2, cp=0))
rpart.plot(fit1, extra=104, fallen.leaves = T, type=4)


##plotmo
library(plotmo)
plotmo(model)

##using tree library
set.seed(2)
tree.heart <- tree(disease~ . ,training)
summary(tree.heart)
plot(tree.heart)
text(tree.heart,pretty = 0)

#prediciton 
tree.pred <- predict(tree.heart,testing , type='class' )
table(tree.pred,testing$disease)

#model selection
cv.tree.heart <- cv.tree(tree.heart, FUN = prune.misclass)
names(cv.tree.heart)

#plot to select our model
par(mfrow=c(1,2))
plot(cv.tree.heart$size,cv.tree.heart$dev, type='b')
plot(cv.tree.heart$k,cv.tree.heart$dev, type='b')

#prune the tree to 9 nodes
prun.tree.heart <- prune.misclass(tree.heart, best=6)
plot(prun.tree.heart)
text(prun.tree.heart, pretty=0)
#accuracy
tree.pred <- predict(prun.tree.heart,testing , type='class' )
table(tree.pred,testing$disease)

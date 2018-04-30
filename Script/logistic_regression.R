library(ggplot2)
library(ROCR)
library(survey)


#Import the data
data1 = read.csv("heart.csv", header = FALSE, sep=" ")
names(data1) <- c("age", "sex","chest_pain","resting_blood_pressure","serum_cholesterol","fasting_blood_sugar",
                  "resting_electrocardiographic","maximum_heart_rate","exercise_induced_angina","oldpeak","ST_segment_slope"
                  ,"number_of_major_vessels","thal","disease")



#data comparison
plot(data1$maximum_heart_rate,data1$oldpeak,col = c("red", "blue")[as.numeric(data1$disease)])

##changing disease to yes and no=Nominal
data1$disease <- as.factor(ifelse(data1$disease == 2, "Yes", "No"))

##splitting the data 80/100
##making a hold out validation 
set.seed(300)
ind <- sample(2, nrow(data1), replace=TRUE, prob=c(0.8, 0.2))
train <- data1[ind==1,]
test <- data1[ind==2,]

#display correlation and plot it
pairs.panels(train)


##cleaning the data, making sure there is no missing values
anyNA(data1)
#occurence of variables
sapply(train,function(x) sum(is.na(x)))
sapply(train, function(x) length(unique(x)))
#missing values
library(Amelia)
missmap(train, main = "Missing values vs observed")

#wald test for age
regTermTest(model1,'age')

##building the model
model1 <- glm(disease ~.,family=binomial(link='logit'),data=train)
summary(model1)
aa<-AIC(model2)
bb<-BIC(model2)

#remove age
sub1 <- train[c(-1)]
model2 <- glm(disease ~.,family=binomial(link='logit'),data=sub1)
summary(model2)
a<-AIC(model2)
b<-BIC(model2)

#remove fasting blood
sub2 <- train[c(-1,-6)]
model3 <- glm(disease ~.,family=binomial(link='logit'),data=sub2)
summary(model3)
c<-AIC(model3)
d<-BIC(model3)

# remove resting blood presure
sub3 <- train[c(-1,-6,-4)]
model4 <- glm(disease ~.,family=binomial(link='logit'),data=sub3)
summary(model4)
e<-AIC(model4)
f<-BIC(model4)


#remove ST_segment slope
sub4 <- train[c(-1,-6,-4,-11)]
model5 <- glm(disease ~.,family=binomial(link='logit'),data=sub4)
summary(model5)
g<-AIC(model5)
h<-BIC(model5)

#remove exercise induced angina
sub5 <- train[c(-1,-6,-4,-11,-9)]
model6 <- glm(disease ~.,family=binomial(link='logit'),data=sub5)
summary(model6)
i<-AIC(model6)
j<-BIC(model6)

## anova on model 6
anova(model6, test="Chisq")

#plotting AIB BIC
aic <- c(aa,a,c,e,g,i)
bic <- c(bb,b,d,f,h,j)

plot(aic,bic, pch=19,col='blue',main="AIC BIC analysis")

##prediction

fitted.results <- predict(model6, newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.8,"No","Yes")
misClasificError <- mean(fitted.results == test$disease)
print(paste('Accuracy',1-misClasificError))

fitted.results <- predict(model1, newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,"No","Yes")
misClasificError <- mean(fitted.results != test$disease)
print(paste('Accuracy',1-misClasificError))


p <- predict(model6, test, type="response")
pr <- prediction(p, test$disease)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,col="blue")

auc <- performance(pr, measure = "auc")
auc <- auc.values[[1]]


##Anova test"
anova(model6,test = 'Chisq')

##Residual analysis
plot(model6,col='blue')















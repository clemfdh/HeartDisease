
## first hypothesis testing

#import data
data1 = read.csv("heart.csv", header = FALSE, sep=" ")
names(data1) <- c("age", "sex","chest_pain","resting_blood_pressure","serum_cholesterol","fasting_blood_sugar",
                  "resting_electrocardiographic","maximum_heart_rate","exercise_induced_angina","oldpeak","ST_segment_slope"
                  ,"number_of_major_vessels","thal","disease")

#basic stats
basicStats(data1)

#make a subset, we want just the sick individuals
data_subset <- data1[data1$disease == 2,]
data_subset <- data.frame(data_subset$age, data_subset$disease )

#basic stats
basicStats(data_subset)
mean(data_subset$data_subset.age)

#test statistic
xbar =  56.59167          # sample mean 
mu0 = 65            # hypothesized value 
sigma = 8.116273          # population standard deviation 
n = 120                 # sample size 
z = (xbar-mu0)/(sigma/sqrt(n))
pval = pnorm(z)

#Confidence Interval
error <- 1.96*(sigma/sqrt(n))
left <- xbar - error
right <- xbar + error






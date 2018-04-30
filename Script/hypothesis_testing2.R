#Second Hypothesis test

#We have already import the data

#make the subset 2
data_subset2 <- data1[data1$disease == 1,]
data_subset2 <- data.frame(data_subset2$serum_cholesterol, data_subset2$disease )
mean(data_subset2$data_subset2.serum_cholesterol)

basicStats(data_subset2)

#t stattistic
xbar =  244.213333          # sample mean 
mu0 = 250            # hypothesized value 
sigma =   54.019085        # population standard deviation 
n = 150               # sample size 
z = (xbar-mu0)/(sigma/sqrt(n))

#P_Value
pval = pnorm(z)

#Confidence Interval
error <- 1.96*(sigma/sqrt(n))
left <- xbar - error
right <- xbar + error


########### ILLUSTRATION OF THE CENTRAL LIMIT THEOREM #############
########### Author: Ana Arribas Gil #####################

#Load the data (this represents an underlying distribution F1)

load("population1.RData") #loads a vector named F1
load("population2.RData") #loads a vector named F2
load("population3.RData") #loads a vector named F3

dist <- F1     #select the underlying distribution, it can also be F2, F3
dist_id <- 1   #id number, it can also be 2, 3

X11()
par(mfrow=c(3,3)) #Divide the graphical window in 3 rows and 3 columns

#Represent the distribution
mean_F=mean(dist)
var_F=var(dist)
hist(dist, main=paste("Population", dist_id),freq=FALSE, xlab="x", ylab="Density", col=8)
rangeF=range(dist) # range of values
p1 <- 0.1*diff(rangeF)
p2 <- 0.9*diff(rangeF)
mtext(paste("Mean=",round(mean_F,2)), side=1, line=2, at=p1,cex=0.6)
mtext(paste("Var=",round(var_F,2)), side=1, line=2, at=p2,cex=0.6)

#Illustrate the central limit theorem by taking random samples from F1 and considering
#their averages. What is the distribution of the sample mean?

sample_sizes=c(5,10,15,20,25,30,35,40)

for (i in 1:length(sample_sizes)) {  #loop over different sample sizes
  
  n <- sample_sizes[i]   #set the sample size
  
  sample_means<-numeric(length=200)  #vector with the sample means 
  for (j in 1:200) {  #200: number of samples (repetitions)
    
    xn <- sample(dist,n)  #j-the random sample
    
    sample_means[j] <- mean(xn)  #j-th sample mean
  }
  
  #Represent the distribution of the sample mean of a random sample of size n=sample_sizes[i]
  mean_sm=mean(sample_means)
  var_sm=var(sample_means)
  hist(sample_means, main=paste("Sample mean, n=",n),freq=FALSE, xlab="x", ylab="Density", col=4, xlim=rangeF)
  mtext(paste("Mean=",round(mean_sm,2)), side=1, line=2, at=p1,cex=0.6)
  mtext(paste("Var=",round(var_sm,2)), side=1, line=2, at=p2,cex=0.6) 
  
  #Superpose the pdf of a normal distribution with the given mean and variance
  x <- seq(0,110,1000) #1000 equally distant points in [0,110]
  y <- dnorm(x,mean = mean_sm, sd = sqrt(var_sm)) #pdf of a normal distribution with mean mean_sm and variance var_sm, evaluated at x
  lines(x, y , col=2)
  
}

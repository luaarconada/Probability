#### EXERCISE Elevator    ######
ite <- 10000  # number of repetitions of the random experiment
count <- numeric(length=ite)  # vector to save event ocurrences

for (i in 1:ite){
  floors <- sample(1:10, 5, replace=T)  # vector of length 5: 
  # i-th component: floor at which 
  # the i-th person leaves the elevator
  
  # Event A: no one gets out at the first three floors, i.e. floors_i > 3 for all i 
  count[i] <- (sum(floors > 3)==5) #1: A occurred, 0: A didn't occur
  # Event B: three people get out at the 6th floor
  #count[i] <- (sum(floors == 6)==3) #1: B occurred, 0: B didn't occur
  # Event C: they all get out at different floors
  #count[i] <- length(unique(floors))==5 #1: C occurred, 0: C didn't occur
  # Event D: everybody gets out at the same floor
  #count[i] <- length(unique(floors))==1 #1: D occurred, 0: D didn't occur
  
}
# Probability of A (nominal/theoretical value)
p=7^5/10^5
# Probability of B (nominal/theoretical value)
#p=10*9^2/10^5
# Probability of C (nominal/theoretical value)
#p=10*9*8*7*6/10^5
# Probability of D (nominal/theoretical value)
#p=10/10^5

# Plot relative frequencies of the event: cumsum takes cummulative sums over vector components
X11()
plot(1:ite, cumsum(count)/(1:ite), ylim=c(0,1), type="l", main="Probability of no one getting out in the 1st 3 floors", xlab="nb. of repetitions", ylab="Relative frequency")
abline(h=p, col="red")  # horizontal line with the nominal value



### EXERCISE  car license plate #####

letters <- c("B","C","D","F","G","H","K","L","M","N","P","R","S","T","V","W","X","Y")
numbers <- 0:9

n <- 100000
counter <- c()

for (i in 1:n){
  plate_numbers  <- sample(numbers,4,replace=TRUE)
  plate_letters  <- c(sample(letters[1:9],1), sample(letters,2,replace=TRUE))
  
  if(length(unique(plate_numbers))==1 | length(unique(plate_letters))==1 ) {
    counter <- c(counter,1)
  }else{
    counter <- c(counter,0)
  }
}

prob <- sum(counter)/n

# Probability value is 0.0035

plot(cumsum(counter)/(1:n),
     type="l",ylab="probability",xlab="repetitions") 
abline(h=0.0035,col="red")
     

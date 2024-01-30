### Convergence in Probability, but not almost surely:
########### Author: Ana Arribas Gil #####################

par(mfrow=c(2,1)) #Divide the graphical window in 2 rows and 1 column

#Simulate X_n for n=1 to n=1000
N <- 1000
x <- rbinom(rep(1,N),size=1,prob=1/(1:N))


X11()
plot(x,type="l", xlab="n", ylab=expression("x"[n]))

for(i in 1:10){ # Simulate and represent 10 additional realizations of X_n
  xi <- rbinom(rep(1,N),size=1,prob=1/(1:N))
  x <- rbind(x,xi) # We append the new realization to the previous ones
  points(xi,type="l",col=i)
  readline(prompt = "Press [enter] to continue.")
}


for(i in 1:100){ # Simulate and represent 100 additional realizations of X_n
  xi <- rbinom(rep(1,N),size=1,prob=1/(1:N))
  x <- rbind(x,xi) # We append the new realization to the previous ones
  points(xi,type="l",col=i)
}

# Probability |X_n - 0| > 0 as a function of n. The limit as n grows is 0 (which implies convergence in probability)

p <- apply(x, 2, function(y){ sum(y>0)}) / nrow(x)
plot(p,type="l", xlab="n", ylab=expression(paste("P(|X"[n],"-0| > 0)")))

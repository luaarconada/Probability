#### EXERCISE SET 2

#Exercise 7

nr=1000
X=rhyper(nr,61,89,10)
X11();plot(ecdf(X),main="Empirical cdf from 1000 simulated samples")
lines(0:10,phyper(0:10,61,89,10),col="blue",type="s")



#creat the two distribution you want to compare
normal.dist<-rnorm(10000)
beta.dist<-rbeta(10000, shape1=1, shape2=2)

#look at your distributions
hist(normal.dist, probability=T)
lines(density(normal.dist, bw=1), col="red")
hist(beta.dist, probability=T)


#compare the distributions using a simple scatter plot
plot(sort(normal.dist), sort(beta.dist))

#compare to a scatter plot of same distributions
plot(sort(normal.dist), sort(rnorm(10000,mean=5)))



qqplot(normal.dist, beta.dist)
qqline(beta.dist)
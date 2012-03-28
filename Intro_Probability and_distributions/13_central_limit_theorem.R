#you can use clt.examp from the TeachingDemos package
library(TeachingDemos)
clt.examp()
clt.examp(10)


#create your population with an odd distribution that would cause statistical nightmares
population<-rgeom(1000000,0.1)

#eg: that starts pushing the envelope (a distribution with infinit variance)
#population<-rcauchy(1000000)

#plot a histogram of the population
hist(population)

#sample from your population
sample.size<-100
sample1<-sample(population, size=sample.size)

#plot a histogram of you sample
hist(sample1)
#not sample histogram should look alot like that of the parent population and more so with larger sample size as seen in the law of large numbers


#calculate the mean for many sample
sample.means<-replicate(100, mean(sample(population, sample.size)))

#plot a histogram of the means
hist(sample.means, xlim=c(min(sample.means),max(sample.means)), probability=T, nclass=max(sample.means)-min(sample.means)+1)
lines(density(sample.means, bw=1), col="red")
abline(b=1)

#compare to a normal distribution
qqnorm(sample.means)
qqline(sample.means, col = 2)

#try this with different distributions, sample sizes and with sum instead o mean
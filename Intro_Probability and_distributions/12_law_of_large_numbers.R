
par(new=F)
#create your population
population<-rnorm(100000,50,10)

#plot a histogram of the population
hist(population)

#sample from your population
sample.size<-10
sample1<-sample(population, size=sample.size)

#add the population mean and samples mean
abline(v=mean(population), lwd=3, col="blue")
abline(v=mean(sample1), lwd=3, col="red")

#run this first part many times

#try this with varying population size
for(i in 1:100){
	sample.size<-i
	#calculate the mean for many sample
	sample.means<-replicate(100, mean(sample(population, 	sample.size)))
	#plot the means
	plot(x=rep(sample.size, length(sample.means)), y=sample.means, xlim=c(1,100), ylim=c(min(population),max(population)))
	par(new=T)
	
}


abline(h=mean(population), col="red", lwd=3)
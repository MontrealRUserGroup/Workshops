Steps

1. create the distriubtion plot in Rcmdr
2. copy the scrip from Rcmdr's script window to a new script
3. create a for loop enclosing the code in the new script (if you want to change multiple parameters, ad nested for loops)
4. edit Rcmdr's code to work with the for loop
	a. change the parameters using you indexed for loop value
	b. change plot parameters to continue plotting on the same plot :  par(new=T)
	d. make sure the axis are the same for each additional graph on the same plot (xlim=c(-3,3), ylim=)
	d. create coding and a legend (using line type lty, color col, line width lwd...)
	
5. Repead the process for each type of distribution that intersts you or copy paste the code you have written and edit it for a new distribution

6. Open a PDF file at the start of your script using PDF() and close the PDF file with dev.off()


Here is an example:


mean.i<-c(-1,0,1/2,1)
sd.j<-c(1/2,1,2)
mean.col<-rainbow(length(mean.i))
sd.lwd<-1:length(sd.j)


par(new=F)
for(i in 1:length(mean.i)){
	for(j in 1:length(sd.j)){

.x <- seq(-3.291, 3.291, length.out=100)
plot(.x, dnorm(.x, mean= mean.i[i], sd=sd.j[j]), xlab="x", ylab="Density", 
  main="Normal Distribution", type="l", xlim=c(-3,3), ylim=c(0,0.8), col= mean.col[i], lwd=sd.lwd[j])
abline(h=0, col="gray")
par(new=T)
remove(.x)

}
}

legend(-3,0.8, legend=mean.i, fill=mean.col, title="mean")
legend(-3,0.6, legend=sd.j, lwd=sd.lwd, title="sd")
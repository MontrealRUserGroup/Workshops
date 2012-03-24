##############################################
### Monte Carlo Simulation estimation of pi ##
## Author: Corey Chivers                    ##
##############################################

rm(list=ls())

options(digits=4)

## initialize ##
N=500 # Number of MC points
points <- data.frame(x=numeric(N),y=numeric(N))
pi_est <- numeric(N)
inner <-0
outer <-1

## BUILD Circle ##
circle <- data.frame(x=1:360,y=1:360)

for(i in 1:360)
{
  circle$x[i] <-0.5+cos(i/180*pi)*0.5
  circle$y[i] <-0.5+sin(i/180*pi)*0.5
}


## SIMULATE ##
pdf('MCpiT.pdf')

layout(matrix(c(2,3,1,1), 2, 2, byrow = TRUE))
for(i in 1:N)
{

  # Draw a new point at random
  points$x[i] <-runif(1)
  points$y[i] <-runif(1)

  # Check if the point is inside
  # the circle
  if( (points$x[i]-0.5)^2 + (points$y[i]-0.5)^2 > 0.25 )
  {
    outer=outer+1
  }else
  {
    inner=inner+1
  }

  current_pi<-(inner/(outer+inner))/(0.25)
  pi_est[i]= current_pi
  print(current_pi)

  par(mar = c(5, 4, 4, 2),pty='m')
  plot(pi_est[1:i],type='l',
    main=i,col="blue",ylim=c(0,5),
    lwd=2,xlab="# of points drawn",ylab="estimate")
  # Draw true pi for reference
  abline(pi,0,col="red",lwd=2) 

  par(mar = c(1, 4, 4, 1),pty='s')
  plot(points$x[1:i],points$y[1:i],
    col="red",
    main=c('Estimate of pi: ',formatC(current_pi, digits=4, format="g", flag="#")),
    cex=0.5,pch=19,ylab='',xlab='',xlim=c(0,1),ylim=c(0,1))
  lines(circle$x,circle$y,lw=4,col="blue")
  frame() #blank

}
dev.off()



##############################################
##############################################


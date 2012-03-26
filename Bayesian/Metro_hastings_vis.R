################################################################
###     Metropolis-Hastings Visualization     		   #########
###      Created by Corey Chivers, 2012                #########
################################################################


mh_vis<-function(prop_sd=0.1,target_mu=0,
    target_sd=1,seed=1,iter=5000,plot_file='MH.pdf')
{
    plot_range=c(target_mu-3*target_sd,target_mu+3*target_sd)
    track <- NULL
    k_X = seed; ## set k_X to the seed position

    pdf(plot_file)
    par(mfrow=c(3,1),mgp = c(1, 0.5, 0))

    for(i in 1:iter)
    {
      track<-c(track,k_X)    ## The chain
      k_Y = rnorm(1,k_X,prop_sd) ## Candidate point

      ## -- plot kernal density estimation of the Chain
      par(mar=c(0,3,2,3),xaxt='n',yaxt='n')
      
      curve(dnorm(x,target_mu,target_sd),col='blue',lty=2,lwd=2,xlim=plot_range)
      if(i > 2)
      lines(density(track,adjust=1.5),col='red',lwd=2)

      ## -- plot the chain
      par(mar=c(0,3,0,3))
      plot(track,1:i,xlim=plot_range,main='',type='l',ylab='Trace')

      pi_Y = dnorm(k_Y,target_mu,target_sd,log=TRUE)
      pi_X = dnorm(k_X,target_mu,target_sd,log=TRUE)

      ## -- plot the target distribution and propsal distribution actions
      par(mar=c(3,3,0,3),xaxt='s')
      curve(dnorm(x,target_mu,target_sd),xlim=plot_range,col='blue',lty=2,ylab='Metropolis-Hastings',lwd=2)
      curve(dnorm(x,k_X,prop_sd),col='black',add=TRUE)
      abline(v=k_X,lwd=2)
      points(k_Y,0,pch=19,cex=2)
      abline(v=k_Y)

      a_X_Y = (pi_Y)-(pi_X)
      a_X_Y = a_X_Y

      if (a_X_Y > 0)
        a_X_Y = 0

      ## Accept move with a probability a_X_Y
      if (log(runif(1))<=a_X_Y)  
      {
        k_X = k_Y;
        points(k_Y,0,pch=19,col='green',cex=2)
        abline( v=k_X,col='black',lwd=2)
      }
        
      ## Adapt the poposal
      if(i>100)
        prop_sd=sd(track[floor(i/2):i])

      if(i%%100==0)
          print(paste(i,'of',iter))
    }

    dev.off()
}


mh_vis()


########################################################################
## Montreal R User Group Workshop, 2012
## Special Topics: Bayesian Methods
## Designed and Facilitated by Corey Chivers
## Department of Biology, McGill University
########################################################################


##@  0.1 @##

rm(list=ls())			                   ## Housekeeping

#install.packages('MHadaptive')             ## This package will do the MCMC heavy lifting
#library(MHadaptive)
# If you are running an older version of R ( < 2.14 ), run the following line
# to load the functions we will need:

 source('http://madere.biol.mcgill.ca/cchivers/MHadaptive_source.R')

# You won't have any man pages if you use this option, 
# so you should also download the pdf manual here:
# http://cran.r-project.org/web/packages/MHadaptive/MHadaptive.pdf

###  --  ###




########################################################################
################# The Ecologist's Quarter Redux ########################
########################################################################


##@  1.1 @##

ecol_quarter<-c('T','H')	# Define a quarter
theta<-0.6			        # theta parameter describing the quarter's tendency to land caribou side up 
n=10			            # number of flips

#Simulate n flips of the ecologists' quarter
flips<-sample(ecol_quarter,n,p=c(theta,1-theta),replace=TRUE) 

flips 			            # Show the simulated flips
outcome<-paste(length(flips[flips=='T']),' out of ',n,' flips landed tails',sep='') # Out of the n flips, how many tails?
outcome

###  --  ###



##@  1.2  @##

## The function returns  log( P(D|theta)P(theta) )
log_likelihood_ecol_quarter<-function(q_theta)
{
  likelihood=0
  for(f in 1:length(flips)) # Take the product across all flips
  {
    if(flips[f] == 'T')
      likelihood<-likelihood+log(q_theta[1])
    if(flips[f] == 'H')
      likelihood<-likelihood+log(1-q_theta[1])
  }
  prior<-dunif(q_theta[1],0,1,log=TRUE) # Use a uniform prior (ie we believe that any theta on the interval 0 to 1 are equally likely a priori)
  return(likelihood + prior)
}
    
###  --  ###


##@  1.3  @##

## Run MCMC on the ecologist's quarter flips.
mcmc<-Metro_Hastings(li_func=log_likelihood_ecol_quarter,pars=c(0.5),par_names='theta',iterations=10000)

## Plot the results
plotMH(mcmc)
## Print the Bayesian Credible Intervals
BCI(mcmc)

###  --  ###

########################################################################
########################################################################







########################################################################
##################### Regression done Bayesian #########################
########################################################################

##@  2.1  @##

salmon<-read.csv('salmon_dat.csv')
head(salmon)
x11()
plot(salmon$body_length, salmon$egg_mass,xlab='Body Length (cm)', ylab='Egg Mass (mg)',pch=16)

###  --  ###



##@  2.2  @##

## Define the Bayesian linear regression model
li_salmon<-function(pars)
{
    a<-pars[1]      #intercept
    b<-pars[2]      #slope
    tau<-pars[3]    #error (precision)
    mean_egg_mass<- a + b * salmon$body_length
    log_likelihood<-sum( dnorm(salmon$egg_mass,mean_egg_mass, tau, log=TRUE) )
    prior<- prior_salmon(pars)
    return(log_likelihood + prior)
}

###  --  ###



##@  2.3  @##

## Define the Prior distributions
prior_salmon<-function(pars)
{
    a<-pars[1]      #intercept
    b<-pars[2]      #slope
    tau<-pars[3]    #error

    prior_a<-dnorm(a,0,100,log=TRUE)             ## Here, I have used very non-informative (flat) priors on all 
    prior_b<-dnorm(b,0,100,log=TRUE)             ## parameters.  How would you change these if you had some prior information
    prior_tau<-dgamma(tau,1,1/100,log=TRUE)      ## about one or more of the parameters?

    return(prior_a + prior_b + prior_tau)
}

###  --  ###



##@  2.4  @##

## Run Metropolis-Hastings MCMC on the regression model
mcmc_salmon<-Metro_Hastings(li_func=li_salmon,pars=c(0,0,3),par_names=c('a','b','tau'),iterations=10000,burn_in=2000)

###  --  ###



##@  2.5  @##

## Plot the MCMC output
plotMH(mcmc_salmon)
## Print the Bayesian Credible Intervals
BCI(mcmc_salmon)

###  --  ###


########################################################################
########################################################################







########################################################################
################### Prediction and Uncertainty #########################
########################################################################


##@  3.1  @##

## Function to compute predictions from the posterior
## distribution of the salmon regression model
predict_eggmass<-function(pars,length)
{
    a<-pars[,1]      #intercept
    b<-pars[,2]      #slope
    tau<-pars[,3]    #error    
    pred_mass<- a + b * length 
    pred_mass<- rnorm(length(a),pred_mass,tau)
    return(pred_mass)
}

###  --  ###



##@  3.2  @##

## generate prediction
pred_length<-80     # predict for an 80cm individual
pred<-predict_eggmass(mcmc_salmon$trace,length=pred_length)
## Plot prediction distribution
x11()
hist(pred,breaks=30,main='',probability=TRUE)

###  --  ###



##@  3.3  @##

## Ask some questions...

## What is the 95% BCI of the prediction?
quantile(pred,p=c(0.025,0.975))

## What is the probability that this individual will produce
## eggs which are smaller than 35mg?
length(which(pred < 35)) / length(pred)

###  --  ###


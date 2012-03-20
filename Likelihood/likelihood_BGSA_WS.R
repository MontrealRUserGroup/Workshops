########################################################################
## Montreal R User Group Workshop
## Likelihood Methods, 2012
## Designed and Facilitated by Corey Chivers
## Department of Biology, McGill University
########################################################################


##@  0.1 @##
rm(list=ls())			    # Housekeeping

#install.packages('emdbook')	# Bolker's data
library(emdbook)		    

source('http://madere.biol.mcgill.ca/cchivers/ccMLE.R')       
                        # Provides functions for
                        # approximate CI and likelihood
                        # surface plotting
###  --  ###




########################################################################
################### The Ecologist's Quarter ############################
########################################################################

##@  1.1 @##
## This function calculates the likelihood of theta, 
## for a given series of flips(x): L(theta | x)
likelihood_ecol_quarter<-function(q_theta,q_flips)
{
  likelihood=1
  # Take the product across all flips
  for(f in 1:length(q_flips)) 
  {
    if(q_flips[f] == 'T')
      likelihood<-likelihood*(q_theta)
    if(q_flips[f] == 'H')
      likelihood<-likelihood*(1-q_theta)
  }
  return(likelihood)
}
###  --  ###



##@  1.2  @##
ecol_quarter<-c('T','H')	# Define a quarter
theta<-0.6			# theta parameter describing the quarter's tendency to land caribou side up 
n=10				# number of flips

#Simulate n flips of the ecologists' quarter
flips<-sample(ecol_quarter,n,p=c(theta,1-theta),replace=TRUE) 

flips 			# Show the simulated flips

# Out of the n flips, how many tails?
outcome<-paste(length(flips[flips=='T']),' out of ',n,' flips landed tails',sep='') 

# Plot the likelihood function, given the observed (simulated) flips
curve(likelihood_ecol_quarter(x,q_flips=flips),
    xlab=expression(theta),
    ylab='Likelihood',
    xlim=c(0,1),
    main=outcome)	
###  --  ###



##@  1.3  @##
## Same as the first function, but returns the log(likelihood)
log_likelihood_ecol_quarter<-function(q_theta,q_flips)
{
  ll=0
  if(q_theta > 1 || q_theta < 0) # censor illegal parameter values
    return(NA) 
  for(f in 1:length(q_flips)) # Take the product across all flips
  {
    if(q_flips[f] == 'T')
      ll<-ll+log(q_theta)
    if(q_flips[f] == 'H')
      ll<-ll+log(1-q_theta)
  }
  return(ll)
}

## Compute the MLE using the generic numerical optimization
## routine optim()
m_eco_q<-optim(par=0.5,fn=log_likelihood_ecol_quarter,
    lower=0,upper=1, # range of params to search
    method='Brent',  # Use this method for 1D models
    control=list("fnscale"=-1),
    hessian=TRUE,
    q_flips=flips)

m_eco_q



## Compute the approximate confidence interval
confint(m_eco_q) 
###  --  ###	



########################################################################
########################################################################












#################################################################################
##### -- Estimating Biological Model Parameters with Maximum Likelihood -- ######
#################################################################################

##@  2.1 @##
data(ReedfrogFuncresp)		# Load Frog predation data
## Plot the data
plot(ReedfrogFuncresp$Initial,ReedfrogFuncresp$Killed, 
    xlab="Initial density",
    ylab="Number killed")
###  --  ###



##@  2.2 @##
## Visualize the binomial distribution
## Play with the value of N, and p, then
## re-run this section and what happens
N=10
p=0.2
k <- 0:N
li <- dbinom(k,N,p)
plot(k,li,type='h')
###  --  ###



##@  2.3 @##
## Define the likelihood function (type II functional response model)
binomLL2 = function(pars,data)  
{
  a = pars[1]
  h = pars[2]
  predprob = a/(1+a*h*data$N)
  sum(dbinom(data$k,prob=predprob,size=data$N,log=TRUE))
}
###  --  ###



##@  2.4 @##
## Estimate the Maximum Likelihood
frogs=data.frame(N=ReedfrogFuncresp$Initial,k=ReedfrogFuncresp$Killed)

m2<-optim(par=c(0.5,0.0125),fn=binomLL2,
    control=list("fnscale"=-1),
    hessian=TRUE,
    data=frogs)

m2 		# Output of MLE routine
###  --  ###


##@  2.5 @##
## Confidence intervals
confint(m2,parnames = c("a","h"))

## Likelihood surface plot
domain<-confint(m2,parnames = c("a","h"))
surface_plot(binomLL2,domain=domain,data=frogs)
###  --  ###



##@  2.6 @##
## Plot the data with MLE prediction
op=par(mfrow=c(1,1),bty="l",las=1)
plot(ReedfrogFuncresp$Initial,ReedfrogFuncresp$Killed,
    xlab="Initial density",
    ylab="Number killed",
    main='type II')

## Prediction and 95% conf. interval (stochastic interval) 
## NOTE: these intervals ignore parameter uncertainty - 
## this can be hazardous to your (academic) health. Come to the workshop on Bayesian to learn more!

  a<-m2$par[1]
  h<-m2$par[2]
  curve(a*x/(1+a*h*x),add=TRUE)
  curve(qbinom(0.025,size=floor(x),prob=a/(1+a*h*x)),lty=2,add=TRUE,type="s")
  curve(qbinom(0.975,size=floor(x),prob=a/(1+a*h*x)),lty=2,add=TRUE,type="s")

###  --  ###

#################################################################################
#################################################################################










########################################################################
###### -- Akaike Information Criterion & Model Selection  -- ###########
########################################################################

##@  3.1 @##
##### Alternative models: #####
## Likelihood function for a type I response (competing model 1)
binomLL1 = function(pars,data)  
{
  a = pars[1]
  if(a > 1 || a < 0) ## constrain: 0 < a < 1
    return(NA)
  predprob = a
  sum(dbinom(data$k,prob=predprob,size=data$N,log=TRUE))
}


m1<-optim(par=0.5,fn=binomLL1,    
    lower=0,upper=1, # range of params to search
    method='Brent',  # Use this method for 1D models
    control=list("fnscale"=-1),
    hessian=TRUE,
    data=frogs)

m1

## Likelihood function for a type III response (competing model 3)
binomLL3 = function(pars,data)  
{
  a = pars[1]
  h = pars[2]
  c_par = pars[3]
  if(c_par < 1 || h < 0 || h > 1) ## constrain: c and h
    return(NA)
  predprob = h-exp(-a*data$N^c_par)
  sum(dbinom(data$k,prob=predprob,size=data$N,log=TRUE))
}


m3<-optim(par=c(0.5,0.5,2),fn=binomLL3,
    control=list("fnscale"=-1),
    hessian=TRUE,
    data=frogs)

m3
###  --  ###
##################################################################




##@  3.2 @##

# This function calculates the Akaike Information Criterion
AIC<-function(L,k)		
{
  return(2*k - 2*L)
}

AIC(m1$value,1)	# Get AIC value for the type I model
AIC(m2$value,2)	# Get AIC value for the type II model
AIC(m3$value,3)	# Get AIC value for the type III model
###  --  ###


##@  3.3 @##

# This function calculates Akaike model weights, w_i
calc_w<-function(aic)		
{
  best_m<-min(aic)
  w<-sapply(1:length(aic), function (x) {
    return(exp(-0.5*(aic[x]-best_m))/sum(exp(-0.5*(aic-best_m)))) 
  })
  return(w)
}

## Calculate Model weights (L(M | D)) using AIC values
calc_w(c(AIC(m1$value,1),AIC(m2$value,2),AIC(m3$value,3)))
###  --  ###
##################################################################




##@ 3.4 @##
############ Plot each competing model and predictions #################
par(mfrow=c(2,2)) # 2*2 frame to plot all 3 models on
plot(ReedfrogFuncresp$Initial,ReedfrogFuncresp$Killed,
    xlab="Initial density",
    ylab="Number killed",
    main='type I')
## Add the MLE model predictions

  a<-m1$par[1]
  curve(a*x,add=TRUE)
  curve(qbinom(0.025,size=floor(x),prob=a),lty=2,add=TRUE, type="s")
  curve(qbinom(0.975,size=floor(x),prob=a),lty=2,add=TRUE,type="s")


## type II model
plot(ReedfrogFuncresp$Initial,ReedfrogFuncresp$Killed,
    xlab="Initial density",
    ylab="Number killed",
    main='type II')

## Prediction and 95% conf. interval 
## NOTE: these intervals ignore parameter uncertainty - come to the workshop on Bayesian to learn more!
  a<-m2$par[1]
  h<-m2$par[2]
  curve(a*x/(1+a*h*x),add=TRUE)
  curve(qbinom(0.025,size=floor(x),prob=a/(1+a*h*x)),lty=2,add=TRUE,type="s")
  curve(qbinom(0.975,size=floor(x),prob=a/(1+a*h*x)),lty=2,add=TRUE,type="s")

## type III model
plot(ReedfrogFuncresp$Initial,ReedfrogFuncresp$Killed,
    xlab="Initial density",
    ylab="Number killed",
    main='type III')

  a<-m3$par[1]
  h<-m3$par[2]
  c_par<-m3$par[3]
  curve(x*(h-exp(-a*x^c_par)),add=TRUE)
  curve(qbinom(0.025,size=floor(x),prob=(h-exp(-a*x^c_par))),lty=2,add=TRUE,type="s")
  curve(qbinom(0.975,size=floor(x),prob=(h-exp(-a*x^c_par))),lty=2,add=TRUE,type="s")

###  --  ###
########################################################################
########################################################################




##@ 4.0 @##
########### Sandbox - let's build and fit our own model ################

## Some data to fit a model to:
data<-read.csv('http://madere.biol.mcgill.ca/cchivers/test_data.csv')
head(data)
plot(data)

###  --  ###

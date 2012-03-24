library(MASS)
BCI <-
function(mcmc_object,interval=c(0.025,0.975))
{
    num_params<-length(mcmc_object$par_names)
    if(num_params > 1)
    {
        bci<-array(dim=c(num_params,length(interval)))
        post_mean<-numeric(num_params)
        for(i in 1:num_params)
        {
            bci[i,]<-quantile(mcmc_object$trace[,i],probs=interval)
            post_mean[i]<-mean(mcmc_object$trace[,i])
        }
        rownames(bci)<-mcmc_object$par_names
        colnames(bci)<-as.character(interval)
    }
    if(num_params == 1)
    {  
            bci<-quantile(mcmc_object$trace,probs=interval)
	    post_mean<-mean(mcmc_object$trace)
    }
    return(cbind(bci,post_mean))
}

mcmc_thin <-
function(mcmc_object,thin=5)
{
    num_params<-length(mcmc_object$par_names)
    if(num_params > 1)
    {
        ind<-seq(1,length(mcmc_object$trace[,1]),thin)
        mcmc_object$trace<-mcmc_object$trace[ind,]
    }else
    {
        ind<-seq(1,length(mcmc_object$trace),thin)
        mcmc_object$trace<-mcmc_object$trace[ind]
    }
        return(mcmc_object)
}
Metro_Hastings <-
function(li_func,pars,prop_sigma=NULL,par_names=NULL,iterations=50000,burn_in=1000,adapt_par=c(100,20,0.5,0.75),quiet=FALSE,...)
{
    if (!is.finite(li_func(pars, ...))) 
        stop("Seed parameter values <pars> are not in the defined parameter space.  Try new starting values for <pars>.")

    if (is.null(par_names))
        par_names<-letters[1:length(pars)]

    if(!is.null(dim(prop_sigma)))
    {
        if( ( dim(prop_sigma)[1] != length(pars) ||  dim(prop_sigma)[2] != length(pars) ) && !is.null(prop_sigma) )
            stop("prop_sigma not of dimension length(pars) x length(pars)")
    }

    if(is.null(prop_sigma)) #if no proposal matrix given, estimate the Fisher information to use as the diagonal (start in the right variance scale)
    {
        if(length(pars)!=1)
        {
            fit<-optim(pars,li_func,control=list("fnscale"=-1),hessian=TRUE,...)
            fisher_info<-solve(-fit$hessian)
            prop_sigma<-sqrt(diag(fisher_info))
            prop_sigma<-diag(prop_sigma)
        }else{
            prop_sigma<-1+pars/2
        }
    }

    prop_sigma<-makePositiveDefinite(prop_sigma)
	mu<-pars
	pi_X<-li_func(pars,...)			        # initial likelihood evaluation
	k_X<-pars
	trace<-array(dim=c(iterations,length(pars)))
    deviance<-array(dim=iterations) 
    announce<-floor(seq(iterations/10,iterations,length.out=10))
	for(i in 1:iterations)
	{
	        k_Y<-mvrnorm(1,mu=k_X,Sigma=prop_sigma)	# Draw proposal point
        				
        	pi_Y<-li_func(k_Y,...)		        # evaluate likelihood at proposal point
        		
        	a_X_Y = (pi_Y)-(pi_X)		        # Compare relative likelihoods
        	if(is.nan(a_X_Y))
                    a_X_Y<--Inf                          # never jump outside the range of the parameter space
        	if( log(runif(1,0,1)) <= a_X_Y)	        # Make the jump according to MH probability
              	{
        		k_X = k_Y
        		pi_X = pi_Y
        	}	
        
        	trace[i,]<-k_X	
                deviance[i]<-(-2*pi_X)                      # Store the deviance for calculating DIC
		if(i > adapt_par[1] && i %% adapt_par[2] == 0 && i < (adapt_par[4]*iterations) )	        # adapt the proposal covariance structure
		{   
                    len<-floor(i*adapt_par[3]):i
                    x<-trace[len,]
                    N<-length(len)
                    p_sigma <- (N-1) * var(x)/N
                    p_sigma <-makePositiveDefinite(p_sigma)   # To deal with rounding problems that can de-symmetrize
                    if(!(0 %in% p_sigma) ) 
                        prop_sigma<-p_sigma
         }
         if(!quiet && i %in% announce)
            print(paste('updating: ',i/iterations*100,'%',sep=''))
	}
    trace<-trace[burn_in:iterations,]
    DIC<-NULL
    ## Calculate the DIC
    D_bar<-mean(deviance[burn_in:iterations])
    if(length(pars)>1)
    {
        theta_bar<-sapply(1:length(pars),function(x){mean( trace[,x] )})
    }else
        theta_bar<-mean( trace )

    D_hat<-li_func(theta_bar,...)
    pD<-D_bar-D_hat
    DIC<-D_hat + 2*pD
    ##
    if(length(pars)>1)
        accept_rate<-length(unique(trace[,1]))/(iterations-burn_in) else
    accept_rate<-length(unique(trace))/(iterations-burn_in)
    val<-list("trace"=trace,"prop_sigma"=prop_sigma,"par_names"=par_names,"DIC"=DIC,'acceptance_rate'=accept_rate)
    class(val)<-"MHposterior"
	return(val)
}
plotMH <-
function(mcmc_object,correlogram=TRUE)
{
    par(pty='s',mar=c(3.1,4,3,1),mgp=c(2,1,0))
    num_params<-length(mcmc_object$par_names)
    par(mfrow=c(num_params,2))
    
    if(num_params > 1)
    {
        for(i in 1:num_params)
        {
            par<-mcmc_object$par_names[i]
            hist(mcmc_object$trace[,i],breaks=30,probability=TRUE,xlab=par,ylab='Posterior Density',main=paste('Posterior distribution of ',par) )
            plot(mcmc_object$trace[,i],type='l',main=paste('Trace of ',par),xlab='Iteration',ylab=par)
        }
      
        if(correlogram)
        {
            par(ask=TRUE)
            par(mfrow=c(num_params,num_params),mar=c(2.5,2,1,1),mgp=c(2,1,0))
            for(i in 1:num_params)
            {
                for(j in 1:num_params)
                {
                    if(i == j)
                    {
                        frame()
                        text(0.5,0.5,mcmc_object$par_names[i],cex=2)
                    }
                    if(i != j)
                        plot(mcmc_object$trace[,j],mcmc_object$trace[,i],xlab='',ylab='',main='')
                }
            }
        }
    }
    if(num_params == 1)
    {
            par<-mcmc_object$par_names
            hist(mcmc_object$trace,breaks=30,probability=TRUE,xlab=par,ylab='Posterior Density',main=paste('Posterior distribution of ',par) )
            plot(mcmc_object$trace,type='l',main=paste('Trace of ',par),xlab='Iteration',ylab=par)
    }
    par(ask=FALSE)
}


# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 DESCRIPTION:
#  isPositiveDefinite        M  Checks if the matrix X is positive definite
#  makePositiveDefinite      M  Forces the matrix x to be positive definite
################################################################################


isPositiveDefinite <-
    function(x)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Checks if the matrix x is positive definite

    # Arguments:
    #   x - a symmetric matrix or any other rectangular object
    #       describing a covariance matrix which can be converted into
    #       a matrix by the function 'as.matrix'.

    # FUNCTION:

    # Transform:
    x = as.matrix(x)

    # Check if matrix is positive definite:
    ans = .is.positive.definite(m = x)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.is.positive.definite <-
    function (m, tol, method = c("eigen", "chol"))
{
    # Author:
    #   Copyright 2003-05 Korbinian Strimmer
    #   Rank, condition, and positive definiteness of a matrix
    #   GNU General Public License, Version 2

    method = match.arg(method)
    if (!is.matrix(m)) {
        m = as.matrix(m)
    }
    if (method == "eigen") {
        eval = eigen(m, only.values = TRUE)$values
        if( missing(tol) ) {
            tol = max(dim(m))*max(abs(eval))*.Machine$double.eps
        }
        if (sum(eval > tol) == length(eval)) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    } else if (method == "chol") {
        val = try(chol(m), silent = TRUE)
        if (class(val) == "try-error") {
            return(FALSE)
        } else {
            return(TRUE)
        }
    }
}


# ------------------------------------------------------------------------------


makePositiveDefinite <-
    function(x)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Forces the matrix x to be positive definite

    # Arguments:
    #   x - a symmetric matrix or any other rectangular object
    #       describing a covariance matrix which can be converted into
    #       a matrix by the function 'as.matrix'.

    # FUNCTION:

    # Make Positive Definite:
    ans = .make.positive.definite(m = x)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.make.positive.definite <-
    function(m, tol)
{
    # Author:
    #   Copyright 2003-05 Korbinian Strimmer
    #   Rank, condition, and positive definiteness of a matrix
    #   GNU General Public License, Version 2

    # Method by Higham 1988

    if (!is.matrix(m)) {
        m = as.matrix(m)
    }

    d = dim(m)[1]
    if ( dim(m)[2] != d ) {
        stop("Input matrix is not square!")
    }

    es = eigen(m)
    esv = es$values

    if (missing(tol)) {
        tol = d*max(abs(esv))*.Machine$double.eps
    }
    delta =  2*tol
        # factor two is just to make sure the resulting
        # matrix passes all numerical tests of positive definiteness

    tau = pmax(0, delta - esv)
    dm = es$vectors %*% diag(tau, d) %*% t(es$vectors)

    # print(max(DA))
    # print(esv[1]/delta)

    return( m + dm )
}


################################################################################


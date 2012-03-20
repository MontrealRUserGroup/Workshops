## Corey Chivers, 2012
## corey.chivers@mail.mcgill.ca

## WARNING: This method of calculating the CI of MLE 
## parameters assumes that the likelihod function is 
## normally distributed. Like many classical statistical
## techniques, it is an asymtotic approximation.
confint<-function(fit,conf=0.95,parnames=NULL)
{
    n_par<-length(fit$par)
    interval<-c((1-conf)/2,1-((1-conf)/2))

    fisher_info<-solve(-fit$hessian)
    se_est<-sqrt(diag(fisher_info))
    
    ci<-array(dim=c(n_par,2))
    
    for(i in 1:n_par)
        ci[i,]<-qnorm(interval,fit$par[i],se_est[i])
    
    colnames(ci)<-paste(interval*100,"%")

    if(is.null(parnames))
    {
        rownames(ci)<-paste("par",1:n_par)
    }else
        rownames(ci)<-parnames

    return(ci)
}

## Plot the likelihood surface for a 2D model
surface_plot<-function(lhood,res=50,domain,...)
{
    x<-seq(domain[1,1],domain[1,2],length.out=res)
    y<-seq(domain[2,1],domain[2,2],length.out=res)

    z<-array(dim=c(res,res))

    for(i in 1:res)
        for(j in 1:res)
            z[i,j]<-lhood(c(x[i],y[j]),...)


    persp(x=x,y=y,z,theta=45,phi=35,ticktype='detailed',zlab='Log L')
}


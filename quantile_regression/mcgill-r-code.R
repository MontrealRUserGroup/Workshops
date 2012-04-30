# Montreal R meeting
# Quantiles, by Arthur Charpentier
# charpentier.arthur@uqam.ca
# April, 23rd, 2012

u=seq(0,1,by=.01)
plot(u,qnorm(u),lwd=2,col="red",type="l",ylim=c(-3,6))

lines(u,qnorm(u,mean=1,sd=2),col="blue",type="l")
u=seq(0,1,by=.1)
points(u,qnorm(u,mean=0,sd=1)*2+1,col="blue",pch=19,cex=1.5)

set.seed(1)
X=rnorm(1000)
boxplot(X,X[1:50],horizontal=TRUE,col="light blue")

# install.packages("quantreg", dependencies = TRUE)
library(quantreg)

base=read.table("http://freakonometrics.free.fr/salmon_dat.csv",sep=",",header=TRUE)
attach(base)
plot(body_length,egg_mass)
abline(lm(egg_mass~body_length),col="red",lwd=2)

library(quantreg)
abline(rq(egg_mass~body_length,tau=.5),col="blue",lwd=2)

summary(rq(egg_mass~body_length,tau=.5))

StormMax=read.table("http://freakonometrics.free.fr/extremedatasince1899.csv",header=TRUE,sep=",")
StormMaxBasin=subset(StormMax,Region=="Basin")
StormMaxBasin=subset(StormMaxBasin,Yr>1977)
attach(StormMaxBasin)

plot(Yr,Wmax,ylim=c(35,175),
xlab="Year",ylab="Intensity (kt)",col="blue")
abline(lm(Wmax~Yr),lwd=2,col="red")

boxplot(Wmax~as.factor(Yr),ylim=c(35,175),
xlab="Year",ylab="Intensity (kt)",col="light blue")

bp=boxplot(Wmax~as.factor(Yr),ylim=c(35,175),
xlab="Year",ylab="Intensity (kt)",col="light blue")
x=1:29
points(x,bp$stats[4,],pch=19,col="purple")
abline(lm(bp$stats[4,]~x),col="purple",lwd=2)

points(x,bp$stats[5,],pch=19,col="red")
abline(lm(bp$stats[5,]~x),col="red",lwd=2)


Q=function(p=.9) as.vector(by(Wmax,as.factor(Yr),function(x) quantile(x,p)))
V90=Q(.9)
bp=boxplot(Wmax~as.factor(Yr),ylim=c(35,175),
xlab="Year",ylab="Intensity (kt)",col="light blue")
u=1:29
points(u,V90,col="red",pch=19)
abline(lm(V90~u),col="red",lwd=2)

slope=function(p){return((lm(Q(p)~u,weights=table(Yr))$coefficients[2]))}
slopeic=function(p){return(confint(lm(Q(p)~u,weights=table(Yr)))[2,])}
pb=seq(.01,.99,by=.01)
MV =Vectorize(slope)(pb)
MIC=Vectorize(slopeic)(pb)
plot(pb,MV,ylim=range(MIC),col="white",ylab="slope",xlab="probability level")
polygon(c(pb,rev(pb)),c(MIC[1,],rev(MIC[2,])),col="light green",border=NA)
lines(pb,MV,lwd=2,col="red")
abline(h=0)

attach(base)
REG0=lm(egg_mass~body_length)
as.vector(REG0$coefficients)
E=residuals(REG0)
REG1=lm(egg_mass~body_length,weight=1/abs(E))
as.vector(REG1$coefficients)
E=residuals(REG1)
REG2=lm(egg_mass~body_length,weight=1/abs(E))
as.vector(REG2$coefficients)

REG=lm(egg_mass~body_length)
for(i in 1:200){
E=residuals(REG)
REG=lm(egg_mass~body_length,weight=1/abs(E))
print(as.vector(REG$coefficients))
}

poids=function(e) pmin(1/abs(e),10000)
REG=lm(egg_mass~body_length)
for(i in 1:200){
E=residuals(REG)
REG=lm(egg_mass~body_length,weight=poids(E))
print(as.vector(REG$coefficients))
}

summary(rq(egg_mass~body_length,tau=.5))

plot(egg_mass~body_length,xlim=c(75,85),ylim=c(35,42))
abline(rq(egg_mass~body_length,tau=.5),lwd=2,col="red")

u=seq(.05,.95,by=.01)
model=rq(egg_mass~body_length,tau=u)
coefsup=function(u) summary(rq(egg_mass~body_length,tau=u))$coefficients[,3]
coefinf=function(u) summary(rq(egg_mass~body_length,tau=u))$coefficients[,2]
coefest=function(u) summary(rq(egg_mass~body_length,tau=u))$coefficients[,1]
CS=Vectorize(coefsup)(u)
CI=Vectorize(coefinf)(u)
CE=Vectorize(coefest)(u)

k=2
plot(u,CE[k,],xlab="probability level",ylab="body_length",col="white",ylim=c(min(0,CI[k,]),max(CS[k,])))
polygon(c(u,rev(u)),c(CS[k,],rev(CI[k,])),col="light green",border=NA)
lines(u,CE[k,],lwd=2,col="red")
abline(h=0)

u=1:29
Q=function(p=.9) as.vector(by(Wmax,as.factor(Yr),function(x) quantile(x,p)))
V90=Q(.9)
bp=boxplot(Wmax~as.factor(Yr),ylim=c(35,175),
xlab="Year",ylab="Intensity (kt)",col="light blue")
points(u,V90,col="red",pch=19)
abline(lm(V90~u),col="red",lty=2)
abline(lm(V90~u,weights=table(Yr)),col="red",lwd=2)

u=seq(.05,.95,by=.01)
coefsup=function(u) summary(rq(Wmax~Yr,tau=u))$coefficients[,3]
coefinf=function(u) summary(rq(Wmax~Yr,tau=u))$coefficients[,2]
coefest=function(u) summary(rq(Wmax~Yr,tau=u))$coefficients[,1]
CS=Vectorize(coefsup)(u)
CI=Vectorize(coefinf)(u)
CE=Vectorize(coefest)(u)
k=2
plot(u,CE[k,],xlab="probability",ylab="slope",col="white",ylim=c(min(CI[k,]),max(CS[k,])))
polygon(c(u,rev(u)),c(CS[k,],rev(CI[k,])),col="light green",border=NA)
lines(u,CE[k,],lwd=2,col="red")
abline(h=0)


base=read.table("http://freakonometrics.free.fr/natality2005.txt",sep=";")
base=base[1:5000,]
u=seq(.05,.95,by=.01)

coefstd=function(u) summary(rq(WEIGHT$\sim$SEX+SMOKER+WEIGHTGAIN+
BIRTHRECORD+AGE+ BLACKM+ BLACKF+COLLEGE,data=base,tau=u))$coefficients[,2]
coefest=function(u) summary(rq(WEIGHT$\sim$SEX+SMOKER+WEIGHTGAIN
BIRTHRECORD+AGE+ BLACKM+ BLACKF+COLLEGE,data=base,tau=u))\$coefficients[,1]
CS=Vectorize(coefsup)(u)
CE=Vectorize(coefest)(u)
k=2
plot(u,CE[k,])
polygon(c(u,rev(u)),c(CE[k,]+1.96*CS[k,],rev(CE[k,]-1.96*CS[k,])))

summary(rq(WEIGHT~SEX+SMOKER+WEIGHTGAIN+
BIRTHRECORD+AGE+ BLACKM+ BLACKF+COLLEGE,data=base,tau=.8))

library(mnormt)
Z=rmnorm(20,mean=c(0,0),varcov=matrix(c(1,.6,.6,1),2,2))
source("http://www.wiwi.uni-bielefeld.de/~wolf/software/R-wtools/bagplot/bagplot.R")
bagplot(Z,factor=1,dkmethod=1)

library(rainbow))
plot(ElNino)

(PCA=PCAproj(t(ElNino\$y), center = median))
bagplot(PCA$loadings,factor=1,dkmethod=1)


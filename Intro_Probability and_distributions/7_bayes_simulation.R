##general house cleaning
##get use to this early
##this clears the workspace
rm(list = ls())


#Bayes theorem simulation

states<-c("diseased", "healthy")
contamination.level<-0.05
state.p<-c(contamination.level, 1-contamination.level)

population<-data.frame(state=sample(states, size=10000, replace=T, prob= state.p))





diagnostic.f<-function(state){
	
	diagnostics<-c("diseased", "healthy")
	sensitivity<-0.95
	specificity<-0.95
	
	if(state=="diseased") diagnostic<-sample(diagnostics, 1, prob=c(sensitivity, 1-sensitivity))
	if(state=="healthy") diagnostic<-sample(diagnostics, 1, prob=c(1-specificity, specificity))
	
	}
	
population$diagnostic<-sapply(population$state, FUN=diagnostic.f)

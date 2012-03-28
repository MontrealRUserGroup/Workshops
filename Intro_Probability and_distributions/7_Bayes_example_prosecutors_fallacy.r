
#First let's set up the parameters of the model
prob.disease = 0.001                                #probability of an individual having the disease
prob.dect.present = 0.99                            #probability of our test reporting the disease present if it's there
prob.dect.absent = 0.01                             #probability of our test falsely reporting the disease present if it's not there

pop.size = 10000

pop = rbinom(pop.size,prob=prob.disease,size=1)     #generates a population of 10000, who have the disease (1) or don't (0)
                                                    #rbinom is a command that generates a random binomial number. don't stress too much about it right now; We'll be covering it and
                                                    #other, related functions in depth next week

test.result = ifelse(pop==1,
  rbinom(pop.size,prob.dect.present,size=1),
  rbinom(pop.size,prob.dect.absent,size=1))             #generates the vector of test results, accounting for both false positives and negatives



#percentage of positive diagnoses that correctly identified the disease.
percent.correct = length(test.result[pop==1&test.result==1])/length(test.result[test.result==1])        


#calculating the predicted number of correctly diagnosed patients. Try and figure out why the denominator takes the form it does
bayes.predict = prob.dect.present * prob.disease/
(prob.dect.present*prob.disease + prob.dect.absent*(1-prob.disease))                                   


#Now let's rerun the simulation a few thousand times, to find out if this actually works:
bayes.repeat.sample = rep(0,length=500)
for(i in 1:500){
  pop = rbinom(pop.size,prob=prob.disease,size=1)
  test.result = ifelse(pop==1,
    rbinom(pop.size,prob.dect.present,size=1),
    rbinom(pop.size,prob.dect.absent,size=1))
  percent.correct = length(test.result[pop==1&test.result==1])/length(test.result[test.result==1])
  bayes.repeat.sample[i] = percent.correct
}

quantile(bayes.repeat.sample,c(0.05,0.5,0.95))
mean.bias = mean(bayes.repeat.sample - bayes.predict)




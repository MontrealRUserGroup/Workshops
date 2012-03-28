# Baye's Theorem and inverse probabilities
# P(S|D)=(P(D|S)/(P(D|S)P(S)+P(D|!S)(1-P(S))))P(S)

#The theorem allows the calculation of three probabilities from their inverses

#Creation of a the bayes() function for visualization of the application of Baye's Theorem

	#The function has three inputs:
		#1. sensitivity P(D|S), which is the probability of getting a positive diagnostic result when the state is positive
		#2. specificity P(!D|!S)=1-P(D|!S), which is the probability of getting a negative result when the state is negative
			#which is equal to 1-the false positive rate, which is getting a positive result for a negative state
		#3. base_rate P(S), which is the frequency of the state in the population
	
	#The function returns 4. the accuracy of the diagnostic P(S|D), the probability of a positive state given a positive diagnostic
		#the other inverse probabilities that can be calculated using the theorem (and could be integrated to this function) are
			#6. the false negative rate P(S|!D), the probability of having a positive state given a negative diagnostic
			#5. the diagnostic rate P(D), the frequency of a positive diagnostic in the population


	#The function could be rewritten (and please feel free to do so):
		#to have probabilities 4-6 as inputs
		#to calculate three probabilities depending on the probabilities given
		
	#The function allows the visualization of the interdependancy between the three given probabilities and their inverses
		#the visualization is based on the Wolfram Demonstration Project http://demonstrations.wolfram.com/BayessTheoremAndInverseProbability/


#Create the function
bayes<-function(sensitivity, specificity, base_rate){
	
	#Start an empty plot with axis from 0 to 1
	#There are many ways to do this, here we use data spanning the desired interval:seq(0,1,0.1)
	#We then set point character to an empty space pch=""
	#We also set the axis to "i" which plots axis within the data interval (as compared to the default of adding 4% to each end of the axis)
	#We also add axis labes with ylab and xlab
	plot(seq(0,1,0.1),seq(0,1,0.1), xaxs="i", yaxs="i", xlab="Probability of state", ylab="Probability of diagnostic signal", pch="", main="Baye's theorem and inverse probabilities")


	#We place points at the know locations on the plot
		#sensitivity P(D|S)  shown in RED	
		points(x=1, y=sensitivity, cex=2,xpd=NA, col="red", pch=16)
		#add a label
		text(x=1, y=sensitivity, col="red", labels="sensitivity P(D|S)", pos=2, adj=1 )
		
		#specificity P(!D|!S)=1-P(D|!S)
		#1-specificity shown in PURPLE
		points(x=0, y=1-specificity, cex=2,xpd=NA, col="purple", pch=16)
		text(x=0, y=1-specificity, col="purple", labels="1-specificity P(D|!S)", pos=4, adj=0 )

		#base_rate P(S)	shown in GREEN
		points(x=base_rate, y=0, cex=2,xpd=NA, col="green", pch=16)
		text(x= base_rate, y=0, col="green", labels="base_rate P(S)", pos=3, adj=0.5 )

	#We trace the marginal probabilities
	
		#The red dotted line between the conditional probabilities 1-specificity P(D|!S) and  sensitivity P(D|S) traces out the marginal probability of P(D)
		#a weighted average of the two endpoint conditionals using the base rate P(S)  to specify a weighting function.
			lines(x=c(0,1), y=c(1-specificity, sensitivity), col="red", lty=2)
			#doted greem line up from P(S) fot the weighting
			abline(v= base_rate, lty=2, col="green")
		
		#Similarly, the blue dotted line between  P(S|!D) and P(S|D)  traces out the marginal probability P(S)
		#a weighted average of the two endpoint conditionals using  to specify a weighting function
			#Using the theorem we can calculate the diagnostic accuracy
			#diagnostic_value P(S|D)
			diagnostic_value= base_rate*sensitivity/((sensitivity*base_rate)+(1-specificity)*(1-base_rate))
			#Using the theorem, we already know two points (P(S|D) and the intercection of the red and green line), which we can enter into a data frame
			#Lets add a point at the intersection where {P(S),P(D)}
			points(base_rate, (1-specificity)+base_rate*(sensitivity-(1-specificity)), pch=1, cex=2)
			text(base_rate, (1-specificity)+base_rate*(sensitivity-(1-specificity)), col="black", labels=" {P(S),P(D)}", pos=3, adj=0.5 )
			 #and at the diagnostic accuracy P(S|D) in blue
			 points(x= diagnostic_value, y=1, cex=2,xpd=NA, col="blue", pch=16)
			text(x= diagnostic_value, y=1, col="blue", labels="diagnostic accuracy P(S|D) ", pos=1, adj=0.5 )
			#data for blue line
			inverse<-data.frame(S=c(base_rate, diagnostic_value), D=c((1-specificity)+base_rate*(sensitivity-(1-specificity)),1))
			#we can use this data to produce a linear model
			model<-lm(D~S, inverse)
			#we can then plot this model using abline, which will automaticaly extract the slope and intercept parameters from a model object
			abline(model, lty=2, col="blue")


	#We send the diagnostic accuracy as a results using return()
	return(diagnostic_value)

}

#Run the function on your choice of know probabilities
bayes(0.8, 0.7, 0.2)
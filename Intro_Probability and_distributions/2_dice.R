# -*- coding: utf-8 -*-
#####################
#####################
##
##	Create a dice and throw it
##
####################
####################

##general house cleaning
##get use to this early
##this clears the workspace
rm(list = ls())


	#create a dice

		#create a dice by creating an object, called "normal.dice", from a sequence of numbers
		#seq() is a function that creates a sequence, it has arguments "from", "to" and "by" (increments)
	
	

normal.dice<-seq(from=1, to=6, by=1)



	#create a function to throw your dice

		#create an object for the function called "throw"
		#this function will have the arguments "dice" which will be the dice you will give the function
		#and n.throws which will be the number of times to throw the dice
		#
		#the function you create will contain {} the function sample()
		#this function takes a value randomly from its argument x=dice
		#it takes size=n.throws values and allows for replacement (replacement=T for TRUE)
		#replacement means each value can be picked many times, just like multiple throws of dice can fall on the same value


throw<-function(dice, n.throws){sample(x=dice, size=n.throws, replace=T)}



	#try it, throw your dice once
		#select the text including the next line, the function that throws the dice with your normal.dice as argument
	
	
throw(normal.dice, 1)
	
	
	
	#if you are in R
		#in OS X press command-enter
		#in windows press control-r
	#if you are outside of R
		#copy your text into r (either the GUI or the command line)
		


	#throw your dice n.throws

n.throws<-1000

throw(normal.dice,n.throws)


	#save the results of n.throws into an object called "game"


game<-throw(normal.dice,n.throws)


	#visualize the results using the function hist()
	#to learn more about hist() or anything in R
	#try ?hist or ??hist
	#this function plots a histogram of frequency: counts per data
	#breaks is the values of the extremes of the bins
	#from which to which value should be incorporated into each category
	

par(mfrow = c(1,2)) 
	#this command will set up a grid of figures; par is a command
	#to change graphical parameters, and mfrow is an arguement to say how many
	#rows and columns of figures you want. Each new plot command will fill
	#in this grid starting from top left, and moving right, then down.

hist(game,breaks=seq(from=0.5,to=6.5,by=1))

	#to create a probability density histogram
	#incert the argument freq=F (for false)
	
hist(game,breaks=seq(from=0.5,to=6.5,by=1), freq=F)


####################
####################
## If you are not impressed by numbers, and feel the need to see a coin actually flip
## The package TeachingDemos has just what you need

#check that TeachingDemos is installed, install it if it is not
if(require("TeachingDemos")==F)
install.packages("TeachingDemos")
#load TeachingDemos
require("TeachingDemos")


#create the die
plot.rgl.die()
#roll the regular die
roll.rgl.die(steps=50)


##TeachingDemos also contains a "dice" that throws "ndice", "rolls" times

dice(rolls=1, ndice=1, sides=6, plot.it=T, load=rep(1, 6))



#look into the functions by either typing them directly to see their content or with ?, ??
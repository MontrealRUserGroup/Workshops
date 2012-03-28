##general house cleaning
##get use to this early
##this clears the workspace
rm(list = ls())
#you could also set your working directory
#setwd("path/to/directory")


#use the dice and throwing function you already built
#you dice
dice<-seq(from=1, to=6, by=1)
#your throwing function
throw<-function(dice, n.throws){sample(x=dice, size=n.throws, replace=T)}


#number of throws
#lets set the number of throws as in a Chevalier
#de Méré's game
n.throws<-24

#now create a game with two dice being thrown each time
game<-data.frame(dice1=throw(dice,n.throws), dice2=throw(dice,n.throws))

#check how many double sixes you get
#note that conditions (==,>=,>,...) lead to logical TRUE/FALSE
#FALSE has a numeric value of 0, while TRUE has a numeric value of  1
sum(game$dice1==6 & game$dice2==6)

#create a function that can play the game for you
play.game<-function(n.throws){
	game<-data.frame(dice1=throw(dice,n.throws), dice2=throw(dice,n.throws))
	
	#we will compile the number of games won
	wins<-wins+as.integer(sum(game$dice1==6 & game$dice2==6)>=1)
	return(wins)
	}


#set the number of games played
n.games<-10000

#initalize wins
wins<-0

#play the games using a "for" or a "while" loop
#initialize loop
i<-0

#time the process using system.time
sytems.time(
for(i=1:n.games){
	wins<-play.game(n.throws)
	}
)
wins/n.games



#play the games using "apply" or "lapply" on a vector
#time the process using "system.time"

system.time(
index<-rep(x=n.throws,times=n.games)
total.wins<-lapply(X=index, FUN=play.game)
wins/n.games
)

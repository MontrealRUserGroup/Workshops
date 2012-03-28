##general house cleaning
##get use to this early
##this clears the workspace
rm(list = ls())

#install and load required libraries
if(require("TeachingDemos")==F)
install.packages("TeachingDemos")
#load TeachingDemos
require("TeachingDemos")

#you could also set your working directory
#setwd("path/to/directory")

####################
####################
## Cheating
####################
####################


#use the dice and throwing function you already built
#you dice
dice<-seq(from=1, to=6, by=1)

#you can give a probability to each value on your dice
#here is the vector of the probabilities
p_dice<-c(1/6,1/6,1/6,1/6,1/6,1/6)
#or easier
p_dice<-rep(1/6,6)

#now lets rewrite the throwing function to include these values
#your throwing function
throw<-function(dice, n.throws, p_dice){sample(x=dice, size=n.throws, replace=T, prob= p_dice)}

#create an object n.throws to rapidly change the number of throws
n.throws<-1000

#now create a game of n.throws
game<-throw(dice, n.throws, p_dice)

#so far probabilities have not been affected
p6<-sum(game==6)/n.throws
p6


#start cheating: load your dice
#now lets buid our cheated probabilities
p_dice<-c(0,0,0,0,0,1/6)

#here is the game were we win every time
game<-throw(dice, n.throws, p_dice)

p6<-sum(game==6)/n.throws
p6


##
## you can do the above with the TeachingDemo library's dice() function
dice(rolls=1,ndice=2, sides=6, plot.it=T, load= p_dice)


####################
####################
## Coins
####################
####################



#create a coin
coin<-c("heads", "tails")

#throw the coin
sample(coin, size=1, replace=T, prob=NULL)


####################
####################
## Again, if you need the glamourous visuals
##
## 3D coin
##
####################
####################


#plot coin
plot.rgl.coin()
#flit coin
flip.rgl.coin()



####################
####################
##
## Deck of cards
##
####################
####################



#create a deck of cards
card_deck<-data.frame(suite=rep(c("Clubs","Diamonds", "Hearts", "Spades"), each=13), color=rep(c("red", "black"), each=26), values=rep(c(1:10,"Jack", "Queen", "King"), times=4))


#draw five cards for your hand
hand<-card_deck[sample(nrow(card_deck), size=5),]






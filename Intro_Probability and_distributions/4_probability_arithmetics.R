##general house cleaning
##get use to this early
##this clears the workspace
rm(list = ls())
#you could also set your working directory
#setwd("path/to/directory")


#use the dice and throwing function you already built
#your dice
dice<-seq(from=1, to=6, by=1)
#your throwing function
throw<-function(dice, n.throws){sample(x=dice, size=n.throws, replace=T)}

#create an object n.throws to rapidly change the number of throws
n.throws<-1000

#create a game with a dice being thrown n.throws
game<-throw(dice,n.throws)


#Mutually exclusive events (sampling without replacement)
#calculate the probability of two events
sum(game==1)/length(game)
sum(game==2)/length(game)

#calculate the combined probability of an event OR another
(sum(game==1) + sum(game==2))/length(game)

sum(game==1 | game==2)/length(game)



#Events not mutually exclusive (sampling with replacement)

#now create a game with two dice being thrown each time
game<-data.frame(dice1=throw(dice,n.throws), dice2=throw(dice,n.throws))

#note that conditions (==,>=,>,...) lead to logical TRUE/FALSE
#FALSE has a numeric value of 0, while TRUE has a numeric value of  1


#find the probability of getting a 2 on the first dice and getting a 4 on the second dice
# "|" means "or" in this context ?"|"

p2or4<-sum(game$dice1==2|game$dice2==4)/n.throws
p2or4

#another way to do the same thing
p2or4<-with(game, sum(dice1==2|dice2==4))/n.throws


#probability of getting a sum to 5
# "&" is the AND symbols
p1and4<-with(game, sum(dice1==1&dice2==4))/n.throws
p2and3<-with(game, sum(dice1==2&dice2==3))/n.throws
p3and2<-with(game, sum(dice1==3&dice2==2))/n.throws
p4and1<-with(game, sum(dice1==4&dice2==1))/n.throws

psum5<-sum(p1and4, p2and3, p3and2, p4and1)
print(psum5)


#or
psum5<-with(game, sum((dice1+dice2)==5)/n.throws)
print(psum5)




#################
#Visualization using Ven diagrams
#the experimental package venneuler allows the use of Ven digrams
#to express sets that are in the form of matrices
#################
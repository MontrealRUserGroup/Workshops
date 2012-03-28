#####################
#####################
##
##	A dice game with the second throw conditional on the first
##
####################
####################

##general house cleaning
##get use to this early
##this clears the workspace
rm(list = ls())





throw<-function(){
	
#create the first dice
	dice1<-seq(from=1, to=6, by=1)
	p_dice1<-c(1/6,1/6,1/6,1/6,1/6,1/6)

#create the second dice
	#two version of the second dice will exist
	#a first version for small values
	dice2.small<-seq(from=1, to=6, by=1)
	p_dice2.skewed<-c(1/24,1/12,3/24,1/6,5/24,3/8)

	#a second version for large values
	dice2.large<-seq(from=7, to=12, by=1)
	p_dice2.normal<-c(1/6,1/6,1/6,1/6,1/6,1/6)
	
	#the first dice is thrown
	dice1<-sample(x=dice1, size=1, replace=T, prob= p_dice1)
	
	#how the second dice is thrown depends on the value of the first dice
	if(dice1<=3) dice2<-sample(x=dice2.small, size=1, replace=T, prob= p_dice2.skewed) else if(dice1>=4)dice2<-sample(x=dice2.large, size=1, replace=T, prob=p_dice2.normal)
	game<-data.frame(dice1=dice1, dice2=dice2)
}	


game<-replicate(1000, throw())
game<-t(game)



######
#There are many other solutions for this simulation
# here you can find indexing
#you could also have a for loop, use aggregation function (eg. aggregate, apply, by)

			#create the first dice
				dice1<-seq(from=1, to=6, by=1)
				p_dice1<-c(1/6,1/6,1/6,1/6,1/6,1/6)

			#create the second dice
				#two version of the second dice will exist
				#a first version for small values
				dice2.small<-seq(from=1, to=6, by=1)
				p_dice2.skewed<-c(1/24,1/12,3/24,1/6,5/24,3/8)

				#a second version for large values
				dice2.large<-seq(from=7, to=12, by=1)
				p_dice2.normal<-c(1/6,1/6,1/6,1/6,1/6,1/6)


#create first dice
game<-data.frame(dice1=sample(dice1,1000, replace=T))
game$dice2[game$dice1<=3]<-sample(dice2.small, sum(game$dice1<=3), replace=T, prob=p_dice2.skewed)
game$dice2[game$dice1>3]<-sample(dice2.large, size=sum(game$dice1>3), replace=T, prob=p_dice2.normal)
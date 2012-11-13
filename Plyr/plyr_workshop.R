##
#Data manipulation in R: Approaches using plyr and reshape
#by: Etienne Low-Decarie
##

################################################################################

##
# Housekeeping
##

rm(list=ls())

##
#Load libraries
##

#install.packages("plyr")
library(plyr)

#install.packages("reshape")
library(reshape)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("vegetarian")
library(vegetarian)

#install.packages("vegan")
library(vegan)


################################################################################

##
#Make your data play nice
##

###################
#Make you data long
###################


# molten.data<-melt(data,
#                   id.vars=ls("id.var.1", "id.var.2"),
#                   measure.vars=ls("measure.vars", "measure.vars"),
#                   variable_name = "variable")


#Make the iris data long

  iris$id<-row.names(iris)
  
  molten.iris<-melt(iris,
                    id.vars=c("Species", "id"),
                    #measure.vars=c("measure.vars", "measure.vars"),
                    variable_name = "measure")
  
  head(molten.iris)

###################
#Make you data wide
###################

# cast.data<-cast(data,
#                 formula = x_variable_1 + x_variable_2 ~
#                   y_variable_1 + y_variable_2)


#Make the molten.iris data wide
  
cast.data<-cast(molten.iris,
                formula = Species + id ~ ...)

head(cast.data)

  
################################################################################

##
#Split-Apply-Combine:Old school R
##

#Calculate the mean Sepal Length for setosa

#Split = subset 
  selected.iris<-molten.iris[molten.iris$Species=="setosa" & 
                              molten.iris$measure=="Sepal.Length" ,]

#Apply = function
  mean.sepal.length<-mean(selected.iris$value)


#Calculate the mean of each measure for each species

#Split & Apply in a function
  mean.measure<-function(Species, measure){
    selected.iris<-molten.iris[molten.iris$Species==Species & 
                                molten.iris$measure==measure,]
    
    mean.sepal.length<-mean(selected.iris$value)
    
    return(mean.sepal.length)
  }

  
#Combine = rbind

system.time({
#Split Apply Combine in a loop
  mean.data<-NULL
  for(Species in unique(molten.iris$Species)){
      for(measure in unique(molten.iris$measure)){
        
        #print(paste(Species, measure))
        
        mean.measure.temp<-mean.measure(Species, measure)
        
        mean.data.temp<-data.frame(Species=Species,
                                   measure=measure,
                                   mean=mean.measure.temp)
        
        mean.data<-rbind(mean.data, mean.data.temp)
        
      }
  
  }
}) 
#To test inside for loop
#Species="setosa"
#measure="Sepal.Length"
  





################################################################################

##
#Split-Apply-Combine:Plyr
##


# summarised.data<-ddply(.data=data,
#                        .variable=c("variable1", "variable2"),
#                        value.returned.by.function=function(arguments,...),
#                        summarise)
# 
# 
# transformed.data<-ddply(.data=data,
#                        .variable=c("variable1", "variable2"),
#                        value.returned.by.function=function(arguments,...),
#                        transform)
                       
# 
# summarised.data<-ddply(.data=data,
#                        .variable=c("variable1", "variable2"),
#                        value.returned.by.function=function(arguments,...),
#                        summarise)
# 
# 
# summarised.data<-ddply(.data=data,
#                        .variable=c("variable1", "variable2"),
#                        function(subset.data){ your function that returns a data.frame})


                       
#Calculate the mean of each measure for each species

system.time({
mean.ply<-ddply(.data=molten.iris,
                .variables=c("Species", "measure"),
                mean=mean(value),
                summarise,
                .parallel=T)
})                      


#Play ground

# summarise -> transform
mean.ply<-ddply(.data=molten.iris,
                .variables=c("Species", "measure"),
                mean=mean(value),
                transform)

# omit summarise
mean.ply<-ddply(.data=molten.iris,
                .variables=c("Species", "measure"),
                mean=mean(value))

# change function eg. calculate sd
sd.ply<-ddply(.data=molten.iris,
                .variables=c("Species", "measure"),
                sd=sd(value),
                summarise)

# write your own function

# to calculate many statistics

mean.and.sd.ply<-ddply(.data=molten.iris,
                .variables=c("Species", "measure"),
                sd=sd(value),
                mean=mean(value),
                summarise)



mean.and.sd.ply<-ddply(.data=molten.iris,
                        .variables=c("Species", "measure"),
                       function(data, ...){
                         data.frame(mean=mean(data$value, ...), sd=sd(data$value, ...))
                         })

                       

# calculate the slope and intercept for each species of Sepal.Width~Sepal.Length


lm.coef<-function(Sepal.Width,Sepal.Length){coef(lm(formula=Sepal.Width~Sepal.Length))}

slope.and.intercept.ply<-ddply(.data=iris,
                               .variables="Species",
                               intercept=lm.coef(Sepal.Width,Sepal.Length)[1],
                               slope= lm.coef(Sepal.Width,Sepal.Length)[2],
                               summarise)



slope.and.intercept.ply<-ddply(.data=iris,
                               .variables="Species",
                               function(data) {
                                 coef(lm(formula=Sepal.Width~Sepal.Length, data=data))
                                 })




###
# Go multicore
###

#install.packages(parallel)
#install.packages(doMC)
library(parallel)
library(doMC)

registerDoMC(2) # 2 cores


slope.and.intercept.ply<-ddply(.data=iris,
                               .variables="Species",
                               intercept=lm.coef(Sepal.Width,Sepal.Length)[1],
                               slope= lm.coef(Sepal.Width,Sepal.Length)[2],
                               summarise,
                               parallel=T)


###
# See progress
###

slope.and.intercept.ply<-ddply(.data=iris,
                               .variables="Species",
                               intercept=lm.coef(Sepal.Width,Sepal.Length)[1],
                               slope= lm.coef(Sepal.Width,Sepal.Length)[2],
                               summarise,
                               .progress= "text")

###
# plot plyr
###


#Plot idividual sepals and petals as rectangles for each species


#Prepare the data
molten.iris$row.names<-row.names(molten.iris)
molten.iris<-ddply(.data=molten.iris,
                   .variables="row.names",
                   part=unlist(strsplit(x=as.character(measure), split="\\."))[1],
                   dimension=unlist(strsplit(x=as.character(measure), split="\\."))[2],
                   transform)

cast.iris<-cast(data=molten.iris,
                formula=Species + id + part ~ dimension)


# Plot the data mixing ggplot2 and plyr

#setwd("~")
#pdf("iris sepal explore plot.pdf")

d_ply(.data=cast.iris,
      .variables="Species",
      function(data){
        print(qplot(data=data,
                    ymin=I(0),
                    ymax=Length,
                    xmin=I(0),
                    xmax=Width,
                    geom="rect",
                    xlim=c(-1, 10),
                    ylim=c(-1, 10),
                    facets=~id,
                    main=Species,
                    alpha=I(0.3),
                    fill=part))})

#dev.off()
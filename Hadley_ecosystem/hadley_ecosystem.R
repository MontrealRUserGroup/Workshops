##
#The Hadley Ecosystem: reshape,plyr and ggplot
#by: Etienne Low-Decarie
##

################################################################################

##
# Housekeeping
##

rm(list=ls())

##
#Load libraries (or install and load)
##
if(!require(plyr)){install.packages("plyr")}
require(plyr)

if(!require(reshape)){install.packages("reshape")}
require(reshape)

if(!require(ggplot2)){install.packages("ggplot2")}
require(ggplot2)

if(!require(vegetarian)){install.packages("vegetarian")}
require(vegetarian)

if(!require(vegan)){install.packages("vegan")}
require(vegan)

if(!require(gridExtra)){install.packages("plyr")}
require(gridExtra)



################################################################################

##
#Reshape :  Make your data play nice
##

###################
#Make you data long
###################


#How it works

# molten.data<-melt(data,
#                   id.vars=ls("id.var.1", "id.var.2"),
#                   measure.vars=ls("measure.vars", "measure.vars"),
#                   variable_name = "variable")


#Make the iris data long

#Look at iris
grid.newpage() #clear the graphic device
grid.table(head(iris), h.even.alpha=1, h.odd.alpha=0.5,  v.even.alpha=1, v.odd.alpha=1) #create a nice graphic table


  iris$id<-row.names(iris)
  
  molten.iris<-melt(iris,
                    id.vars=c("Species", "id"),
                    #measure.vars=c("measure.vars", "measure.vars"),
                    variable_name = "measure")

grid.newpage()
grid.table(head(molten.iris), h.even.alpha=1, h.odd.alpha=0.5,  v.even.alpha=1, v.odd.alpha=1)

###################
#Make you data wide
###################


#How it works

# cast.data<-cast(data,
#                 formula = x_variable_1 + x_variable_2 ~
#                   y_variable_1 + y_variable_2)


#Make the molten.iris data wide
  
cast.iris<-cast(molten.iris,
                formula = Species + id ~ ...)

#Note the default output of cast is a list not a data.frame
#Use data.frame(cast.data) or cast(..., df=T)

grid.newpage()
grid.table(head(data.frame(cast.iris)), h.even.alpha=1, h.odd.alpha=0.5,  v.even.alpha=1, v.odd.alpha=1)
          

################################################################################  

##
# Plyr:  Split-Apply-Combine
##


#How it works


# my.function<-function(subset.data){
#   results<-do.something(subset.data)
#   return(data.frame(results)}
# 
# returned.results<-__ply(.data=data,
#                         .variable=c("variable1", "variable2”),
# 		                    my.function(subset.data))

#With _dply my.function should return a data.frame
#If you can not return a data.frame, use _lply...


###################
#Example 1
###################
molten.iris.means<-ddply(.data=molten.iris,
                    .variables=c("Species", "measure"),
                    function(subset.data) data.frame(mean=mean(subset.data$value)))

grid.newpage()
grid.table(head(data.frame(molten.iris.means)), h.even.alpha=1, h.odd.alpha=0.5,  v.even.alpha=1, v.odd.alpha=1)

###################
#Example 2
###################

colwise.mean<-numcolwise(mean)  #?colwise in plyr

mean.iris<-ddply(.data=iris,
                 .variables="Species",
                 colwise.mean)

grid.newpage()
grid.table(head(data.frame(mean.iris)), h.even.alpha=1, h.odd.alpha=0.5,  v.even.alpha=1, v.odd.alpha=1)

###################
#Example 3
###################

length.on.width.slope<-function(subset.data){
  with(subset.data,{
    slope.sepal<-lm(Sepal.Width~Sepal.Length)$coefficients[2]
    slope.petal<-lm(Petal.Width~Petal.Length)$coefficients[2]
    return(data.frame(slope.sepal=slope.sepal,
                      slope.petal=slope.petal))
  })
}



iris.slopes<-ddply(.data=iris,
                   .variables="Species",
                   length.on.width.slope)


grid.newpage()
grid.table(head(data.frame(iris.slopes)), h.even.alpha=1, h.odd.alpha=0.5,  v.even.alpha=1, v.odd.alpha=1)




################################################################################  

##
# ggplot:  grammar of graphics
##

###################
#Example 1
###################

#Most basic plot

basic.plot<-qplot(data=iris,
                  x=Sepal.Length,
                  y=Sepal.Width)

print(basic.plot)


categorical.plot<-qplot(data=iris,
                  x=Species,
                  y=Sepal.Width)

print(categorical.plot)


#Edited most basic plot

basic.plot<-qplot(data=iris,
                  x=Sepal.Length,
                  xlab="Sepal Width (mm)",
                  y=Sepal.Width,
                  ylab="Sepal Length (mm)",
                  main="Sepal dimensions")

print(basic.plot)



#Add aesthetics

basic.plot<-qplot(data=iris,
                  x=Sepal.Length,
                  xlab="Sepal Width (mm)",
                  y=Sepal.Width,
                  ylab="Sepal Length (mm)",
                  main="Sepal dimensions",
                  colour=Species,
                  shape=Species,
                  alpha=I(0.5))

print(basic.plot)


#Add geoms

plot.with.line<-basic.plot+geom_line()
print(plot.with.line)

plot.with.linear.smooth<-basic.plot+geom_smooth(method="lm", se=F)
print(plot.with.linear.smooth)

plot.smooth.on.all<-basic.plot+geom_smooth(method="lm", aes(group=1))
print(plot.smooth.on.all)

plot.with.smooth.on.all.and.species<-plot.with.linear.smooth+geom_smooth(method="lm", aes(group=1))
print(plot.with.smooth.on.all.and.species)


###################
#Example 2
###################

CO2.plot<-qplot(data=CO2,
                x=conc,
                y=uptake,
                colour=Treatment)

print(CO2.plot)

CO2.plot<-CO2.plot+facet_grid(.~Type)
print(CO2.plot)


#More than one data point per x value
#Need to either specify group
print(CO2.plot+geom_line())

CO2.plot.group<-CO2.plot+geom_line(aes(group=Plant))
print(CO2.plot.group)


#Or add an extra aes
CO2.plot.shape<-qplot(data=CO2,
                x=conc,
                y=uptake,
                colour=Treatment,
                shape=Plant)+
                  facet_grid(.~Type)+
                  geom_line()+
                  scale_shape_manual(values=1:length(unique(CO2$Plant)))
print(CO2.plot.shape)

#Or calculate a statistic
CO2.plot.mean<-CO2.plot+geom_line(stat="summary", fun.y="mean", size=I(3), alpha=I(0.3))
print(CO2.plot.mean)

CO2.plot.group.mean<-CO2.plot.group+geom_line(stat="summary", fun.y="mean", size=I(3), alpha=I(0.3))
print(CO2.plot.group.mean)












################################################################################  

##
# Super user stuff
##




###
# Go multicore
###


#Install and load needed packages
if(!require(parallel)){install.packages("parallel")}
if(!require(doMC)){install.packages("doMC")}


#Tell R how many cores to use
#If this is not a remote machine, keep yourself a core to work on
registerDoMC(2) # 2 cores


iris.slopes<-ddply(.data=iris,
                   .variables="Species",
                   length.on.width.slope,
                   .parallel=T)


###
# See progress
###

iris.slopes<-ddply(.data=iris,
                   .variables="Species",
                   length.on.width.slope,
                  .progress= "text")

#Try .progress= "tk" or .progress= "win"

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
                    xlim=c(-1, 8),
                    ylim=c(-1, 8),
                    facets=~id,
                    main=unique(data$Species),
                    alpha=I(0.3),
                    fill=part))})

#graphics.off()

# You can use the "#" symbol to tell R to ingnore the text that follows
#### you can use as many "#" as you wish 
# you can also place "#" comments after R inputs, for example:
x<-0 # blablabla... my R interesting R input


##############################################################################
# Date: Tuesday, Oct. 11, 2011
# By: Insert your name (e.g. Zofia E. Taranu)
# Description: BGSA Introductory Worshop - Day 2
# Version of R used: RStudio 0.94.92
# ...

# 1.HOUSEKEEPING
# Housekeeping are functions that will ensure that your script runs smoothly.
  
# Clear R memory
rm(list=ls())   

# Removes all variables from memory 
# Prevents errors such as use of older data
  
# 2. SET WORKING DIRECTORY
# A directory is a folder and the instructions (path) to get to that folder
# The working directory is the folder from which R will read files
# and to which R will save files
setwd("~/Desktop/PhD/FALL 2011/Stats workshop - Day 2/Data_Intro Day 2")
  

# 3. DATA IMPORT
# First we will import "clean" data in a csv file

iris_data<-read.csv("iris_good.csv") 
# "iris_good.csv" is the complete file name, where ".csv" is the file's extension

# Look at...
# a. the entire data matrix (think of this as your excel spreadsheet in a way)
iris_data
iris_data$Sepal.Length
  
# b. the variables included in this dataset (column names)
names(iris_data)
  
# c. the first few lines (default is 6 lines)
head(iris_data, n = 5)     # 5 is the number of lines

# d. the structure of your data (variables, types, values)
str(iris_data)
  
# e. a summary of the data
summary(iris_data)
# note that summary has useful outputs for many generic R object types 
# (eg. anova objects)
  
# f. a default plot
plot(iris_data)
# as with "summary", plot has default outputs for many generic R object types 
  
# 4. BROKEN DATA
# Attempt to import and visualization the BROKEN data ("iris_broken")

iris_data<-read.csv("iris_broken.csv")
# Error 1: wrong extension (.txt not .csv)

iris_data<-read.csv("iris_broken.txt")
  
# Look at the data, head() and str() are
# these are most useful at the importing stage
head(iris_data)
str(iris_data)

# Error 2: rammed all the columns into one
# ie. did not recognize the seperation character
  
# Try the import again with a different separator
iris_data<-read.csv("iris_broken.txt", sep = "")

# use the "sep" argument to tell R what you have been using to separate 
# your columns (here; a TAB)
head(iris_data)
str(iris_data)
  
# Error 3: The first few lines are useless 
iris_data<-read.csv("iris_broken.txt", sep = "", skip = 4)

# Add the "skip" argument to skip a few lines
head(iris_data)
str(iris_data)
  
# Error 4: Looking at the file, we notice that our data contains uncapitalized "na" and "Forgot_this_value"
# Recall that R only recognizes "NA"
iris_data$Petal.Length
iris_data<-read.csv("iris_broken.txt", sep = "", skip = 4, na.strings = c("NA","na","Forgot_this_value"))
                      
head(iris_data)
str(iris_data)

# Error 5: Numerous variables are not numerical
class(iris_data$Sepal.Length)  # numberic   YAY!
class(iris_data$Sepal.Width)   # factor  ;(
class(iris_data$Petal.Length)  # factor  ;(
class(iris_data$Petal.Width)   # numberic   YAY!

# notice for example:
iris_data$Sepal.Width[23]  # the row 23 value was not properly entered

iris_data<-read.csv("iris_broken.txt",
                      sep="",
                      skip=4,
                      na.strings=c("NA", "na","forgot_this_value"),
                      as.is=c("Sepal.Width", "Petal.Length"))
  
# tell R to leave the two columns alone
head(iris_data)
str(iris_data)

# that was not perfect, now R thinks these should only be characters, not numeric values
# use "as.class wanted(argument)" where you can replace "class wanted"" with "numeric", "ordered" or "factor"              
iris_data$Sepal.Width <- as.numeric(iris_data$Sepal.Width)
iris_data$Petal.Length <- as.numeric(iris_data$Petal.Length)
# Notice the WARNING because NAs were introduced where non-numeric values were found   
# e.g., the 23rd Sepal Width entry has now been changed to <NA>     
iris_data$Sepal.Width[23]                    
  
# Broken data has now been fixed and loaded into R!! Big time YAY!
head(iris_data)
str(iris_data)

# 5. SAVING DATA  

# a. Saving an R file 
save(iris_data, file = "iris_cleaned.R")               

# Clear your memory
rm(list = ls())
  
# b. reload iris_data
load("iris_cleaned.R")
head(iris_data)   # looking good!
                    
# 6. EDITING DATA

# Let's work with the original dataset
rm(list=ls())
iris_data<-read.csv("iris_good.csv")

# a. apply functions (FUN) to your data using "lapply"
?apply
# 'lapply' returns a list of the same length as X, each element of which is 
# the result of applying FUN (any given function) to the corresponding element 
# of X.

# apply the "mean" function 
iris_means<-sapply(iris_data[,2:5],mean) # calling up the columns by indices
# or alternatively, you can call up the columns by name:
iris_means<-sapply(iris_data[,c("Sepal.Length",
                                "Sepal.Width",
                                "Petal.Length",
                                "Petal.Width")],mean)

# you may replace the values with normalized data by using the "scale" function
iris_data[,2:5]<-sapply(iris_data[,2:5],scale)
# or, equivalently:
iris_data[,c("Sepal.Length", "Sepal.Width","Petal.Length","Petal.Width")]<-sapply(iris_data
                                    [,c("Sepal.Length",
                                      "Sepal.Width",
                                      "Petal.Length",
                                      "Petal.Width")],scale)

# b. creating a dataframe with a new object name
# The "data.frame" command creates data frames which share many of the properties 
# of matrices.

rm(list=ls())   # to remove all the work we just did
iris_data<-read.csv("iris_good.csv")  # reload the data iris 

normalized_iris<-data.frame(iris_data["Species"],sapply(iris_data[,c("Sepal.Length",
                                               "Sepal.Width",
                                               "Petal.Length",
                                               "Petal.Width")],scale))

# where the first column of this dataframe is "Species" and the last four are the 
# normalized Sepal and Petal variables!                     

names(normalized_iris)
head(normalized_iris)
str(normalized_iris)
# c. replacing values or names in data file

# if you noticed a typo or if, for example, the taxonomy has changed and you need 
# to update some of your species names, you can use the "gsub" command
# We will substitute the name "versicolor" with "furfuracea"

?gsub
# searches for matches to the argument "pattern"" within each element of a character 
# vector and replaces this with the argument "replacement".
gsub(pattern = "versicolor",replacement = "furfuracea",x = iris_data$Species)

# Let's combine "lappy" and "gsub" to implement this change into the dataframe
iris_data<-as.data.frame(sapply(iris_data,gsub,pattern="versicolor",replacement="furfuracea"))
levels(iris_data$Species)   # seems to have worked!
head(iris_data)
str(iris_data)      # but the variables appear as factors again ;(

iris_data$Sepal.Length <- as.numeric(iris_data$Sepal.Length)
iris_data$Sepal.Width <- as.numeric(iris_data$Sepal.Width)
iris_data$Petal.Length <- as.numeric(iris_data$Petal.Length)
iris_data$Petal.Width <- as.numeric(iris_data$Petal.Width)
str(iris_data)    # ok now we're good!

# d. subsetting and sorting dataframes
iris_sub<-subset(iris_data,Sepal.Length>4.4,select=c(Sepal.Length,Species))     
head(iris_sub)      
# where we've only kept rows with Sepal.Length greater than 4.4 and "select" 
# alowed us to choose columns of intereste (i.e., "Sepal Length" and "Species")  

#                                    
iris_sub<-subset(iris_data,Sepal.Length > median(Sepal.Length) & Species == "virginica")
# OR
iris_sub<-iris_data[iris_data$Sepal.Length > median(iris_data$Sepal.Length) & iris_data$Species == "virginica",]                                     

# e. ordering the data                                
iris_ord<-iris_data[order(iris_data$Species,iris_data$Sepal.Width),]
# Where we've first ordered by "Species" and then by "Sepal Width"    
 
# 7. EXPORTING DATA

# a. recall our normalized iris data, now save it to a .csv file:
write.table(normalized_iris, file="normalized_iris.csv", sep = ",")
              
plot(iris_data$Sepal.Length, normalized_iris$Sepal.Length, xlab = "Sepal Length", 
     ylab = "Normal. Sepal Length", pch = 19, col = "cadetblue")                                     
                                                             
# b. saving PDF figures
  
rm(list=ls())                # clear history
install.packages("foreign")  # installing a package                                     
library(foreign)             # load libraries

# Note, you can create a new folder called "Figures", then reset your working directory 
# so that your figures are saved there!

iris_data<-read.csv("iris_good.csv")                                     
setwd("~/Desktop/PhD/FALL 2011/Stats workshop - Day 2/Figures_Intro Day 2")
                                     
# c. saving a PDF of the plot
pdf("iris_plot.pdf")   # Opens a PDF in your Figures folder
plot(iris_data)        # Creates the figure
dev.off()              # Closes the PDF file


# 7. DEALING WITH THE BROKEN WITH THE BROKEN CO2 DATA!
rm(list=ls())
setwd("~/Desktop/PhD/FALL 2011/Stats workshop - Day 2/Data_Intro Day 2")  
CO2_data<-read.csv("CO2_broken.csv") 

# look at the data
head(CO2_data)
str(CO2_data)

# Error 1: the two first lines are just random notes about some missing values
# in the Quebec subset 
CO2_data<-read.csv("CO2_broken.csv", skip = 2)
head(CO2_data)
str(CO2_data)

# Error 2: take care of NAs; Let's look at the Quebec data                                                             
CO2_sub_QC<-subset(CO2_data, CO2_data$Type == "Quebec", select=c(conc, uptake))

# replace the "cannot_read_notes" to proper NAs
CO2_data<-read.csv("CO2_broken.csv", skip = 2, na.string = c("cannot_read_notes"))
head(CO2_data)
str(CO2_data)
                                                            
# Looks like we have levels called "chiled" instead of "chilled" and "nnchilled"
# instead of "nonchilled": typos!
levels(CO2_data$Treatment)

# Error 3: take care of typos in the Treatment variable                                                             
CO2_data<-as.data.frame(sapply(CO2_data,gsub,pattern="chiled",replacement="chilled"))
CO2_data<-as.data.frame(sapply(CO2_data,gsub,pattern="nnchilled",replacement="nonchilled"))
levels(CO2_data$Treatment)

str(CO2_data)

# One more problem, the "conc" and "uptake" variables appear as factors

# Error 4: change thes variables from factor to numeric 
CO2_data$conc<-as.numeric(CO2_data$conc)
CO2_data$uptake<-as.numeric(CO2_data$uptake)

str(CO2_data)

# All done!!
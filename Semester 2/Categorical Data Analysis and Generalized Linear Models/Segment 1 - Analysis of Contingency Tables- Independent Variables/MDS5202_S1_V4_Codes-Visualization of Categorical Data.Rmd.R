# A survey asks people if they smoke or not. The data is
 # Yes, No, No, Yes, Yes
#We can enter this into R with the c() command, and summarize with the table command as follows

x=c("Yes","No","No","Yes","Yes")
table(x)


########_Factor_########


x=c("Yes","No","No","Yes","Yes")
x
factor(x)  



#######_Bar Charts_######## 

# a group of 25 people are surveyed as to their coffee-drinking preference. The categories were (1) Latte, 
# (2) Cappuccino , (3) Espresso and (4) cold brew 
coffee = c(3, 4, 1, 1, 3, 4, 3, 3, 1, 3, 2, 1, 2, 1, 2, 3, 2, 3, 1, 1, 1, 1, 4, 3, 1)

barplot(coffee)

barplot(table(coffee)) # call with summarized data
barplot(table(coffee)/length(coffee))  # divide by n for proportion

#####################
library(vcd)
Arthritis
head(Arthritis)
counts <- table(Arthritis$Improved)
counts

 
barplot(counts, 
         main="Simple Bar Plot", 
         xlab="Improvement", ylab="Frequency") 
 
 
 barplot(counts, 
         main="Horizontal Bar Plot", 
         xlab="Frequency", ylab="Improvement", 
         horiz=TRUE)
 
 
#########_Pie Charts_#########
 coffee.counts = table(coffee) 
 coffee.counts # store the table result
pie(coffee.counts)

names(coffee.counts) = c("Latte","Cappuccino","Espresso","cold\n brew")
pie(coffee.counts)

pie(coffee.counts,col=c("purple","green2","cyan","white"))

##########_Histogram_###########

mtcars
head(mtcars)
?mtcars
par(mfrow=c(2,2))
hist(mtcars$mpg) 
hist(mtcars$mpg, breaks=12, col="red",  xlab="Miles Per Gallon", main="Colored histogram with 12 bins")
hist(mtcars$mpg, freq=FALSE,breaks=12,col="red",xlab="Miles Per Gallon",main="Histogram, rug plot, density curve")
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg), col="blue", lwd=2)

############ kernel density plot ##############
par(mfrow=c(2,1))
d <- density(mtcars$mpg) 
plot(d) 
plot(d, main="Kernel Density of Miles Per Gallon") 
polygon(d, col="red", border="blue") 

######_Making Numerical data categorical #########

# Categorical variables can come from numeric variables by aggregating values. For example. The test marks could
# be placed into broad categories of 0-40, 40-60 and over 60. To do this using R one uses the cut()
#function and the table() function


marks= c(23, 45, 65, 21,59, 70, 56, 80,43,22)
cat = cut(marks, breaks  = c(0,40,60,max(marks)))
cat
levels(cat) = c("failed", " 2 division", "1 division")
table(cat)



######## Bivariate Data ############

# Suppose a student survey is done to evaluate if students who smoke study less.
# amount of studying :- "less than one hour"- "1", "5-10 year" - "2", "more than 10 years" - "3"

smokes = c("Y","N","N","Y","N","Y","Y","Y","N","Y")
amount = c(1,2,2,3,3,1,2,1,3,2)
table(smokes,amount)




barplot(table(smokes,amount))
barplot(table(amount,smokes))



#################################################
 data("Arthritis")
head(Arthritis)

#You can generate simple frequency counts using the table() function. Here’s an example:

mytable <- with(Arthritis, table(Improved))
 mytable
 # You can turn these frequencies into proportions with prop.table()
prop.table(mytable) 
 
#or into percentages, using prop.table()*100 
prop.table(mytable)*100
 
#For two-way tables, the format for the table() function is 
mytable <- xtabs(~ Treatment+Improved, data=Arthritis)
mytable


# You can generate marginal frequencies and proportions using the margin.table() 

 margin.table(mytable, 1)

 margin.table(mytable, 2)
 
# You can use the addmargins() function to add marginal sums to these tables.
 
 addmargins(mytable)


# chi square test ##
H0:
H1:  
 mytable <- xtabs(~Treatment+Improved, data=Arthritis)
 chisq.test(mytable)
 
 mytable <- xtabs(~Improved+Sex, data=Arthritis) 
 chisq.test(mytable)
 
 
 
# FISHER’S EXACT TEST ##

  mytable <- xtabs(~Improved+Sex, data=Arthritis)
  fisher.test(mytable)
  
  
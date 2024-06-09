#Suppose we want to analyse the productivity of 5 kind on fertilizer, 5 kind of tillage, and 5 kind of seed
#The three factors are: fertilizer (fertilizer1:5), tillage (treatA:E), seed (A:E). 
#The numbers are the productivity in cwt / year(centum weight or quintal)
#The data are organized in a latin square design, as follows
#               treatA  treatB  treatC  treatD  treatE
#   fertilizer1  "A42"   "C47"   "B55"   "D51"   "E44"         
#   fertilizer2  "E45"   "B54"   "C52"   "A44"   "D50"         
#   fertilizer3  "C41"   "A46"   "D57"   "E47"   "B48"         
#   fertilizer4  "B56"   "D52"   "E49"   "C50"   "A43"         
#   fertilizer5  "D47"   "E49"   "A45"   "B54"   "C46"

#Now create a dataframe in R with these data:
fertil <- c(rep("fertil1",1), rep("fertil2",1), rep("fertil3",1), rep("fertil4",1), rep("fertil5",1))
treat <- c(rep("treatA",5), rep("treatB",5), rep("treatC",5), rep("treatD",5), rep("treatE",5))
seed <- c("A","E","C","B","D", "C","B","A","D","E", "B","C","D","E","A", "D","A","E","C","B", "E","D","B","A","C")
freq <- c(42,45,41,56,47, 47,54,46,52,49, 55,52,57,49,45, 51,44,47,50,54, 44,50,48,43,46)

mydata <- data.frame(treat, fertil, seed, freq)
mydata

#We can re-create the original table, using the matrix function:
matrix(mydata$seed, 5,5)

matrix(mydata$freq, 5,5)

#The ANOVA table
myfit <- lm(freq ~ fertil+treat+seed, mydata)
anova(myfit)

#The difference between group considering the fertilizer is not significant (p-value > 0.1)
#The difference between group considering the tillage is quite significant (p-value < 0.05)
#The difference between group considering the seed is very significant (p-value < 0.001)

library(agricolae)
LSD.test(y = myfit,
         trt = "seed",
         DFerror = myfit$df.residual,
         MSerror = deviance(myfit)/myfit$df.residual,
         alpha = 0.05,
         group = TRUE,
         console = TRUE)

#The results showed that seed varieties B and D are statistically more productivity than C,E and A


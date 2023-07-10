### SETTING THE SYSTEM working directory

setwd(dir='C:/Users/Samuel Effiong/Desktop/statistical computing 2 .. sta 418/leture 1')

#### confirm the  SYSTEM working directory
getwd()


## clear all old environmental variables
rm(list=ls())
#=======================================================

# VECTOR AND SOLUTIONS
# In r programming , a vector is a container for data element of the same basic type of sequence.
# this element are called COMPONENT SEQUENCE
#c()

# LENGHT VECTOR : IT IS USE TO MEASURE THE LENGHT 
# length()

# TYPES OF VECTOR 
#the are 3 types c(),seq(), rep()

# eg: 
a = rep(1:2,c(10,10))
a1 = rep(1:2, each =10)

a2 = rep(1:10)

a3 = rep(1:4,4)

a3 =c(8,9,12)
a4 = rep(a3, 5)

a5 = rep(a3,1:3) # take accenting order up

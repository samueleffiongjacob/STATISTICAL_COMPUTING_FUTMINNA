### SETTING THE SYSTEM working directory

setwd(dir='C:/Users/Samuel Effiong/Desktop/statistical computing 2 .. sta 418/leture 1')

#### confirm the  SYSTEM working directory
getwd()

## clear all old environmental variables
rm(list=ls())


### R INTODUCTION

# Assignments in R

x = 32
v <- 34

z = x+v
z

## Arithmetic operation

a = 5 *6
a

b = 1+5*3
b

### FIRST SESSION 
#COLUMN VECTORS

e <- c (1,2,3,4,5,6)
e

## how to get help

help("assign")

assign("t", c(1,2,3,4,5,6))
t
#the assign would do the same job as 

tt <- c (1,2,3,4,5,6)


# reprocal

qq <- 1/t
qq

matrix <- matrix(x,3,2)
matrix

# defining matrix in a rowwise direction 

matrix <- matrix(x,3,2, byrow =TRUE)
matrix

# it could also befine as

da = matrix(c(1,2,3,4,5,6),3,2, byrow = TRUE)
da

ad = matrix(x,3)
ad

dda = matrix(x,,3)
dda

#Vector arithmetic

# inbuilt functions in : log, exp, sin, cos, tan, sqrt,max,min,range, length,sum,prod

# Sequence command
seq(1,6,1)
# concept of above work like (from , to , by , length, along)

seq(1,6,0.5)

# sequence short cort
qqq = 1:6
qqq

#same as 
seq(1,6)

seq(6,1,-1)
ddd = 6:1
ddd


############# LIST

country <- c("SA", "ZIM", "NAM")
POLULATATION <- c(43,55,40)
INTRATE <- c(12,200,15)
sta <- list(coun= country, pop = POLULATATION,int=INTRATE)
sta

### call function from list with $

sta$coun
sta$pop

## it very usefully for statistical analysis


###rnormal .
?rnorm #know about it

rwt <- matrix(rnorm(9),3,3)

#SVD =====  singular value decomposition

svd(rwt)
rwt

## list and data frame can be attach and detach
attach(sta)
max(pop)
var(pop)
range(pop)


detach(sta)
max(pop)

max(sta$pop)


#The Replicate Command (rep)

y = c(23,45,32,223)
z = rep(y, times =5)
z

# 0r 

zz= rep(1:3,2:4)
zz

# error 
z1 = rep(1:3,2:5)
z1

# length in rep

ds1 = rep(1:3, length=8)
ds1


###ACCESSING DIFFRENT ELEMENT OF A MATRIX/VECTOR


u <- 1:10
u
u[2]
u[,1]


u1 <- matrix(rnorm(9),3,3)
u1
u1[,1] ## column wisely
u1[1,] ## row wisely
u1[1:2] ## pick column 1 and 2 rows in column 1
u1[1,2] # pick row column 2 in row 1
u1[,2:1] # skip to column 2 and go back to 1 or start counting from 2
u1[,3:1] # skip to column 3 and go back to 1 or start counting from 3


# logical vector
asa  <- 18
ast <- asa>2
ast

asa1  <- 1:5
ast1 <- asa1 > 2
ast1

asa2  <- 18
ast2 <- asa<2
ast2



# Arithmetic operation with logical vectors

rt <- asa1[asa1>2]
rt
# the above is restricted set to greater than 2

rt1 <- sum(asa1>2)
rt1
# the above simply means that 3 element of asa1 is greater than 2

rt2 <- sum(asa1[asa1>2])
rt2
# the above calculate the sum greater than 2


## entering and importing data
#Some useful numerical functions

#. round
rtr8 <- round(-1.5)
rtr8

# signif
rtr9 <- signif(rtr8)
rtr9

# floor
rtr10 <- floor(2.3)
rtr10

# ceiling.
rtr11 <- ceiling(2.3)
rtr11

# trunc :’ takes a single numeric argument ‘x’ and returns a numeric vector containing the
#integers by truncating the values in ‘x’ toward ‘0’. backward approximation
rtr12 <- trunc(2.3)
rtr12

#. abs.
rtr13 <- abs(23)
rtr13

#. exp....
rtr14 <- exp(80)
rtr14

#gamma
rtr15 <- gamma(80)
rtr15

#. lgamma.
rtr16 <- lgamma(80)
rtr16

#log.
rtr17 <- log(80)
rtr17 

#log10
rtr18 <- log10(80)
rtr18

# sign.
rtr19 <- sign(80)
rtr19

#  sqrt.
rtr20 <- sqrt(80)
rtr20

# All trig functions as usual (i.e. sin, asin, asinh for sine, arc sin and arc hyperbolic sine)
#importing with scan function

#for sine
trig <- sin(67)
trig

trig1 <- asin(56)
trig1

trig2 <- asinh(70)
trig2




plank <- scan("C:/Users/Samuel Effiong/Desktop/statistical computing 2 .. sta 418/job refrence/pokemon.txt", skip = 1:4, nlines = 7)
plank

plank1 <- ts(read.csv("C:/Users/Samuel Effiong/Desktop/statistical computing 2 .. sta 418/job refrence/pokemon.csv", header = TRUE, sep = ","))
plank1
?ts


library() # to see avaible library or package

data()

# TIME SERIES .. before we use we have to install

#installing time series
#install.packages("tseries", dependencies = TRUE)
#install.packages("ts")


# loading time series package 
#library(tseries)
#library(xts)


# Time series 

time <- ts(JohnsonJohnson, frequency = 4, start = c(1959, 2)) # 2nd Quarter of 1959
time
print( ts(1:10, frequency = 7, start = c(12, 2)), calendar = TRUE)
# print.ts(.)



#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/ts



#entering data using scan()

age <- scan()

##PRESIDENTS’ AGES DATA SET before u copy number remove #
#57 61 57 57 58 57 61 54 68 51 49 64 50 48 65 52 56 46 54 49 51 47 55 55  
#54 42 51 56 55 51 54 51 60 62 43 55 56 61 52 69 64 46 54

# view the 16th president in age

age[16]

# FREQUENCY TABLES
boundaries0 <- table(age)
boundaries0

boundaries = seq(34.5, 69.5, by=5) 
boundaries


boundaries1 <-  table(cut(ages, boundaries))
boundaries1

boundaries2 <-  table(cut(ages, c(boundaries, Inf)))
boundaries2

# RELATIVE FREQUENCY TABLES
boundaries3 <- length(age)
boundaries3

boundaries4 <- table(cut(ages, boundaries)) / 43
boundaries4


# r plot

# BARPLOT
me<- barplot(age)
me

#histogram
me1 <- hist(ages)
me1

me2 <- hist(age, ylab="x", xlab="y", main = "new")
me2

me3 <- hist(age, breaks=boundaries, ylab="x", xlab="y", main = "new")
me3


quit()

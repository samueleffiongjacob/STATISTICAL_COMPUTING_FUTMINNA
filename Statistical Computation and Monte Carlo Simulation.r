### SETTING THE SYSTEM working directory

setwd(dir='C:/Users/Samuel Effiong/Desktop/statistical computing 2 .. sta 418/leture 1')

#### confirm the  SYSTEM working directory
getwd()


## clear all old environmental variables
rm(list=ls())
#=======================================================
# Useful package for current .
# install.packages("wavethresh")
library(wavethresh)
library(ggplot2)
knitr::opts_chunk$set(echo = FALSE)
#================================================================
                              #  CLASS WORK
# Use R as you would a calculator to find numeric answers to the following
#================================================================
# variable for question 1.1
clss <- 0
clss1 <- 1
clss2 <- 2
clss3 <- 3
clss4 <- 4
#================================================================
#                   question 1.1a
#a. 2+ 2(3 + 4)

solution_a <- clss2 + clss2 * (clss3 + clss4)
solution_a

#================================================================
#                   question 1.1b
#b. 4^3 + 3(2 + 1)

solution_b <- clss4^clss3 + clss3 * (clss2 + clss1)
solution_b

#================================================================
#           question 1.1c

#c. sq-rt(4 + 3)(2 + 1)

solution_c <-  (clss4 + clss3) * (clss2 + clss1)
solution_c
solution_c1 <-  sqrt(solution_c)
solution_c1


#================================================================
#                 question 1.1d
#d.((1+2)/(3+4))^4

solution_d <- ((clss1+clss2)/(clss3+clss4))^clss4
solution_d


#================================================================

              #  values for question 1.2

# 1.2 Use R to compute the following
rat <- 0.2
rat1 <- 0.25
rat2 <- 2.3
rat3 <- 1 
rat4 <- 4
rat5 <- 5
rat6 <- 6
rat7 <- 7
rat8 <- 100

#================================================================
#           question 1.2a

solution_2.1 <- rat1-rat 
solution_2.1a <- sqrt(rat * (rat3-rat)/rat8)
solution_2.1ab <- solution_2.1 / solution_2.1a
solution_2.1ab
#===================================================================================
#           question 1.2b

solution2.2 <- rat3 + rat2^rat4
solution2.2a <- rat5 / rat6 -rat7
solution2.2ab <- solution2.2 /solution2.2a
solution2.2ab
#================================================================
#                   question 1.3

# Assigning the numbers 2 through 5 to variables
num1 <- 2
num2 <- 3
num3 <- 4
num4 <- 5

# Multiplying all the values
solution1.3 <- num1 * num2 * num3 * num4
solution1.3 


#or  Printing the result
print(solution1.3 )

#================================================================
#                         question 1.3
# Step 1: Enter the data into a data vector x
pig <- c(254108)
pig1 <-  2
pig2 <- 6
pig3 <- 9

# Step 2: Find the square of each number
pig_squared <- pig^pig1
pig_squared 

# Step 3: Subtract 6 from each number
pig_minus_6 <- pig - pig2
pig_minus_6

# Step 4: Subtract 9 from each number and then square the answers
pig_minus_9_squared <- (pig - pig3)^pig1
pig_minus_9_squared

#================================================================
                  #BIVARIATE CATEGORICAL DATA


#first way to perform bivarete analysis perform row blinding
Bivariate <- rbind(c(56,8), c(2,16))
Bivariate
#secondly perform column blinding

Bivariate1 <- cbind(c(56,2), c(8,16))
Bivariate1

                # second way to perform bivarate analysis and prefer
Seatbelts <- matrix(c(56,2,8,16), nrow=2, ncol =2)
Seatbelts1 <- addmargins(Seatbelts)
row_name <- c("Buckled", "unbuckled", "child")
col_names <- c("Buckled", "unbuckled", "parent")

dimnames(Seatbelts1) <- list(row_name,col_names )
Seatbelts1
#================================================================
                                  #Functions

# f(n) such that = {         n            when n < 2
#                      f(n-1) + f(n-2)    when n ≥ 2


#R line code to perform function

f <- function(n){
  if (n<2) return(n)
  return(f(n-1) + f(n-2))
}

# Using it on first 0:10 arguments
sapply(0:10, f)
#==========================================================
# Using it on first 11:20 arguments
sapply(10:20, f)


#================================================================
#           Functions: nonlinear model

#        N[t1] = N[t] exp [r(1-N[t]/k)]

ricker <- function(nzero,r, k=1,time=100,from=0,to=time){
  N <- numeric(time + 1)
  N[1] <- nzero
  for (i in 1:time) N[i +1] <- N[i]*exp(r*(1-N[i]/k))
  Time <- 0:time
  plot(Time,N,type="l",xlim=c(from,to))
}


eg1 <- layout(matrix(1:3, 3, 1))
eg1

eg2 <- ricker(0.1, 1); title("r = 1")
eg3 <- ricker(0.1, 2); title("r = 2")
eg4 <- ricker(0.1, 3); title("r = 3")


#=====================================
                  # from here my code
ricker <- function(length, scale) {
  t <- seq(-(length-1)/2, (length-1)/2)
  eg1 <- (2 / (sqrt(3 * scale) * pi^(1/4))) * (1 - (t - center)^2 / scale^2) * exp(-(t - center)^2 / (2 * scale^2))
  return(eg1)
  plot(eg1, type = "l", main = "Ricker")
}
scale <- 10
center <- 0
length <- 100

eg1 <- layout(matrix(1:3, 3, 1))
eg1
eg2 <- ricker(0.1, 1); title("r = 1")
eg3 <- ricker(0.1, 2); title("r = 2")
eg4 <- ricker(0.1, 3); title("r = 3")
eg2
eg3
eg4
#=====================================================================

ricker <- function(length, scale, center) {
  t <- seq(-(length-1)/2, (length-1)/2)
  wavelet <- (2 / (sqrt(3 * scale) * pi^(1/4))) * (1 - (t - center)^2 / scale^2) * exp(-(t - center)^2 / (2 * scale^2))
  return(wavelet)
}

# Define the parameters
scale <- 10
center <- 0
length <- 100

# Generate the Ricker wavelet
wavelet <- ricker(length, scale, center)

# Plot the wavelet
plot(wavelet, type = "l", main = "Ricker Wavelet")
#=============================================================


# Define the function for the nonlinear model
nonlinear_model <- function(N, r, k) {
  N * exp(r * (1 - N/k))
}

# Define the initial conditions and parameters
N0 <- 1  # Initial population size
r <- 0.2  # Growth rate
k <- 10  # Carrying capacity
time <- seq(0, 10, by = 0.1)  # Time points for simulation

# Simulate the population growth using the nonlinear model
population <- numeric(length(time))
population[1] <- N0

for (i in 2:length(time)) {
  population[i] <- nonlinear_model(population[i-1], r, k)
}

# Plot the population growth
plot(time, population, type = "l", xlab = "Time", ylab = "Population Size", main = "Nonlinear Model")
##################################################################################################

                      # SIMULATION IN R
#################
#  Ex 1 Compare for instance the following output:
vecpoisson <-  rpois(100,5)
mean0 <- mean(vecpoisson)
var0 <- var(vecpoisson)


#EX1a  If you do this severally, you get many different values. Why don’t you get
#the same answer every time?
set.seed(911)
vecpoisson <- rpois(100,5)
mean1 <- mean(vecpoisson)
var1 <- var(vecpoisson)

set.seed(911)
vecpoisson <- rpois(100,5)
mean2 <- mean(vecpoisson)
var2 <- var(vecpoisson)



###################################################
                        #### USING THE APPLY FUNCTION
reps <- 50000
nexps <- 5
rate <- 0.1
set.seed(0)
system.time(x1 <- replicate(reps, sum(rexp(n = nexps, rate = rate))))
head(x1)
#############################################################
                        ###FUNCTION APPLY() CONT.....

# To confirm that this is correct, we can make a histogram or a qqplot of the
#simulation and compare it to what we know is the truth

############################# LATEST WAY TO WRITE DENSITY
suppressWarnings({
  ggplot(data.frame(x1), aes(x1))+
    geom_histogram(aes(y = after_stat(density)))+
    stat_function(fun = function(x)dgamma(x, shape = nexps, scale =1/rate), bins = 30 ,color = "red", size = 2)
  
})

###       b simple apply OR set.seed(0)
system.time(x1 <- sapply(1 :reps, function(i)sum(rexp(n = nexps, rate = rate)))) 


########### BEFORE WAY
suppressWarnings({
  ggplot(data.frame(x1), aes(x1))+
    geom_histogram(aes(y = ..density..))+
    stat_function(fun = function(x)dgamma(x, shape = nexps, scale =1/rate), bins = 30 ,color = "red", size = 2)
  
})

###       b simple apply OR set.seed(0)
system.time(x1 <- sapply(1 :reps, function(i)sum(rexp(n = nexps, rate = rate)))) 


              ##################       HOME WORK

#                   Q1 
# Set the number of simulations
reps <- 1000000

# Generate the random samples
U1 <- runif(reps)
U2 <- runif(reps)
U3 <- runif(reps)

# Calculate the maximum M for each simulation
M <- pmax(U1, U2, U3)

# Estimate the probability P(M > 0.75)
probability <- sum(M > 0.75) / reps

# Output the estimated probability
probability

#===============================  
                    #Q2
# Set the number of simulations
reps <- 100000

# Set the target probability
target_probability <- 0.25

# Initialize n and probability
n <- 1
probability <- 0

# Simulate until the estimated probability reaches the target
while (probability < target_probability) {
  # Generate n standard normal observations
  Z <- rnorm(reps * n)
  Z <- matrix(Z, ncol = reps)
  
  # Calculate the maximum Z for each simulation
  Z_max <- apply(Z, 2, max)
  
  # Estimate the probability P(Z(n) > 4)
  probability <- mean(Z_max > 4)
  
  # Increment n
  n <- n + 1
}

# Output the estimated n

estimated_n <- n - 1
estimated_n
#=============================================
                      #Q3

# Set the number of simulations
reps <- 100000

# Set the parameter λ
reps1 <- 0.10

# Set the sample size
n <- 21

# Initialize the count of cases where sample mean > sample median
count <- 0

# Perform simulations
for (i in 1:reps) {
  # Generate Poisson random variables
  X <- rpois(n, reps1)
  
  # Calculate sample mean and sample median
  sample_mean <- mean(X)
  sample_median <- median(X)
  
  # Check if sample mean is greater than sample median
  if (sample_mean > sample_median) {
    count <- count + 1
  }
}

# Estimate the probability
probability <- count / reps

# Output the estimated probability
probability


#==================================================================
                        #Q4

# Set the target variance
target_variance <- 0.4

# Define the objective function to minimize
objective_function <- function(p) {
  X <- p * rnorm(1000) + (1 - p) * runif(1000)
  var_X <- var(X)
  abs(var_X - target_variance)
}

# Set the initial value for p
initial_p <- 0.5

# Optimize the objective function
view <- optim(initial_p, objective_function)

# Extract the estimated value of p
estimated_p <- view$par


# Output the estimated p
estimated_p


                    #####  Generating Random Mixtures of Normal Data


sampa=rnorm(1000000,0,1)
sampb=rnorm(1500000,3,1)
combined = c(sampa, sampb)

plt = ggplot(data.frame(combined), aes(x = combined)) + stat_bin(binwidth = 0.25, position = "identity")
plt


#Questions a to determine number of modes

# Plot the histogram
modes<- hist(combined, breaks = 100)
modes

## yes it is surprising


# Question b Now plot the density using geom_density().
plt <- ggplot(data.frame(combined), aes(x = combined)) +
  geom_density()
plt

# Question c solution
#Both plots can be used to analyze the modes in the data
#================================================================

                                      #TRY AT HOME WORK

##### making 3,000,000 with means separated by 1S
# Set the number of points
try <- 3000000

# Generate the first group with mean 0 and variance 1
group1 <- rnorm(try, 0, 1)

# Generate the second group with mean 1 and variance 2
group2 <- rnorm(try, 1, sqrt(2))

# Combine the groups into a single dataset
combined <- c(group1, group2)
combined

##############################################
#Q2
# Generate the first group with mean 0 and variance 1
pop1 <- rnorm(2000000)

# Generate the second group with mean 1 and variance 2
pop2 <- rnorm(1000000, 1, 2)

# Combine the groups into a single dataset
combined <- c(pop1, pop2)

# Create the labels for the different groups
labels <- rep(c("combined", "pop1", "pop2"), c(3000000, 2000000, 1000000))

# Create a data frame with the combined dataset and labels
df <- data.frame(data = c(combined, pop1, pop2), labels = labels)

# Create the plot using ggplot
plt <- ggplot(df, aes(x = data)) +
  geom_histogram(binwidth = 0.25, position = "identity") +
  facet_wrap(~labels, ncol = 1)

# Display the plot
plt
#########
            ######### GENERATIONG NORMAL RANDOM VARIABLES
#We will warm up by generating some random normal variables. Generate 1000 samples from the N(0, 1) distribution
#q1
samples <- rnorm(1000, 0, 1)
samples
# Generate 1000 samples from the N(0, 1) distribution
samples <- rnorm(1000, 0, 1)

###############
#Question 5 Check that these are from N(0, 1) using a quantile-quantile plot
#(Q-Q plot). Use the statqq() function in the ggplot2 package.

# Create a Q-Q plot using ggplot2
qq_plot <- ggplot(data.frame(samples), aes(sample = samples)) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Q-Q PLOT")

# Display the Q-Q plot
qq_plot

################################################################
# Create some example data mystery_samples
x <- 1:10
y <- letters[1:10]
mystery_samples <- data.frame(x, y)

# Save the objects into an .RData file
save(x, y, mystery_samples, file = "data.RData")

load("data.RData")
head(mystery_samples)

# method load from previous save Now load data that has been sampled from a "mystery" distribution.
data <- load("C:/Users/Samuel Effiong/Desktop/statistical computing 2 .. sta 418/pratical resource/.RData")
head(data)

####################
                  # Probability Calculations cont
## normal distribution
x <- c(-2, -2,0,1,2)
x
normal <- pnorm(x)
normal

#Binomial distribution
x <- c(0,1,2,5,8,10,15,20)
binomial <- pbinom(x,size=20,prob=.2)
binomial

##### poisson distribution

poisson  <- ppois(x,6)
poisson 


            ##### Probability Calculations cont.

#Q2  Calculate the following probabilities : Probability that a normal random variable
#with mean 22 and variance 25

mean <- 22
variance <- 25
x <- 24
probability_density <- dnorm(x, mean, sqrt(variance))
probability_density

    #Q2i 

# lies between 16.2 and 27.5
between <- pnorm(27.5, 22, sd= 5)-pnorm(16.2, 22, sd=5)
between

# is greater than 29
greater <- 1-pnorm(29,22,sd=5)
greater

# ls less than 17
less <- pnorm(17, 22, sd=5)
less

# ls less than 15 or greater than 25
less_greater <- pnorm(15,22,sd=5)+1-pnorm(25,22,sd=5)
less_greater

        # q3 Probability that in 60 tosses of a fair coin the head comes up

#In a fair coin toss, the probability of getting a head (H) or a tail (T) is 0.5 each. To calculate the probability of getting a specific number of heads (r) in 60 tosses, we can use the binomial distribution.

#The binomial distribution formula is given by:
  
  P(r) = C(n, r) * p^r * q^(n-r)

#Where:
# P(r) is the probability of getting r heads,
# C(n, r) is the number of ways to choose r items from a set of n items (also known as the binomial coefficient),
# p is the probability of getting a head (0.5 in this case),
# q is the probability of getting a tail (1 - p, which is also 0.5), and
# n is the total number of coin tosses (60 in this case).

#The binomial coefficient can be calculated as:
  
#  C(n, r) = n! / (r! * (n-r)!)

#Now, let's calculate the probability of getting r heads in 60 tosses for a fair coin.

P(r) = C(60, r) * 0.5^r * 0.5^(60-r)

#Please let me know the specific value of r you would like to calculate, and I'll provide you with the corresponding probability.

#Q3i prob 20,25 0r 30 times
prob <- sum(dbinom(c(20,25,30),60, prob = 0.5))
prob

# less than  20 times
prob_ii <- pbinom(19,60,prob=0.5)
prob_ii


# between 20 and 30 times
prob_iii <- pbinom(30,60,prob = 0.5)-pbinom(20,60,prob=0.5)
prob_iii


# Q4 A random variable X has Poisson distribution with mean 7. Find the probability

#X is less than or equal ls
X_is_ess_than_or_equal_ls <- ppois(5,7)
X_is_ess_than_or_equal_ls

less_than_or_equal <- ppois(4,7)
less_than_or_equal

#X is great than 10(strictly) 
X_is_great_than_10 <- 1-ppois(10,7)
X_is_great_than_10

# X is between 4 and 16 
X_is_between_4_and_16 <- ppois(16,7)-ppois(3,7)
X_is_between_4_and_16 


#Quantiles The following examples show how to common the quantiles of some
#common distributions for a given probability (or a number between 0 and 1).

# Normal(0,1) Distribution

Normal_Distribution <- c(.01,.05 ,.1, .2, .5, .8, .95, .98)
Normal_Distribution_cal <- qnorm(Normal_Distribution,mean = 0,sd=1)
Normal_Distribution_cal

#Binomial (n,p) Distribution
Binomial_Distribution <- c(.01,.05 ,.1, .2, .5, .8, .95, .98)
Binomial_Distribution_cal <- qbinom(Binomial_Distribution, size=30,prob=.2)
Binomial_Distribution_cal

# Poisson (λ ) Distribution
Poisson_Distribution <- c(.01,.05 ,.1, .2, .5, .8, .95, .98)
Poisson_Distribution_cal <- qpois(Poisson_Distribution ,6)
Poisson_Distribution_cal

#=======================================================
                  #Sample Skewness and Kurtosis

#A function R code to compute the sample skewness follows
skewness = function(x){
  n = length(x)
  z= (x-sum(x)) /sd(x)*(z**3)/n
}

###################################################
      # NORMAL DISTRIBUTION STATISTICAL INFERENCE

# EXAMPLE QUSTIONS  : you take a test and score 85, which sounds great until the professor
# announces the mean score is 92 with a standard deviation of 8. Then you
# begin to wonder, how badly did you do? To answer this use the pnorm
# function in R as follows:

# Solution
inferential_sta <-  pnorm(85, mean = 92, sd = 8)
inferential_sta

# EXAMPLE 2 :This means you scored better than 19.1this question looking at the other
#end of the distribution as well using 1-pnorm.

inferential_sta2 <- 1 - pnorm(85, mean = 92, sd = 8)
inferential_sta2

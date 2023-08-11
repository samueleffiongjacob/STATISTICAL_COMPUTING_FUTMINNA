# Set the working directory to the location of the file "record" in the folder "XYZ" on the desktop
setwd(dir='C:/Users/Samuel Effiong/Desktop/statistical computing 2 .. sta 418/leture 1')
rm(list=ls())

# USEFUL LIBRARY
library(ggplot2)
#=================================================================================================================================
#                       PASS QUSTION 2019/2020 
#==========================================================================================================================
# 1a
# we can use dhyper function :accept 4 paramenter below
N <- 20 # total number of passengers
m <- 6 # number of people infected(number of successes in the population)
n <- N - m # number of failures in the population
k <- 4 # number of passengers selected from the quadrant for a PCR TEST
x <- 0 # number of successes
q <-  1 # 1 infected individual
p <- 2 # 2 infected individual
# i probability that no person is infected
no_1 <- dhyper(x, m, n , k ) 
no_1

# 1ii The probability that at least two persons are infected:
leat_2 <- 1 - (dhyper(x, m, n, k) + dhyper(q, m, n, k))
# lower.tail FALSE :allows you to compute the "upper tail" probability without minusing  the result from 1
# This will give you the probability of getting more than 2 successes (i.e., at least 3 successes).
leat_2 

# 1iii
# The probability that at most two persons among the quarantined are infected:
at_most <- dhyper(0, 6, 14, 4) + dhyper(1, 6, 14, 4) + dhyper(2, 6, 14, 4)
#sum up the probability of having 0, 1, or 2 infected person:
at_most

#1ii number of persons likely to develop the symptom if persons == 1000
likely_to_develop <- 1000 * no_1 
likely_to_develop 
#============================================================================================================================
# 1b
# Read the data from the file "record" into a data frame
data <- read.csv("record.csv")  # If the file is in CSV format
View(data)

# (i) Determine the number of students in the file
num_students <- nrow(data)
num_students


# (ii) Prepare a frequency table of gender
gender_freq <- table(data$gender)
gender_freq

# (iii) Prepare a frequency table of gender and faculty
gender_faculty_freq <- table(data$gender, data$faculty)
gender_faculty_freq

# (iv) Write R code to compute the mean age of students
mean_age <- mean(data$age)
mean_age 

# (v) Write R code to compute the mean age of students by gender
mean_age_by_gender <- tapply(data$age, data$gender, mean)
mean_age_by_gender


# (vi) Draw the histogram of monthly and density distribution of the student age
# Assuming you have the ggplot2 package installed

# Histogram of monthly 
# 1 by using normal histogram
hist(x = data$monthly, breaks = 10, col = "blue")
# do not us bin with on ggplot computing this way
#2 using ggplot
ggplot(data, aes(x = monthly)) +
  geom_histogram(bins = 10,fill = "cornflowerblue",color = "white") +
  labs(title = "Histogram of Monthly Expenditure",
       x = "Monthly Expenditure",
       y = "Frequency")


# Density distribution of student age
# 1 using plot
density_1 <- plot(density(data$age), main =  "DENSITY PLOT", col="green",xlab = "age",ylab = "Density")  #0r use ggplot blow

# 2 ggplot
ggplot(data, aes(x = age)) +
  geom_density( color="blue",alpha = 0.5,fill="black") + # only use , fill = "blue" when specified
  labs(title = "Density Distribution of Student Age",
       x = "Age",
       y = "Density")

# (vii) Compute the average monthly pocket money expended on students by parents
mean_monthly_by_parents <- mean(data$monthly / data$parent)
mean_monthly_by_parents
#========================================================================================================================================================
# q2 to simulate the possible waiting time at each traffic
# we do this by using `runif()` :  function, which generates random numbers from a uniform distribution
# accept 3 parameterbelow

n = 5 # the number of observations
min = 0 # the minimum value of the distribution (0 minutes)
max = 3 # the maximum value of the distribution (3 minutes)

set.seed(123) # for reproducibility
waiting_times <- runif(n = 5, min = 0, max = 3)
waiting_times

# q2 i Compute the total time spent stopping at the traffic lights:
total_time <- sum(waiting_times)
total_time

# What is the minimum time spent at a traffic light:
minimum_time <- min(waiting_times)
minimum_time

#q2 iv 
#  it means that the car spent approximately 6.79 minutes waiting at the 5 traffic lights combined.
#q2 v 
# it means that the shortest waiting time at any of the 5 traffic lights was approximately 0.083 minutes, or about 5 seconds.

#===============================================================================================================
#=========================================================================================================================
#q2b 

# a)  For a standard normal variate z, compute the probability that z falls between -1.96 and 1.96:
prob_between <- pnorm(1.96) - pnorm(-1.96)
prob_between

# b  Generate 5 random normal data stored as x with mean 100 and standard deviation 16:
set.seed(123) # for reproducibility
x <- rnorm(n = 5, mean = 100, sd = 16)


# bi) Compute the z-scores:
z_scores <- (x - mean(x)) / sd(x)
# or 
z_scores1 <- scale(x)

# bii Write R code to compute probability for the z-scores:
prob_z_scores <- pnorm(z_scores)
prob_z_scores

# c) How many ways to select a team of 3 players from 10 men:
ways_to_select_team <- choose(10, 3)
# or 
length(combn(10,3,simplify = F))
#OR
factorial(10)/(factorial(7) * factorial(3))

# d) Suppose that the population of adult, male black bears has weights that are approximately
# normally distributed as N(350,75). Determine the probability that a randomly observed male bear 
# weighs more than 450 pounds:
bears <- 1 - pnorm(450, mean = 350, sd = 75)
bears


#e) Assuming that a population is evenly divided on an issue having (p=0.5). 
#A random sample of size 1000 is taken. What is the probability the random sample will have
#550 or more in favor of the issue? 
sample_size <- 1000
p_population <- 0.5
x_in_favor <- 550
prob_x_or_more <- 1 - pbinom(x_in_favor - 1, size = sample_size, prob = p_population)
print(prob_x_or_more)
#=============================================================
#Q4b
#i
injury_level <- c("none","minimal","minor","major")
injury_level <- rep(injury_level,times = 1,each = 2)
seatbelt <- c("No","Yes")
seatbelt <- rep(seatbelt,times = 4)
freq <- c(65963,12813,4000,647,2642,359,303,42)
df <- data.frame(injury_level,seatbelt,freq)
cont_table <- xtabs(freq ~ seatbelt + injury_level,data = df)
cont_table

#ii 
#margin total by seat belt
seatbelt0 <- marginSums(cont_table,"seatbelt")
#margin total by injury level
seatbealt00 <- marginSums(cont_table,"injury_level")
#or do both
Seatbelts1 <- addmargins(cont_table)
Seatbelts1

#Iv
test <- chisq.test(cont_table)
test

#v
#If the p-value from the Chi-Squared test is less than your chosen significance level (often 0.05), 
#you would reject the null hypothesis that seat belt usage and injury level are independent.
#===========================================================================
# q5a

# i) Generate a sequence of integer numbers 1 to 8. Store in variable x. Then, 
# replicate x 3 times and store in p:
x <- 1:8
p <- rep(x, times = 3)

y <- c(2, 3, 3, 2)
q <- rep(x, times = y)

r <- matrix(x, nrow = 2, ncol = 4)
s <- matrix(x, nrow = 4, ncol = 2)

rs <- r %*% s  # multiplication of matrices r and s
sr <- s %*% r  # multiplication of matrices s and r


# q5b

#If you flip a fair coin 10 times, the number of heads follows a binomial distribution
#with parameters n = 10 (the number of trials) and p = 0.5 (the probability of success on each trial).
# Here is how you could calculate the probabilities you are asking for using R:
  
#i) What is the probability of exactly 4 heads?
dbinom(4, size = 10, prob = 0.5)

#ii) What is the probability of getting between 4 and 6 heads, inclusive?
pbinom(6, size = 10, prob = 0.5) - pbinom(3, size = 10, prob = 0.5)

# iii) What is the probability of getting between 2 and 4 heads, inclusive?
pbinom(4, size = 10, prob = 0.5) - pbinom(1, size = 10, prob = 0.5)


#iv) What is the probability of getting more than 7 heads?
1 - pbinom(7, size = 10, prob = 0.5)

# v) Compute E(X) and Var(X) (the expected value and variance of X, respectively). For a binomial distribution, E(X) = np and Var(X) = np(1-p):
E_X = 10 * 0.5
Var_X = 10 * 0.5 * (1 - 0.5)

print(E_X)
print(Var_X)


#q5c

#To find the probability of having seventeen or more cars crossing the bridge in a particular minute, 
# we can use the Poisson distribution, assuming that the number of cars crossing the bridge per minute 
#follows a Poisson distribution with an average rate of twelve cars.

#In R, you can calculate this probability using the ppois() function, which calculates the cumulative
#distribution function (CDF) of the Poisson distribution.

# Average rate of cars crossing the bridge per minute
lambda <- 12

# Number of cars for which we want to find the probability (seventeen or more)
x <- 17

# Calculate the probability using the Poisson CDF
probability_seventeen_or_more <- 1 - ppois(x - 1, lambda)

# Print the result
print(probability_seventeen_or_more)
#==================================================================================
#6a If X follows a uniform distribution UNIF(100,150), it means that the distance where accidents occur 
#is equally likely to be anywhere between 100 and 150 kilometers from the nearest city center.

#We can calculate the probabilities you're asking for using the punif() function in R, which gives
#the cumulative distribution function (CDF) for the uniform distribution.

#Here's how you can do it:
  
# i) P(110 <= X <= 130):

punif(130, min = 100, max = 150) - punif(110, min = 100, max = 150)

# ii) P(127 <= X <= 144)
punif(144, min = 100, max = 150) - punif(127, min = 100, max = 150)
#iii) P(X > 148):
1 - punif(148, min = 100, max = 150)

# 6b) In this scenario, the service times follow an exponential distribution with a rate of 3 customers per minute. You can use R to simulate these service times and calculate the desired quantities.

# i) To simulate the service times (in minutes) for the 10 customers:

set.seed(123)  # for reproducibility
service_times <- rexp(n = 10, rate = 3)
print(service_times)


# ii) To compute how long it would take to serve 3 customers:
  

time_to_serve_3 <- sum(service_times[1:3])
print(time_to_serve_3)


#iii) To determine the total time to completely serve all the 10 customers:
total_time <- sum(service_times)
print(total_time)

# iv) Interpret your result in (iii) if R returns 3.27510:
  
#it means that, based on this particular simulation of service times, 
#it would take approximately 3.27510 minutes to serve all 10 customers.

#q6c
# In R, you can generate random numbers from a normal distribution using the `rnorm()` function.
# If you want to find out how many of these are less than zero, you can use the `sum()` function in combination with a logical condition.

set.seed(123)  # for reproducibility

# Generate 10 random numbers from N(5, 5)
x <- rnorm(n = 10, mean = 5, sd = 5)

# Find out how many are less than zero
num_less_than_zero <- sum(x < 0)

# Print the result
print(num_less_than_zero)
#=====================================================
#To perform the operations, we'll create the "study" dataframe and then calculate the desired statistics.
#Here's how you can do it step by step in R:

# a) Create the dataframe "study"
weight <- c(150, 135, 210, 140, 130)
height <- c(65, 61, 70, 65, 69)
gender <- c("female", "female", "male", "female", "male")

study <- data.frame(Student = c("Mary", "Alice", "Tooyin", "Jude", "John"),
                    Weight = weight,
                    Height = height,
                    Gender = gender)

# b) Add row names to the dataframe
rownames(study) <- c("Mary", "Alice", "Tooyin", "Jude", "John")

# c) Compute average and variance of the weights
average_weight <- mean(study$Weight)
variance_weight <- var(study$Weight)

# di) Compute the average height by gender
average_height_by_gender <- aggregate(Height ~ Gender, data = study, mean)

# ii) Compute the average height by the student
average_height_by_student <- aggregate(Height ~ Student, data = study, mean)

# iii) Generate a frequency table of the gender
gender_frequency_table <- table(study$Gender)

# iv) Compute the variance of their height by gender
variance_height_by_gender <- aggregate(Height ~ Gender, data = study, var)

# v) Compute the skewness and kurtosis of their ages (height?)
library(e1071)
skewness_height <- skewness(study$Height)
kurtosis_height <- kurtosis(study$Height)

# vi) Remove "Gender" from the data frame
study <- study[, c("Student", "Weight", "Height")]

# Print the results
print(study)
print(average_weight)
print(variance_weight)
print(average_height_by_gender)
print(average_height_by_student)
print(gender_frequency_table)
print(variance_height_by_gender)
print(skewness_height)
print(kurtosis_height)

#Please note that in operation v), I assumed you wanted to compute the skewness and kurtosis of the "Height"
#variable, not "ages" since "ages" were not mentioned in the data provided. Also, we used the `e1071` package
#to calculate the skewness and kurtosis. If you haven't installed this package, you can do so by 
#running `install.packages("e1071")` before running the above code.

#=========================================

#Sure, let's go step by step to model the relationship between the scale of the house over 
#the last 15 months:
# a) Generate a sequence from 1 to 15 and store it in 
x: 1:15
# b) Generate a random sample of size 15 between 30 and 100 and store it in y:
set.seed(123)  # for reproducibility
y <- runif(15, min = 30, max = 100)
# c) Form the equation z = x + y:
z <- x + y
# d) Run a linear regression model of z over y and x:
model <- lm(z ~ y + x)
summary(model)
# The `lm()` function fits a linear regression model to predict z using y and x as predictors. 
#The `summary()` function provides a summary of the model and its coefficients.

# 6) Generate 15 random variate from a normal distribution with mean zero and standard deviation 2:
set.seed(456)  # for reproducibility
random_variates <- rnorm(15, mean = 0, sd = 2)

#In this case, `rnorm(15, mean = 0, sd = 2)` generates 15 random numbers from a normal 
#distribution with mean 0 and standard deviation 2.

#Please note that random sampling and generated variates will differ each time you run the code due
#to the use of the `set.seed()` function to ensure reproducibility.

#================================
#To solve 10! / (3! * 7!) in R, you would use the `factorial()` function like so:

# Calculate the result
result <- factorial(10) / (factorial(3) * factorial(7))

# Print the result
print(result)
#=========================================================================================================


### SETTING THE SYSTEM working directory

setwd(dir='C:/Users/Samuel Effiong/Desktop/statistical computing 2 .. sta 418/leture 1')

#### confirm the  SYSTEM working directory
getwd()


## clear all old environmental variables
rm(list=ls())


                            # tEST qustion 

#  part A 

# let DEFINE A GENERAL VARIALE FOR THE OPTION 
clss <- 0
clss1 <- 1
clss2 <- 2
clss3 <- 3
clss4 <- 4
rat <- 0.05
rat1 <- 0.25
rat2 <- 2.3
rat5 <- 5
rat6 <- 6
rat7 <- 7
rat8 <- 100
# ====================================================

#            A.1
#        4^3 + 3(2 + 1)
solution_b <- clss4^clss3 + clss3 * (clss2 + clss1)
solution_b
#  ==========================================
# a1ii

solution_2.1 <- rat1-rat 
solution_2.1ai < (clss4 + clss3) * (clss2 + clss1)
solution_2.1a <- sqrt(solution_2.1ai)
solution_2.1ab <- solution_2.1 / solution_2.1a
solution_2.1ab
#==========================================
soln <- (clss1 + clss2)
soln1<- (clss3  + clss4)
soln2 <- (soln/soln1)^clss4
#====================================================
# a1iv
solution_2.1ai < (clss4 + clss3) * (clss2 + clss1)
solution_2.1a <- sqrt(solution_2.1ai)
solution_2.1a

#================================================================
#A2 
# ========================= A2i
x = -2:4
x
#============================A2ii
y = seq(2,4, by = 0.3)
y
#================================= A2iii
p = rep(c(0,1,0.2,04),2)
p
#============================A2iv
q = rep(c(-0.2,0.2),3)
q
#=========================A2v
k1 = x+y
k1
#=========================A2vi
k2 = x-y
k2
#============================
k3 = 3*(p+q)
k3
#=======================
k4 - (p-q)^2
k4
# ================================================ B1
# Read the data from the file "record" into a data frame
data <- read.csv("record.csv")  # If the file is in CSV format
View(data)

# (i) Determine the number of students in the file
num_students <- nrow(data)
num_students

# (ii) Write R code to compute the mean age of students
mean_age <- mean(data$age)
mean_age 

# (iii) Write R code to compute the mean age of students by gender
mean_age_by_gender <- tapply(data$age, data$gender, mean)
mean_age_by_gender

#iv.  Write R code to compute the average monthly pocket money expend on  a students by parent
mean_age_by_parent <- tapply(data$money, data$parent, mean)
mean_age_by_parent

# was not ask just know it
#  Prepare a frequency table of gender
gender_freq <- table(data$gender)
gender_freq

#  Prepare a frequency table of gender and faculty
gender_faculty_freq <- table(data$gender, data$faculty)
gender_faculty_freq
#==========================================================  B2
child <- c("bucked","unbucked")
child <- rep(child,times = 1,each = 2)
parents <- c("bucked","unbuckled")
parents <- rep(parents,times = 2)
freq <- c(56,2, 8,16)
df <- data.frame(child,parents,freq)
cont_table <- xtabs(freq ~ parents + child,data = df)
cont_table

#ii 
#margin total by seat belt
seatbelt0 <- marginSums(cont_table,"parents")
seatbelt0
#margin total by injury level
seatbealt00 <- marginSums(cont_table,"child")
seatbealt00
#or do both
Seatbelts1 <- addmargins(cont_table)
Seatbelts1

#Iv
test <- chisq.test(cont_table)
test
